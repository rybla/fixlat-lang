module Language.Mfl.Surface.Parsing where

import Language.Mfl.Surface.Ast
import Prelude
import Prim hiding (Type)

import Control.Monad.Computation (ComputationT, evalComputationT, gets, modify, runComputationT, throwError, tryComputation)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar, toCodePointArray)
import Data.String as String
import Data.Top (Top(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Hole (hole)
import Partial.Unsafe (unsafeCrashWith)

type Parser = ComputationT "Parser" ParserCtx ParserEnv ParserErr Effect

type ParserCtx = 
  { }

type ParserEnv = 
  { source :: String }

type ParserErr = String

throwExpected :: forall a. String -> Parser a
throwExpected msg = do
  source <- gets _.source
  throwError $ "Expected '" <> msg <> "' at:\n\n" <> source

runParser :: forall a. Parser a -> String -> Effect (String \/ a)
runParser p source = evalComputationT
  {}
  {source}
  p

--------------------------------------------------------------------------------
-- Module, Declaration
--------------------------------------------------------------------------------

parseModule :: Parser Module
parseModule = do
  ds <- Array.fromFoldable <$> parseMany do
    d <- parseDeclaration
    parseNewlines_
    pure d
  pure $ Module ds

parseDeclaration :: Parser Declaration
parseDeclaration = parseAny (throwExpected "Declaration")
  [
    parseString_ "data" /\ \_ -> do
      parseWhitespace
      name <- parseName
      parseWhitespace
      parseString_ "="
      parseWhitespace
      dat <- parseDataType
      pure $ DataTypeDeclaration name dat
  ,
    parseString_ "lat" /\ \_ -> do
      parseWhitespace
      name <- parseName 
      parseWhitespace 
      parseString_ "="
      parseWhitespace
      lat <- parseLatType
      pure $ LatTypeDeclaration name lat
  ,
    parseString_ "rel" /\ \_ -> do
      parseWhitespace
      name <- parseName
      parseWhitespace 
      parseString_ ":"
      parseWhitespace
      lat <- parseLatType
      pure $ RelationDeclaration name lat
  ,
    parseString_ "rule" /\ \_ -> do
      parseWhitespace
      name <- parseName
      parseString_ "\n"
      -- rule <- Array.fromFoldable <$> 
      --   parseIndentedBlock
      --     { maybe_parseSeparator: Nothing }
      --     parseRuleItem
      rule <- parseRule
      pure $ RuleDeclaration name rule
  ,
    parseString_ "axiom" /\ \_ -> do
      parseWhitespace
      name <- parseName
      parseWhitespace 
      parseString_ "="
      parseWhitespace
      prop <- parseProp
      pure $ AxiomDeclaration name prop
  ,
    parseString_ "fun" /\ \_ -> do
      parseWhitespace
      name <- parseName
      params <- parseParams (parseString_ "(") (parseString_ ")") parseDataType
      pure $ FunctionDeclaration {name, params}
  ,
    parseString_ "fix" /\ \_ -> do
      parseWhitespace
      name <- parseName
      parseString_ "["
      ruleOrAxiomNames <- Array.fromFoldable <$> parseMany (commaItem parseName)
      parseString_ "]"
      pure $ FixpointDeclaration {name, ruleOrAxiomNames}
  ,
    parseString_ "query" /\ \_ -> do
      parseWhitespace
      name <- parseName
      params <- parseParams (parseString_ "(") (parseString_ ")") parseLatType
      queryProp <- parseProp
      pure $ QueryDeclaration {name, params, queryProp}
  ,
    parseString_ "insert" /\ \_ -> do
      parseWhitespace
      name <- parseName
      params <- parseParams (parseString_ "(") (parseString_ ")") parseLatType
      insertionProp <- parseProp
      pure $ InsertionDeclaration {name, params, insertionProp}
  ]

parseParams :: forall a. Parser Unit -> Parser Unit -> Parser a -> Parser (Array (Name /\ a))
parseParams parseLeft parseRight parseSig = do
  parseLeft
  params <- Array.fromFoldable <$> parseMany 
    (commaItem do
      paramName <- parseName
      parseWhitespace
      parseString_ ":"
      parseWhitespace
      paramType <- parseSig
      parseWhitespace
      pure (paramName /\ paramType))
  parseWhitespace
  parseRight
  pure params

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

parseRule :: Parser Rule
parseRule = Array.fromFoldable <$> parseMany parseRuleItem

parseRuleItem :: Parser RuleItem
parseRuleItem = parseAny (throwExpected "RuleItem")
  [
    parseString_ "if" /\ \_ -> do
      parseWhitespace
      t <- parseTerm
      pure $ FilterRuleItem t
  ,
    parseString_ "forall" /\ \_ -> do
      parseWhitespace
      x <- parseName
      parseWhitespace
      parseString_ ":"
      parseWhitespace
      lat <- parseLatType
      pure $ QuantRuleItem x lat
  ,
    parseString_ "let" /\ \_ -> do
      parseWhitespace
      x <- parseName
      parseWhitespace
      parseString_ "="
      parseWhitespace
      t <- parseTerm
      pure $ LetRuleItem x t
  ,
    pure unit /\ \_ -> PropRuleItem <$> parseProp
  ]

--------------------------------------------------------------------------------
-- Type, DataType, LatType
--------------------------------------------------------------------------------

parseDataType :: Parser DataType
parseDataType = parseType (\_ -> parseDataType) []

parseLatType :: Parser LatType
parseLatType = parseType (\_ -> parseLatType)
  [
    parseString_ "Pow" /\ \_ -> do
      parseWhitespace
      dat <- parseDataType
      pure $ PowerSetType Top dat
  ,
    parseString_ "Op" /\ \_ -> do
      parseWhitespace
      lat <- parseLatType
      pure $ OpType Top lat
  ]

parseType :: forall isLat. 
  (Unit -> Parser (Type isLat)) ->
  Array (Parser Unit /\ (Unit -> Parser (Type isLat))) ->
  Parser (Type isLat)
parseType lazy_parseKid extraVariants = parseAny (throwExpected "Type") $
  [
    parseString_ "(" /\ \_ -> do
      lat <- lazy_parseKid unit
      parseString_ ")"
      pure lat
  , 
    parseString_ "Bool" /\ \_ -> pure BoolType
  ,
    parseString_ "Nat" /\ \_ -> pure NatType
  ,
    parseString_ "String" /\ \_ -> pure StringType
  ,
    parseString_ "Set" /\ \_ -> do
      parseWhitespace
      lat <- lazy_parseKid unit
      pure $ SetType lat
  ,
    parseString_ "Tup" /\ \_ -> do
      let parseKid = lazy_parseKid unit
      lats <- Array.fromFoldable <$> parseMany do
        parseWhitespace
        parseKid
      pure $ TupleType lats
  ] 
  <> extraVariants

--------------------------------------------------------------------------------
-- Prop
--------------------------------------------------------------------------------

parseProp :: Parser Prop
parseProp = do
  p <- parseName
  parseWhitespace 
  t <- parseTerm
  pure $ Prop p t

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

parseTerm :: Parser Term
parseTerm = parseAny (throwExpected "Term")
  [
  -- NatConstr
    parseString_ "Zero" /\ \_ -> pure $ ConstrTerm $ NatConstr ZeroConstr
  ,
    parseString_ "Suc" /\ \_ -> do
      parseWhitespace
      t <- parseTerm
      pure $ ConstrTerm $ NatConstr $ SucConstr t
  ,
  -- BoolConstr
    parseString_ "True" /\ \_ -> pure $ ConstrTerm $ BoolConstr true
  ,
    parseString_ "False" /\ \_ -> pure $ ConstrTerm $ BoolConstr false
  ,
  -- StringConstr
    parseString_ "\"" /\ \_ -> do
      s <- parseUntil \c -> c == codePointFromChar '"'
      parseString_ "\""
      pure $ ConstrTerm $ StringConstr $ LiteralStringConstr s
  ,
    parseString_ "Zeta" /\ \_ -> pure $ ConstrTerm $ StringConstr $ ZetaConstr
  ,
  -- SetConstr
    parseString_ "{" /\ \_ -> do
      ts <- Array.fromFoldable <$> parseMany (commaItem parseTerm)
      pure $ ConstrTerm $ SetConstr $ LiteralSetConstr ts
  ,
    parseString_ "Sigma" /\ \_ -> pure $ ConstrTerm $ SetConstr $ SigmaConstr
  ,
  -- TupleConstr
    parseString_ "Tup" /\ \_ -> do
      ts <- Array.fromFoldable <$> parseMany do
        parseWhitespace
        parseTerm
      pure $ ConstrTerm $ TupleConstr ts
  -- NeuTerm
  ,
    parseString_ "$" /\ \_ -> do
      f <- parseName
      parseString_ "("
      ts <- Array.fromFoldable <$> parseMany (commaItem parseTerm)
      parseWhitespace
      parseString_ ")"
      pure $ NeuTerm f ts
  -- VarTerm
  ,
    parseString_ "@" /\ \_ -> VarTerm <$> parseName
  ]

--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

-- TODO: more specific rules for what can be included in names
parseName :: Parser Name
parseName = Name <$> parseWhile \c -> not $ c `Array.elem` invalidNameCodePoints

invalidNameCodePoints ∷ Array CodePoint
invalidNameCodePoints = toCodePointArray "\n ()[],:\"'"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

checkSource :: (String -> Boolean) -> Parser Boolean
checkSource cond = cond <$> gets _.source

getNextCodePoint :: Parser (Maybe CodePoint)
getNextCodePoint = do
  source <- gets _.source
  case String.uncons source of
    Nothing -> pure Nothing
    Just {head: c} -> pure $ Just c

checkNextCodePoint :: (CodePoint -> Boolean) -> Parser (Maybe CodePoint)
checkNextCodePoint cond = getNextCodePoint >>= case _ of
  Nothing -> pure Nothing
  Just cp -> if cond cp then pure (Just cp) else pure Nothing

parseWhile :: (CodePoint -> Boolean) -> Parser String
parseWhile cond = do
  source <- gets _.source
  let str = String.takeWhile cond source
  let source' = String.drop (String.length str) source
  void $ modify _ {source = source'}
  pure str

parseUntil :: (CodePoint -> Boolean) -> Parser String
parseUntil cond = do
  source <- gets _.source
  let str = String.takeWhile (not <<< cond) source
  let source' = String.drop (String.length str) source
  void $ modify _ {source = source'}
  pure str

parseCodePoint :: CodePoint -> Parser CodePoint
parseCodePoint cp = getNextCodePoint >>= case _ of
  Nothing -> throwExpected $ String.fromCodePointArray [cp]
  Just cp' -> if cp == cp' then pure cp else throwExpected $ String.fromCodePointArray [cp]

parseString :: String -> Parser String
parseString str = do
  source <- gets _.source
  case String.stripPrefix (String.Pattern str) source of
    Nothing -> throwExpected str
    Just source' -> do
      void $ modify _ {source = source'}
      pure str

parseString_ :: String -> Parser Unit
parseString_ = void <<< parseString

parseAny :: forall a b. Parser b -> Array (Parser a /\ (a -> Parser b)) -> Parser b
parseAny p0 ps0 = go (List.fromFoldable ps0)
  where
  go Nil = p0
  go (Cons (p /\ kp) ps) = tryParser p >>= case _ of
    Left _err -> go ps
    Right a -> kp a

-- | Parses a non-zero number of spaces, and returns the count.
parseSpaces :: Parser Int
parseSpaces = do
  str <- parseWhile (_ == codePointFromChar ' ')
  pure (String.length str)

parseSpaces_ :: Parser Unit
parseSpaces_ = void parseSpaces

parseWhitespace :: Parser Unit
parseWhitespace = void $ parseWhile (_ `Array.elem` (codePointFromChar <$> ['\n', ' ']))

-- | Parses a non-zero number of newlines, and returns the count.
parseNewlines :: Parser Int
parseNewlines = do
  str <- parseWhile (_ == codePointFromChar '\n')
  pure (String.length str)

parseNewlines_ :: Parser Unit
parseNewlines_ = void parseNewlines

tryParser :: forall a. Parser a -> Parser (ParserErr \/ a)
tryParser = tryComputation

parseMany :: forall a. Parser a -> Parser (List a)
parseMany p = tryParser p >>= case _ of
  Left _err -> pure Nil
  Right a -> do
    as <- parseMany p
    pure (Cons a as)

commaItem :: forall a. Parser a -> Parser a
commaItem p = do
  parseWhitespace
  a <- p
  parseString_ ","
  pure a

-- -- | Parses items of an indented block, with increased indentation level, 
-- -- starting with an indented newline. Expects each item to be separated by a
-- -- newline (prioritized) or an optional separator.
-- parseIndentedBlock :: forall a. 
--   { maybe_parseSeparator :: Maybe (Parser Unit)
--   } ->
--   Parser a ->
--   Parser (List a)
-- parseIndentedBlock = hole "TODO"

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

fromJust :: forall a. Maybe a -> a
fromJust Nothing = unsafeCrashWith "fromJust Nothing"
fromJust (Just a) = a
