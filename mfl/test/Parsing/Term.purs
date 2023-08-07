module Test.Parsing.Term where

import Data.Tuple.Nested
import Language.Mfl.Surface.Ast
import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Mfl.Surface.Parsing (parseTerm, runParser)
import Partial.Unsafe (unsafeCrashWith)
import Test.Testing (Test(..), runTest)
import Text.Pretty (pretty)

test :: Effect Unit
test = do
  runTest `traverse_` crashTests
  runTest `traverse_` expectedTests
  pure unit

crashTests :: Array (Test Term)
crashTests = makeCrashTest <$>
  [ "Suc Zero"
  , "Suc Suc Zero"
  , "True"
  , "False"
  , "\"a literal string\""
  , "Zeta"
  , "{True, False,}"
  , "Sigma"
  , "[Zero, True,]"
  , "$f(True, False, Zero, Suc Zero,)"
  , "@x"
  , "[[Zero, Zero,], [Zero, Zero,],]"
  ]

expectedTests :: Array (Test Term)
expectedTests = uncurry makeExpectedTest <$>
  [
    "Suc Zero" /\
    (Term Suc [Term Zero []])
  ]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

makeCrashTest :: String -> Test Term
makeCrashTest source = CrashTest
  { mx: runParser' parseTerm source
  , failString: \err -> err
  , passString: \term' -> source <> " ~~> " <> pretty term'
  }

makeExpectedTest :: String -> Term -> Test Term
makeExpectedTest source term = PredTest
  { mx: runParser' parseTerm source
  , pred: \term' ->
      if term == term' then 
        Right $ 
          source <> " ~~> " <> pretty term'
      else
        Left $
          "Expected " <>
          "\"" <> source <> "\"" <> " " <>
          "to parse to:" <> "\n" <>
          "  " <> pretty term <> "\n" <>
          "but instead, it parsed to:" <> "\n" <>
          "  " <> pretty term'
  }

runParser' p source = runParser p source >>= case _ of
  Left err -> unsafeCrashWith err
  Right a -> pure a