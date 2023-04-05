-- | Syntax types for language.
module Language.Fixlat.Grammar where

import Data.Array
import Data.Generic.Rep
import Data.Maybe
import Data.Newtype
import Data.Show.Generic
import Prelude
import Pretty
import Utility
import Data.Enum (enumFromTo)

newtype Module
  = Module
  { label :: Label
  , statements :: Array Statement
  }

derive instance genericModule :: Generic Module _

instance showModule :: Show Module where
  show x = genericShow x

instance prettyModule :: Pretty Module where
  pretty (Module mdl) =
    vcat
      [ "module" ~ mdl.label ~ ":"
      , vcat $ ("" \~ _) <<< pretty <$> mdl.statements
      ]

-- | A top-level __statement__.
data Statement
  = SyntaxStatement Syntax
  | RuleStatement Rule
  | QueryStatement Query

derive instance genericStatement :: Generic Statement _

instance showStatement :: Show Statement where
  show x = genericShow x

instance prettyStatement :: Pretty Statement where
  pretty (SyntaxStatement syn) = pretty syn
  pretty (RuleStatement rule) = pretty rule
  pretty (QueryStatement query) = pretty query

-- | A __syntax__ declaration. 
-- | - `Syntax.show` encodes how to show an instance of the syntax
newtype Syntax
  = Syntax
  { label :: Label
  , name :: Name
  , argsCount :: Int
  -- , showPartial :: Partial => Array String -> String
  }

derive instance genericSyntax :: Generic Syntax _

instance showSyntax :: Show Syntax where
  show x = genericShow x

instance prettySyntax :: Pretty Syntax where
  pretty (Syntax syn) =
    "syntax" ~ syn.label ~ ":"
      ~ syn.name
      ~ (if syn.argsCount == 0 then mempty else hcat (const "_" <$> enumFromTo 0 syn.argsCount))

-- | An inference __rule__ declaration.
newtype Rule
  = Rule
  { label :: Label
  , params :: Array Name -- parameters (universally quantified)
  , hyps :: Array Expr --hypotheses
  , cons :: Array Expr -- conclusions
  }

derive instance genericRule :: Generic Rule _

instance showRule :: Show Rule where
  show x = genericShow x

instance prettyRule :: Pretty Rule where
  pretty (Rule rule) =
    vcat
      [ "rule" ~ rule.label ~ ":"
          ~ (if null rule.params then mempty else "∀" ~ rule.params ~ ".")
      , indent <<< vcat
          $ [ vcat $ rule.hyps
            , pretty "----------------"
            , vcat $ rule.cons
            ]
      ]

-- | A __query__.
newtype Query
  = Query
  { params :: Array Name -- parameters (universally quantified)
  , hyps :: Array Expr --hypotheses
  , cons :: Array Expr -- conclusions
  }

derive instance genericQuery :: Generic Query _

instance showQuery :: Show Query where
  show x = genericShow x

instance prettyQuery :: Pretty Query where
  pretty (Query query) =
    vcat
      [ "query"
          ~ ":"
          ~ (if null query.params then mempty else "∀" ~ query.params ~ ".")
      , indent <<< vcat
          $ [ vcat $ query.hyps
            , pretty "----------------"
            , vcat $ query.cons
            ]
      ]

-- | A __proposition__. _Can_ have quantifications.
data Prop
  = ExprProp Expr
  | ForallProp { name :: Name, prop :: Prop }
  | ExistsProp { name :: Name, prop :: Prop }

derive instance genericProp :: Generic Prop _

instance showProp :: Show Prop where
  show x = genericShow x

instance prettyProp :: Pretty Prop where
  pretty (ExprProp ex) = pretty ex
  pretty (ForallProp all) = "∀" ~ all.name ~ "." ~ all.prop
  pretty (ExistsProp exi) = "∃" ~ exi.name ~ "." ~ exi.prop

-- | An __expression__. _Cannot_ have quantifications.
data Expr
  = ExprNeu { name :: Name, args :: Array Expr }

varExpr :: Name -> Expr
varExpr name = ExprNeu { name, args: [] }

neuExpr :: Name -> Array Expr -> Expr
neuExpr name args = ExprNeu { name, args }

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

instance prettyExpr :: Pretty Expr where
  pretty (ExprNeu neu) = neu.name ~ hcat neu.args

-- | A __name__.
newtype Name
  = Name String

derive instance genericName :: Generic Name _

derive instance newtypeName :: Newtype Name _

instance showName :: Show Name where
  show x = genericShow x

instance prettyName :: Pretty Name where
  pretty (Name str) = pretty str

-- | A __label__.
newtype Label
  = Label String

derive instance genericLabel :: Generic Label _

derive instance newtypeLabel :: Newtype Label _

instance showLabel :: Show Label where
  show x = genericShow x

instance prettyLabel :: Pretty Label where
  pretty (Label str) = pretty str
