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
  = PredicateStatement Predicate
  | RuleStatement Rule
  | QueryStatement Query

derive instance genericStatement :: Generic Statement _

instance showStatement :: Show Statement where
  show x = genericShow x

instance prettyStatement :: Pretty Statement where
  pretty (PredicateStatement pred) = pretty pred
  pretty (RuleStatement rule) = pretty rule
  pretty (QueryStatement query) = pretty query

-- | A __predicate__ declaration.
newtype Predicate
  = Predicate
  { label :: String
  , name :: Name
  , params :: Array { name :: Name, sort :: Sort } -- parameters
  }

derive instance genericPredicate :: Generic Predicate _

instance showPredicate :: Show Predicate where
  show x = genericShow x

instance prettyPredicate :: Pretty Predicate where
  pretty (Predicate pred) =
    "predicate" ~ pred.label ~ ":"
      ~ pred.name
      ~ ( if null pred.params then
            mempty
          else
            brackets <<< commas <<< (_ <$> pred.params)
              $ \p -> p.name ~ ":" ~ p.sort
        )

-- | An inference __rule__ declaration.
newtype Rule
  = Rule
  { label :: Label
  , params :: Array Name -- parameters (universally quantified)
  , hyps :: Array Term --hypotheses
  , cons :: Array Term -- conclusions
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
  , hyps :: Array Term --hypotheses
  , cons :: Array Term -- conclusions
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
  = TermProp Term
  | ForallProp { name :: Name, prop :: Prop }
  | ExistsProp { name :: Name, prop :: Prop }

derive instance genericProp :: Generic Prop _

instance showProp :: Show Prop where
  show x = genericShow x

instance prettyProp :: Pretty Prop where
  pretty (TermProp ex) = pretty ex
  pretty (ForallProp all) = "∀" ~ all.name ~ "." ~ all.prop
  pretty (ExistsProp exi) = "∃" ~ exi.name ~ "." ~ exi.prop

-- | A __sort__.
data Sort
  = PrimSort PrimSort

derive instance genericSort :: Generic Sort _

instance showSort :: Show Sort where
  show x = genericShow x

instance prettySort :: Pretty Sort where
  pretty (PrimSort prim) = pretty prim

data PrimSort
  = UnitPrimSort
  | BoolPrimSort

derive instance genericPrimSort :: Generic PrimSort _

instance showPrimSort :: Show PrimSort where
  show x = genericShow x

instance prettyPrimSort :: Pretty PrimSort where
  pretty UnitPrimSort = pretty "Unit"
  pretty BoolPrimSort = pretty "Bool"

-- | A __term__. _Cannot_ have quantifications.
data Term
  = NeuTerm { name :: Name, args :: Array Term }
  | PrimTerm PrimTerm

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show x = genericShow x

instance prettyTerm :: Pretty Term where
  pretty (NeuTerm neu) = neu.name ~ hcat neu.args
  pretty (PrimTerm prim) = pretty prim

data PrimTerm
  = UnitPrimTerm
  | BoolPrimTerm Boolean

derive instance genericPrimTerm :: Generic PrimTerm _

instance showPrimTerm :: Show PrimTerm where
  show x = genericShow x

instance prettyPrimTerm :: Pretty PrimTerm where
  pretty UnitPrimTerm = pretty "unit"
  pretty (BoolPrimTerm b) = pretty if b then "true" else "false"

varTerm :: Name -> Term
varTerm name = NeuTerm { name, args: [] }

neuTerm :: Name -> Array Term -> Term
neuTerm name args = NeuTerm { name, args }

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
