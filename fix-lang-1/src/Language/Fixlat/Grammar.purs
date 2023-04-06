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

newtype Module ann
  = Module
  { label :: Label
  , statements :: Array (Statement ann)
  }

derive instance genericModule :: Generic (Module ann) _

instance showModule :: Show ann => Show (Module ann) where
  show x = genericShow x

instance prettyModule :: Pretty (Module ann) where
  pretty (Module mdl) =
    vcat
      [ "module" ~ mdl.label ~ ":"
      , vcat $ ("" \~ _) <<< pretty <$> mdl.statements
      ]

-- | A top-level __statement__.
data Statement ann
  = PredicateStatement Predicate
  | RuleStatement (Rule ann)
  | QueryStatement (Query ann)

derive instance genericStatement :: Generic (Statement ann) _

instance showStatement :: Show ann => Show (Statement ann) where
  show x = genericShow x

instance prettyStatement :: Pretty (Statement ann) where
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
newtype Rule ann
  = Rule
  { label :: Label
  , params :: Array Name -- parameters (universally quantified)
  , hyps :: Array (Term ann) --hypotheses
  , con :: Term ann -- conclusion
  }

derive instance genericRule :: Generic (Rule ann) _

instance showRule :: Show ann => Show (Rule ann) where
  show x = genericShow x

instance prettyRule :: Pretty (Rule ann) where
  pretty (Rule rule) =
    vcat
      [ "rule" ~ rule.label ~ ":"
          ~ (if null rule.params then mempty else "∀" ~ rule.params ~ ".")
      , indent <<< vcat
          $ [ vcat $ rule.hyps
            , pretty "----------------"
            , pretty rule.con
            ]
      ]

-- | A __query__.
newtype Query ann
  = Query
  { params :: Array Name -- parameters (universally quantified)
  , hyps :: Array (Term ann) --hypotheses
  , cons :: Array (Term ann) -- conclusions
  }

derive instance genericQuery :: Generic (Query ann) _

instance showQuery :: Show ann => Show (Query ann) where
  show x = genericShow x

instance prettyQuery :: Pretty (Query ann) where
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
data Prop ann
  = TermProp (Term ann)
  | ForallProp { name :: Name, prop :: Prop ann }
  | ExistsProp { name :: Name, prop :: Prop ann }

derive instance genericProp :: Generic (Prop ann) _

instance showProp :: Show ann => Show (Prop ann) where
  show x = genericShow x

instance prettyProp :: Pretty (Prop ann) where
  pretty (TermProp ex) = pretty ex
  pretty (ForallProp all) = "∀" ~ all.name ~ "." ~ all.prop
  pretty (ExistsProp exi) = "∃" ~ exi.name ~ "." ~ exi.prop

-- | A __sort__. Every sort has a lattice structure defined over it.
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
data Term ann
  = NeuTerm { name :: Name, args :: Array (Term ann), ann :: ann }
  | PrimTerm (PrimTerm ann)

derive instance genericTerm :: Generic (Term ann) _

instance showTerm :: Show ann => Show (Term ann) where
  show x = genericShow x

instance prettyTerm :: Pretty (Term ann) where
  pretty (NeuTerm neu) = neu.name ~ hcat neu.args
  pretty (PrimTerm prim) = pretty prim

data PrimTerm ann
  = UnitPrimTerm ann
  | BoolPrimTerm Boolean ann

derive instance genericPrimTerm :: Generic (PrimTerm ann) _

instance showPrimTerm :: Show ann => Show (PrimTerm ann) where
  show x = genericShow x

instance prettyPrimTerm :: Pretty (PrimTerm ann) where
  pretty (UnitPrimTerm _) = pretty "unit"
  pretty (BoolPrimTerm b _) = pretty if b then "true" else "false"

varTerm :: forall ann. Name -> ann -> Term ann
varTerm name ann = NeuTerm { name, args: [], ann }

neuTerm :: forall ann. Name -> Array (Term ann) -> ann -> Term ann
neuTerm name args ann = NeuTerm { name, args, ann }

class IsTerm a ann where
  toTerm :: a -> Term ann

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
