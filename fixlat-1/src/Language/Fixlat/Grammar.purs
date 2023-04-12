-- | Syntax types for language.
-- |
-- | Temporary limitations:
-- | - no user-defined data; everything is primitive
-- | - only universal quantification
module Language.Fixlat.Grammar where

import Container
import Control.Alternative
import Control.Monad.Maybe.Trans
import Control.Monad.State
import Control.Plus
import Data.Enum
import Data.Generic.Rep
import Data.Lattice
import Data.Maybe
import Data.Newtype
import Data.Show.Generic
import Data.Traversable
import Data.Tuple
import Data.Tuple.Nested
import Prelude hiding (join)
import Pretty
import Utility
import Bug (throwBug)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Map as Map
import Data.Ord.Generic (genericCompare)
import Partial.Unsafe (unsafeCrashWith)

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

-- | A `Statement` is a top-level statement in a module.
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

-- | A `Predicate` is a top-level predicate declaration.
newtype Predicate
  = Predicate
  { label :: String
  , name :: Name
  , param :: { name :: Name, sort :: Sort }
  }

derive instance genericPredicate :: Generic Predicate _

instance showPredicate :: Show Predicate where
  show x = genericShow x

instance prettyPredicate :: Pretty Predicate where
  pretty (Predicate pred) =
    "predicate" ~ pred.label ~ ":"
      ~ pred.name
      ~ braces (pred.param.name ~ "," ~ pred.param.sort)

-- | A `Rule` is a top-level inference rule declaration.
newtype Rule ann
  = Rule
  { label :: Label
  , params :: Array Name -- parameters (universally quantified)
  , hyps :: Array (Term ann) -- hypotheses
  , con :: Term ann -- conclusion
  }

derive instance genericRule :: Generic (Rule ann) _

instance showRule :: Show ann => Show (Rule ann) where
  show x = genericShow x

instance prettyRule :: Pretty (Rule ann) where
  pretty (Rule rule) =
    vcat
      [ "rule" ~ rule.label ~ ":"
          ~ (if Array.null rule.params then mempty else "∀" ~ rule.params ~ ".")
      , indent <<< vcat
          $ [ vcat $ rule.hyps
            , pretty "----------------"
            , pretty rule.con
            ]
      ]

instance partialOrdRuleSort :: PartialOrd (Rule Sort) where
  comparePartial (Rule rule1) (Rule rule2) = unsafeCrashWith "partialOrdRuleSort.comparePartial"

-- | A `Query` is a top-level query.
newtype Query ann
  = Query
  { params :: Array Name -- parameters (universally quantified)
  , hyps :: Array (Term ann) -- hypotheses
  , con :: Term ann -- conclusion
  }

derive instance genericQuery :: Generic (Query ann) _

instance showQuery :: Show ann => Show (Query ann) where
  show x = genericShow x

instance prettyQuery :: Pretty (Query ann) where
  pretty (Query query) =
    vcat
      [ "query"
          ~ ":"
          ~ (if Array.null query.params then mempty else "∀" ~ query.params ~ ".")
      , indent <<< vcat
          $ [ vcat $ query.hyps
            , pretty "----------------"
            , pretty query.con
            ]
      ]

-- | A `Sort`. Every sort has a lattice structure defined over its terms.
data Sort
  = UnitSort
  | BoolSort
  | TupleSort { mb_ord :: Maybe TupleOrdering, components :: Array Sort }

derive instance genericSort :: Generic Sort _

instance eqSort :: Eq Sort where
  eq x = genericEq x

instance showSort :: Show Sort where
  show x = genericShow x

instance prettySort :: Pretty Sort where
  pretty UnitSort = pretty "Unit"
  pretty BoolSort = pretty "Bool"
  pretty (TupleSort tup) = maybe (pretty "<<Nothing>>") pretty tup.mb_ord ~ angles (commas tup.components)

-- | A `TupleOrdering` corresponds to a way to derive a lattice ordering over a
-- | tuple from the lattice orderings over its components.
data TupleOrdering
  = LexicographicTupleOrdering

derive instance genericTupleOrdering :: Generic TupleOrdering _

instance eqTupleOrdering :: Eq TupleOrdering where
  eq x = genericEq x

instance showTupleOrdering :: Show TupleOrdering where
  show x = genericShow x

instance prettyTupleOrdering :: Pretty TupleOrdering where
  pretty LexicographicTupleOrdering = pretty "Lexicographic"

{- !TODO intro this feature later

-- | A __qualified proposition__.
data QualProp ann
  = QualProp
    { params :: Array { name :: Name, sort :: Sort }
    , prop :: Prop ann
    }

derive instance genericQualProp :: Generic (QualProp ann) _

instance showQualProp :: Show ann => Show (QualProp ann) where
  show x = genericShow x

instance prettyQualProp :: Pretty (QualProp ann) where
  pretty (QualProp qp)
    | Array.null qp.params = pretty qp.prop
    | otherwise =
      "∀" ~ hcat (qp.params <#> \param -> param.name ~ ":" ~ param.sort)
        ~ "."
        ~ qp.prop

instance partialOrdQualPropSort :: PartialOrd (QualProp Sort) where
  comparePartial (QualProp qp1) (QualProp qp2) = comparePartial qp1.prop qp2.prop

instance latticeQualPropSort :: Lattice (QualProp Sort) where
  join qp1 qp2 = unwrap <$> join (PartialOrdLattice qp1) (PartialOrdLattice qp2)
  meet qp1 qp2 = unwrap <$> meet (PartialOrdLattice qp1) (PartialOrdLattice qp2)
-}
-- | A `Prop` corresponds to an unqualified __proposition__.
data Prop ann
  = Prop { name :: Name, arg :: Term ann }

derive instance genericProp :: Generic (Prop ann) _

instance showProp :: Show ann => Show (Prop ann) where
  show x = genericShow x

instance prettyProp :: Pretty (Prop ann) where
  pretty (Prop prop) = prop.name ~ braces prop.arg

instance partialOrdPropSort :: PartialOrd (Prop Sort) where
  comparePartial (Prop prop1) (Prop prop2) = do
    guard $ prop1.name == prop2.name
    comparePartial prop1.arg prop2.arg

instance latticePropSort :: Lattice (Prop Sort) where
  join prop1 prop2 = unwrap <$> join (PartialOrdLattice prop1) (PartialOrdLattice prop2)
  meet prop1 prop2 = unwrap <$> meet (PartialOrdLattice prop1) (PartialOrdLattice prop2)

-- | A `Term` corresponds to an instance of data. A `Prop` cannot appear inside
-- | a `Term`, but a `Term` can appear inside a `Prop`.
data Term ann
  = VarTerm { name :: Name, ann :: ann }
  | UnitTerm { unit :: Unit, ann :: ann }
  | BoolTerm { bool :: Boolean, ann :: ann }
  | TupleTerm { components :: Array (Term ann), ann :: ann }

derive instance genericTerm :: Generic (Term ann) _

instance containerTerm :: Container Term where
  open (VarTerm x) = x.ann
  open (UnitTerm x) = x.ann
  open (BoolTerm x) = x.ann
  open (TupleTerm x) = x.ann
  mapContainer f (VarTerm x) = VarTerm x { ann = f x.ann }
  mapContainer f (UnitTerm x) = UnitTerm x { ann = f x.ann }
  mapContainer f (BoolTerm x) = BoolTerm x { ann = f x.ann }
  mapContainer f (TupleTerm x) = TupleTerm x { ann = f x.ann }

instance showTerm :: Show ann => Show (Term ann) where
  show x = genericShow x

instance prettyTerm :: Pretty (Term ann) where
  pretty (VarTerm v) = pretty v.name
  pretty (UnitTerm u) = pretty u.unit
  pretty (BoolTerm b) = pretty b.bool
  pretty (TupleTerm t) = angles (commas t.components)

-- | A `Name` of a predicate, universally-quantified term, etc.
newtype Name
  = Name String

derive instance genericName :: Generic Name _

derive instance newtypeName :: Newtype Name _

instance eqName :: Eq Name where
  eq x = genericEq x

instance ordName :: Ord Name where
  compare x = genericCompare x

instance showName :: Show Name where
  show x = genericShow x

instance prettyName :: Pretty Name where
  pretty (Name str) = pretty str

-- | A `Label` of a module, rule, predicate, etc.
newtype Label
  = Label String

derive instance genericLabel :: Generic Label _

derive instance newtypeLabel :: Newtype Label _

instance showLabel :: Show Label where
  show x = genericShow x

instance prettyLabel :: Pretty Label where
  pretty (Label str) = pretty str

-- | Partial ordering over (open) `Term Sort`. If `a <= b`
-- | 
-- | - Attempt to unify terms. Ununifiable terms are incomparable.
-- | - Nested comparison of terms, where
-- |   - inequal free variables are incomparable
-- |   - tuples have special `TupleOrdering`
instance partialOrdTermSort :: PartialOrd (Term Sort) where
  comparePartial term1 term2 = do
    sigma <- unifyTerms term1 term2
    let
      term1' = substTerm sigma term1

      term2' = substTerm sigma term2

      sr = open term1'
    comparePartial' sr term1' term2'
    where
    comparePartial' _sr (VarTerm v1) (VarTerm v2)
      | v1.name == v2.name = pure EQ

    comparePartial' UnitSort (UnitTerm u1) (UnitTerm u2) = pure $ compare u1.unit u2.unit

    comparePartial' BoolSort (BoolTerm b1) (BoolTerm b2) = pure $ compare b1.bool b2.bool

    comparePartial' (TupleSort tup) (TupleTerm tup1) (TupleTerm tup2) = case tup.mb_ord of
      Just LexicographicTupleOrdering -> do
        Array.foldM
          ( case _ of
              EQ -> \(sr /\ (tm1 /\ tm2)) -> comparePartial' sr tm1 tm2
              o -> const (pure o)
          )
          EQ
          (tup.components `Array.zip` (tup1.components `Array.zip` tup2.components))
      Nothing -> throwBug "partialOrdTermSort.comparePartial" $ "uninstantiated tuple ordering sort:" <> "\n  sr = " <> show (TupleSort tup) <> "\n  tup1 = " <> show (TupleTerm tup1) <> "\n  tup2 = " <> show (TupleTerm tup2)

    comparePartial' _sr _tm1 _tm2' = empty

    substTerm :: forall a. Map.Map Name (Term a) -> Term a -> Term a
    substTerm sigma (VarTerm v) = fromMaybe (VarTerm v) (Map.lookup v.name sigma)

    substTerm sigma (TupleTerm tup) = TupleTerm tup { components = tup.components <#> substTerm sigma }

    substTerm _sigma tm = tm

    unifySorts :: Sort -> Sort -> Boolean
    unifySorts sr1 sr2 = sr1 == sr2

    unifyTerms :: Term Sort -> Term Sort -> Maybe (Map.Map Name (Term Sort))
    unifyTerms term1' term2' = execStateT (unifyTerms' term1' term2') Map.empty
      where
      unifyTerms' tm1 tm2
        | unifySorts (open tm1) (open tm2) = unifyTerms'' tm1 tm2
        | otherwise = throwBug "unifyTerms" $ "attempted to unify terms with different sorts:" <> "\n  tm1 = " <> show tm1 <> "\n  tm2 = " <> show tm2

      unifyTerms'' (VarTerm v1) tm2 = subst v1.name tm2

      unifyTerms'' tm1 (VarTerm v2) = subst v2.name tm1

      unifyTerms'' (UnitTerm u1) (UnitTerm u2)
        | u1.unit == u2.unit = pure unit

      unifyTerms'' (BoolTerm b1) (BoolTerm b2)
        | b1.bool == b2.bool = pure unit

      unifyTerms'' (TupleTerm tup1) (TupleTerm tup2) = for_ (tup1.components `Array.zip` tup2.components) (uncurry unifyTerms')

      unifyTerms'' _ _ = mempty

      subst x tm = modify_ $ Map.insert x tm

      occurs _x (UnitTerm _) = false

      occurs _x (BoolTerm _) = false

      occurs x (VarTerm v1) = v1.name == x

      occurs x (TupleTerm tup) = any (occurs x) tup.components

-- | The partial ordering over `Term Sort` gives rise to a lattice over `Term
-- | Sort` as well via `partialOrdLattice`.
instance latticeTermSort :: Lattice (Term Sort) where
  join a b = unwrap <$> join (PartialOrdLattice a) (PartialOrdLattice b)
  meet a b = unwrap <$> join (PartialOrdLattice a) (PartialOrdLattice b)
