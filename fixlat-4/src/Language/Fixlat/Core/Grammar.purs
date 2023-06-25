module Language.Fixlat.Core.Grammar where

import Data.Either.Nested
import Data.Tuple.Nested
import Data.Variant
import Prelude
import Prim hiding (Type)

import Control.Assert (Assertion, assert)
import Control.Assert.Assertions (equal, exactLength)
import Control.Assert.Refined (class Refined)
import Control.Bug (bug)
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Lattice (class PartialOrd, comparePartial)
import Data.List (List(..), (:))
import Data.List as List
import Data.Make (class Make)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Hole (hole)
import Prim as Prim
import Text.Pretty (class Pretty, indent, lines, parens, pretty, ticks, (<+>))

--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------

newtype Module = Module
  { dataTypes :: Map.Map TypeName DataType
  , latticeTypes :: Map.Map TypeName LatticeType
  , functionSpecs :: Map.Map FunctionName FunctionSpec
  , relations :: Map.Map RelationName Relation
  , rules :: Map.Map RuleName Rule
  , axioms :: Map.Map AxiomName Axiom
  , databaseSpecs :: Map.Map DatabaseSpecName DatabaseSpec
  }

derive instance Newtype Module _

instance Pretty Module where
  pretty (Module modl) = lines
    [ "module:"
    , indent $ lines
        [ "dataTypes:" <+> indent (pretty modl.dataTypes)
        , "latticeTypes:" <+> indent (pretty modl.latticeTypes)
        , "functionSpecs:" <+> indent (pretty modl.functionSpecs)
        , "relations:" <+> indent (pretty modl.relations)
        , "rules:" <+> indent (pretty modl.rules)
        , "axioms:" <+> indent (pretty modl.axioms)
        , "databaseSpecs:" <+> indent (pretty modl.databaseSpecs) ] ]

emptyModule :: Module
emptyModule = Module
  { dataTypes: Map.empty
  , latticeTypes: Map.empty
  , functionSpecs: Map.empty
  , relations: Map.empty
  , rules: Map.empty
  , axioms: Map.empty
  , databaseSpecs: Map.empty }

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A DataType encodes the structure of the Terms of a type, but not a lattice
-- | ordering over them.
data DataType
  = BoolDataType
  | IntDataType
  | NatDataType
  | TupleDataType DataType DataType

derive instance Generic DataType _
instance Show DataType where show x = genericShow x
derive instance Eq DataType 
derive instance Ord DataType

instance Pretty DataType where
  pretty = case _ of
    BoolDataType -> "bool"
    IntDataType -> "int"
    NatDataType -> "nat"
    TupleDataType ty1 ty2 -> "tuple" <+> parens (pretty ty1) <+> parens (pretty ty2)

-- | A LatticeType specifies a lattice ordering over a uniquely deTermined
-- | underlying DataType.
data LatticeType
  = BoolLatticeType
  | IntLatticeType
  | NatLatticeType
  | OpLatticeType LatticeType
  | DiscreteLatticeType DataType
  | TupleLatticeType TupleOrdering LatticeType LatticeType

derive instance Generic LatticeType _
instance Show LatticeType where show x = genericShow x
derive instance Eq LatticeType 
derive instance Ord LatticeType

instance Pretty LatticeType where
  pretty = case _ of
    BoolLatticeType -> "bool"
    IntLatticeType -> "int"
    NatLatticeType -> "nat"
    OpLatticeType lty -> "op" <+> parens (pretty lty)
    DiscreteLatticeType ty -> "discrete" <+> parens (pretty ty)
    TupleLatticeType LexicographicTupleOrdering lty1 lty2 -> "tuple" <+> parens (pretty lty1) <+> parens (pretty lty2)

data TupleOrdering
  = LexicographicTupleOrdering

derive instance Generic TupleOrdering _
instance Show TupleOrdering where show x = genericShow x
derive instance Eq TupleOrdering
derive instance Ord TupleOrdering

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

-- A Term, annotated with type information.
type SymbolicTerm = LatticeTerm TermName
type ConcreteTerm = LatticeTerm Void
type LatticeTerm = Term LatticeType
data Term ty x
  = NeutralTerm FunctionName (Array (Term ty x)) ty
  | PrimitiveTerm Primitive (Array (Term ty x)) ty
  | NamedTerm x ty

derive instance Generic (Term ty x) _
instance (Show x, Show ty) => Show (Term ty x) where show x = genericShow x
derive instance (Eq x, Eq ty) => Eq (Term ty x)
derive instance Bifunctor Term

instance Pretty (Term ty TermName) where
  pretty = case _ of
    NeutralTerm fun tm _ -> pretty fun <> parens (pretty tm)
    PrimitiveTerm prim [] _ -> pretty prim
    PrimitiveTerm prim tms _ -> pretty prim <> parens (pretty tms)
    NamedTerm x _ -> pretty x

instance Pretty (Term ty Void) where pretty = pretty <<< toSymbolicTerm

-- TODO: should this also be over SymbolicTerm?
instance PartialOrd ConcreteTerm where

  comparePartial term@(NeutralTerm _ _ _) _ = bug $ "In order to compare concrete terms, they must be fully simplified. However, you attempted to compare a neutral term " <> ticks (show term) <> "."
  comparePartial _ term@(NeutralTerm _ _ _) = bug $ "In order to compare concrete terms, they must be fully simplified. However, you attempted to compare a neutral term " <> ticks (show term) <> "."

  comparePartial (NamedTerm x _) _ = absurd x
  comparePartial _ (NamedTerm x _) = absurd x

  comparePartial (PrimitiveTerm p1 args1 _lty1) (PrimitiveTerm p2 args2 _lty2) =
    assert equal (_lty1 /\ _lty2) \_ -> do
      let lty = _lty1
      case lty of
        OpLatticeType lty' -> comparePartial (PrimitiveTerm p2 args2 lty') (PrimitiveTerm p1 args1 lty')
        DiscreteLatticeType _ -> if (p1 /\ args1) == (p2 /\ args2) then Just EQ else Nothing
        TupleLatticeType LexicographicTupleOrdering _ _ -> case (p1 /\ args1) /\ (p2 /\ args2) of
          (TuplePrimitive /\ [x1, y1]) /\ (TuplePrimitive /\ [x2, y2]) -> case comparePartial x1 x2 of
            Just EQ -> comparePartial y1 y2
            mc -> mc
        BoolLatticeType -> case (p1 /\ args1) /\ (p2 /\ args2) of
          (FalsePrimitive /\ []) /\ (FalsePrimitive /\ []) -> Just EQ
          (FalsePrimitive /\ []) /\ (TruePrimitive /\ []) -> Just LT
          (TruePrimitive /\ []) /\ (FalsePrimitive /\ []) -> Just GT
          (TruePrimitive /\ []) /\ (TruePrimitive /\ []) -> Just EQ
        IntLatticeType -> case (p1 /\ args1) /\ (p2 /\ args2) of
          (IntPrimitive x1 /\ []) /\ (IntPrimitive x2 /\ []) -> Just (compare x1 x2)
        NatLatticeType -> case (p1 /\ args1) /\ (p2 /\ args2) of
          (ZeroPrimitive /\ []) /\ (ZeroPrimitive /\ []) -> Just EQ
          (ZeroPrimitive /\ []) /\ (SucPrimitive /\ [_]) -> Just LT
          (SucPrimitive /\ [_]) /\ (ZeroPrimitive /\ []) -> Just GT
          (SucPrimitive /\ [x1]) /\ (SucPrimitive /\ [x2]) -> comparePartial x1 x2

typeOfTerm :: forall ty x. Term ty x -> ty
typeOfTerm (NeutralTerm _ _ ty) = ty
typeOfTerm (PrimitiveTerm _ _ ty) = ty
typeOfTerm (NamedTerm _ ty) = ty

data Primitive 
  = ZeroPrimitive
  | SucPrimitive
  | TruePrimitive
  | FalsePrimitive
  | TuplePrimitive
  | IntPrimitive Int

derive instance Generic Primitive _
instance Show Primitive where show x = genericShow x
derive instance Eq Primitive
derive instance Ord Primitive

instance Pretty Primitive where 
  pretty = case _ of
    ZeroPrimitive -> "zero"
    SucPrimitive -> "suc"
    TruePrimitive -> "true"
    FalsePrimitive -> "false"
    TuplePrimitive -> "tuple"
    IntPrimitive x -> pretty x

substituteTerm :: Map.Map TermName SymbolicTerm -> SymbolicTerm -> SymbolicTerm
substituteTerm sigma (NeutralTerm fun tms ty) = NeutralTerm fun (substituteTerm sigma <$> tms) ty
substituteTerm sigma (PrimitiveTerm prim tms ty) = PrimitiveTerm prim (substituteTerm sigma <$> tms) ty
substituteTerm sigma (NamedTerm x ty) = case Map.lookup x sigma of
  Just tm -> tm
  Nothing -> NamedTerm x ty

concreteTerm :: Assertion SymbolicTerm ConcreteTerm
concreteTerm = 
  { label: "concreteTerm"
  , check: checkConcreteTerm
  }

checkConcreteTerm :: SymbolicTerm -> Either String ConcreteTerm
checkConcreteTerm = case _ of
  NeutralTerm fun tms ty -> NeutralTerm fun <$> checkConcreteTerm `traverse` tms <*> pure ty
  PrimitiveTerm prim tms ty -> PrimitiveTerm prim <$> traverse checkConcreteTerm tms <*> pure ty
  term@(NamedTerm _ _) -> Left $ "Term " <> ticks (show term) <> " is not concrete."

toSymbolicTerm :: forall ty. Term ty Void -> Term ty TermName
toSymbolicTerm = rmap absurd

trueTerm :: forall x. Term LatticeType x
trueTerm = PrimitiveTerm TruePrimitive [] BoolLatticeType

falseTerm :: forall x. Term LatticeType x
falseTerm = PrimitiveTerm FalsePrimitive [] BoolLatticeType

--------------------------------------------------------------------------------
-- Function
--------------------------------------------------------------------------------

-- TODO: any other metadata that a function needs?
data FunctionSpec = FunctionSpec
  { functionType :: FunctionType
  , implementation :: Maybe (Array ConcreteTerm -> ConcreteTerm)
  }

instance Pretty FunctionSpec where 
  pretty (FunctionSpec funSpec) = "function:" <+> pretty funSpec.functionType

-- | A FunctionType, which can either be a data function (lattice-polymorphic)
-- | or a lattice function (lattice-specific). Each use of a Function must be
-- | monotonic.
data FunctionType = FunctionType (Array DataType) DataType

instance Pretty FunctionType where pretty (FunctionType args ret) = parens (pretty args) <+> "->" <+> pretty ret

--------------------------------------------------------------------------------
-- Relation
--------------------------------------------------------------------------------

-- | A Relation over Terms of a LatticeType. A Relation must be be covariant in
-- | the argument's lattice.
data Relation = Relation LatticeType

instance Pretty Relation where pretty (Relation lty) = "relation:" <+> pretty lty

--------------------------------------------------------------------------------
-- Proposition
--------------------------------------------------------------------------------

-- | A proposition of a particular instance of a Relation.
type SymbolicProposition = Proposition LatticeType TermName
type ConcreteProposition = Proposition LatticeType Void
data Proposition ty x = Proposition RelationName (Term ty x)

derive instance Generic (Proposition ty x) _
instance (Show x, Show ty) => Show (Proposition ty x) where show x = genericShow x
derive instance Bifunctor Proposition

instance Pretty (Proposition ty TermName) where pretty (Proposition rel tm) = pretty rel <> parens (pretty tm)
instance Pretty (Proposition ty Void) where pretty = pretty <<< toSymbolicProposition

substituteProposition :: Map.Map TermName SymbolicTerm -> SymbolicProposition -> SymbolicProposition
substituteProposition sigma (Proposition rel arg) = Proposition rel (substituteTerm sigma arg)

concreteProposition :: Assertion SymbolicProposition ConcreteProposition
concreteProposition = 
  { label: "concreteProposition"
  , check: \(Proposition rel tm) -> Proposition rel <$> checkConcreteTerm tm
  }

toSymbolicProposition :: forall ty. Proposition ty Void -> Proposition ty TermName
toSymbolicProposition = rmap absurd

instance PartialOrd ConcreteProposition where
  comparePartial (Proposition rel1 arg1) (Proposition rel2 arg2) = 
    assert equal (rel1 /\ rel2) \_ -> comparePartial arg1 arg2

--------------------------------------------------------------------------------
-- Axiom
--------------------------------------------------------------------------------

newtype Axiom = Axiom ConcreteProposition

derive newtype instance Show Axiom

instance Pretty Axiom where pretty (Axiom prop) = "axiom:" <+> pretty prop

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | An derivation Rule for deriving propositions of a Relation. A Rule is built
-- | as a nested structure where each layer is one of the following:
-- |   - Quantifications, which are introduced into scope for the nested Rule
-- |   - Hypothesis proposition
-- |   - Filter
-- |
-- | Finally, the last layer is the conclusion proposition.
-- |
-- | Requirements:
-- |   - Each existentially-quantified variable must be used _only_ in
-- |     hypotheses.
-- |   - Each universally-quantified variable must be used in hypotheses and the
-- |     conclusion.
-- |   - Each universally-quantified variable must be used in the
-- |     immediately-next hypothesis.
data Rule
  = HypothesisRule
      RuleHypothesis
      (Rule \/ SymbolicProposition)

type RuleHypothesis = 
  { quantifications :: Quantifications
  , proposition :: SymbolicProposition
  , filter :: Maybe SymbolicTerm
  }

derive instance Generic Rule _
instance Show Rule where show x = genericShow x

instance Pretty Rule where 
  pretty (HypothesisRule hyp conc) = lines
    [ "rule:"
    , indent $ lines
        [ "hypothesis:\n" <> (indent $ lines
            [ "quantifications:" <+> pretty hyp.quantifications
            , "proposition:" <+> pretty hyp.proposition
            , "filter:" <+> pretty hyp.filter
            ])
        , "conclusion:" <+> pretty conc ] ]

instance Refined "Rule" Rule where
  -- TODO: encode requirements
  -- validate' = hole "validate Rule"
  validate' = \_ -> Nothing

substituteRule :: Map.Map TermName SymbolicTerm -> Rule -> Rule
substituteRule sigma (HypothesisRule hyp conc) = 
  HypothesisRule
    hyp 
      { proposition = substituteProposition sigma hyp.proposition
      , filter = substituteTerm sigma <$> hyp.filter }
    (bimap (substituteRule sigma) (substituteProposition sigma) conc)

-- | `Quantifications` is an alternating list of sets of universal/existential
-- | quantifications. Each group is a set since the ordering among universals or
-- | existentials doesn't matter.
newtype Quantifications = Quantifications
  (List 
    ( (Set UniversalQuantification) \/
      (Set ExistentialQuantification) ))

derive instance Newtype Quantifications _
derive newtype instance Show Quantifications
derive newtype instance Eq Quantifications

instance Pretty Quantifications where pretty (Quantifications quants) = pretty quants

data UniversalQuantification = UniversalQuantification TermName LatticeType

derive instance Generic UniversalQuantification _
instance Show UniversalQuantification where show x = genericShow x
derive instance Eq UniversalQuantification
derive instance Ord UniversalQuantification

instance Pretty UniversalQuantification where pretty (UniversalQuantification x ty) = "∀" <> pretty x <> ":" <+> pretty ty

data ExistentialQuantification = ExistentialQuantification TermName LatticeType

derive instance Generic ExistentialQuantification _
instance Show ExistentialQuantification where show x = genericShow x
derive instance Eq ExistentialQuantification
derive instance Ord ExistentialQuantification

instance Pretty ExistentialQuantification where pretty (ExistentialQuantification x ty) = "∃" <> pretty x <> ":" <+> pretty ty

instance Make Quantifications (Array (Either UniversalQuantification ExistentialQuantification)) where
  make = Array.uncons >>> case _ of
    Nothing -> Quantifications Nil
    Just {head: q, tail: qs} -> case q of
      Left uq -> Quantifications (go (Left (Set.singleton uq)) qs)
      Right eq -> Quantifications (go (Right (Set.singleton eq)) qs)
    where
    go :: (Set.Set UniversalQuantification \/ Set.Set ExistentialQuantification) -> Array (Either UniversalQuantification ExistentialQuantification) -> List (Set UniversalQuantification \/ Set ExistentialQuantification)
    go qset = Array.uncons >>> case _ of
      Nothing -> List.singleton qset
      Just {head: q, tail: qs} -> case qset /\ q of
        Left uqset /\ Left uq -> go (Left (Set.insert uq uqset)) qs
        Right eqset /\ Right eq -> go (Right (Set.insert eq eqset)) qs
        _ -> go (bimap Set.singleton Set.singleton q) qs <> List.singleton qset

--------------------------------------------------------------------------------
-- Database
--------------------------------------------------------------------------------

newtype DatabaseSpec = DatabaseSpec
  { fixpoints :: Map.Map FixpointSpecName FixpointSpec
  , queries :: Map.Map QuerySpecName QuerySpec
  , insertions :: Map.Map InsertionSpecName InsertionSpec
  }

derive instance Newtype DatabaseSpec _

instance Pretty DatabaseSpec where
  pretty (DatabaseSpec database) = lines
    [ "database:"
    , indent $ lines
      [ "fixpoints:" <+> indent (pretty database.fixpoints)
      , "queries:" <+> indent (pretty database.queries)
      , "insertions:" <+> indent (pretty database.insertions) ] ]

emptyDatabaseSpec :: DatabaseSpec
emptyDatabaseSpec = DatabaseSpec
  { fixpoints: Map.empty
  , queries: Map.empty
  , insertions: Map.empty }

-- | An DatabaseSpec FixpointSpec specifies a derived function that populates the
-- | DatabaseSpec with the FixpointSpec of the DatabaseSpec's Terms and the given
-- | Rules.
newtype FixpointSpec = FixpointSpec 
  { axiomNames :: Array AxiomName
  , ruleNames :: Array RuleName
  }

derive instance Newtype FixpointSpec _

instance Pretty FixpointSpec where
  pretty (FixpointSpec fixpoint) = lines
    [ "fixpoint:"
    , indent $ lines
      [ "axioms:" <+> indent (pretty fixpoint.axiomNames)
      , "rules:" <+> indent (pretty fixpoint.ruleNames) ] ]

-- | An DatabaseSpec InsertionSpec specifies a derived function that inserts Terms
-- | into the DatabaseSpec.
data InsertionSpec = InsertionSpec RelationName

instance Pretty InsertionSpec where
  pretty (InsertionSpec rel) = "insertion" <+> pretty rel

-- | An DatabaseSpec QuerySpec specifies a derived function that queries Terms of a
-- | particular form from the DatabaseSpec. The QuerySpec is encoded as a Rule,
-- | which corresponds to QuerySpec that assumes the Rule's premises and looks
-- | for a the lattice-maximal derivation of the conclusion.
data QuerySpec = QuerySpec Rule

instance Pretty QuerySpec where
  pretty (QuerySpec rule) = "query" <+> pretty rule

--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

newtype Name (label :: Symbol) = Name String
derive newtype instance Show (Name label)
derive newtype instance Eq (Name label)
derive newtype instance Ord (Name label)

type TypeName = Name "Type"
type TermName = Name "Term"
type FunctionName = Name "Function"
type RelationName = Name "Relation"
type RuleName = Name "Rule"
type AxiomName = Name "Axiom"
type DatabaseSpecName = Name "Database"
type ModuleName = Name "Module"
type FixpointSpecName = Name "Fixpoint"
type QuerySpecName = Name "Query"
type InsertionSpecName = Name "Insertion"

instance Pretty (Name label) where
  pretty (Name str) = str