module Language.Fixlat.Core.Grammar where

import Data.Tuple.Nested
import Data.Variant
import Prelude
import Prim hiding (Type)

import Control.Assert (Assertion)
import Control.Assert.Refined (class Refined)
import Data.Bifunctor (class Bifunctor, lmap, rmap)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Lattice (class PartialOrd)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Hole (hole)
import Prim as Prim

--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------

newtype Module = Module
  { dataTypes :: Map.Map TypeName DataType
  , latticeTypes :: Map.Map TypeName LatticeType
  , functionSpecs :: Map.Map FunctionName FunctionSpec
  , relations :: Map.Map RelationName Relation
  , rules :: Map.Map RuleName Rule
  , databaseSpecs :: Map.Map DatabaseSpecName DatabaseSpec
  }

derive instance Newtype Module _

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A DataType encodes the structure of the Terms of a type, but not a lattice
-- | ordering over them.
data DataType
  = NamedDataType TypeName

-- | A LatticeType specifies a lattice ordering over a uniquely deTermined
-- | underlying DataType.
data LatticeType
  = NamedLatticeType TypeName
  | BooleanLatticeType

derive instance Generic LatticeType _
instance Show LatticeType where show x = genericShow x
instance Eq LatticeType where eq = genericEq

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

-- A Term, annotated with type information.
type SymbolicTerm = LatticeTerm TermName
type ConcreteTerm = LatticeTerm Void
type LatticeTerm = Term LatticeType
data Term ty x
  = NeutralTerm FunctionName (Term ty x) ty
  | ConstantTerm Constant ty
  | NamedTerm x ty

data Constant
  = TrueConstant
  | FalseConstant

derive instance Generic (Term ty x) _
instance (Show x, Show ty) => Show (Term ty x) where show x = genericShow x
instance (Eq x, Eq ty) => Eq (Term ty x) where eq x y = genericEq x y
derive instance Bifunctor Term

-- TODO: should this also be over SymbolicTerm?
instance PartialOrd ConcreteTerm where
  comparePartial = hole "PartialOrd SymbolicTerm . comparePartial"

derive instance Generic Constant _
instance Show Constant where show x = genericShow x
instance Eq Constant where eq x y = genericEq x y

substituteTerm :: Map.Map TermName SymbolicTerm -> SymbolicTerm -> SymbolicTerm
substituteTerm _sigma = hole "substituteTerm"

concreteTerm :: Assertion SymbolicTerm ConcreteTerm
concreteTerm = 
  { label: "concreteTerm"
  , check: \_prop -> hole "concreteTerm.check"
  }

toSymbolicTerm :: ConcreteTerm -> SymbolicTerm
toSymbolicTerm = rmap absurd

trueTerm :: forall x. Term LatticeType x
trueTerm = ConstantTerm TrueConstant BooleanLatticeType

falseTerm :: forall x. Term LatticeType x
falseTerm = ConstantTerm FalseConstant BooleanLatticeType

--------------------------------------------------------------------------------
-- Function
--------------------------------------------------------------------------------

-- TODO: any other metadata that a function needs?
data FunctionSpec = FunctionSpec FunctionType

-- | A FunctionType, which can either be a data function (lattice-polymorphic)
-- | or a lattice function (lattice-specific). Each use of a Function must be
-- | monotonic.
data FunctionType = FunctionType (Array DataType) DataType

--------------------------------------------------------------------------------
-- Relation
--------------------------------------------------------------------------------

-- | A Relation over Terms of a LatticeType. A Relation must be be covariant in
-- | the argument's lattice.
data Relation = Relation LatticeType

--------------------------------------------------------------------------------
-- Proposition
--------------------------------------------------------------------------------

-- | A proposition of a particular instance of a Relation.
type SymbolicProposition = Proposition LatticeType TermName
type ConcreteProposition = Proposition LatticeType Void
data Proposition ty x = Proposition (Term ty x)

derive instance Bifunctor Proposition

substituteProposition :: Map.Map TermName SymbolicTerm -> SymbolicProposition -> SymbolicProposition
substituteProposition _sigma = hole "substituteProposition"

concreteProposition :: Assertion SymbolicProposition ConcreteProposition
concreteProposition = 
  { label: "concreteProposition"
  , check: \_prop -> hole "concreteProposition.check"
  }

toSymbolicProposition :: ConcreteProposition -> SymbolicProposition
toSymbolicProposition = rmap absurd

-- TODO: should this also be over SymbolicProposition?
instance PartialOrd ConcreteProposition where
  comparePartial = hole "PartialOrd ConcreteProposition . comparePartial"

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
  = QuantificationsRule Quantifications Rule
  | PropositionRule SymbolicProposition Rule
  | FilterRule SymbolicTerm Rule
  | ConclusionRule SymbolicProposition

instance Refined "Rule" Rule where
  -- TODO: encode requirements
  validate' = hole "validate Rule"

-- | `Quantifications` is an alternating list of sets of universal/existential
-- | quantifications. Each group is a set since the ordering among universals or
-- | existentials doesn't matter.
data Quantifications
  = UniversalQuantifications (Set UniversalQuantification) (Maybe Quantifications)
  | ExistentialQuantifications (Set ExistentialQuantification) (Maybe Quantifications)

data UniversalQuantification = UniversalQuantification TermName LatticeType
data ExistentialQuantification = ExistentialQuantification TermName LatticeType

--------------------------------------------------------------------------------
-- Database
--------------------------------------------------------------------------------

newtype DatabaseSpec = DatabaseSpec
  { fixpoints :: Map.Map FixpointSpecName FixpointSpec
  , queries :: Map.Map QuerySpecName QuerySpec
  , insertions :: Map.Map InsertionSpecName InsertionSpec
  }

derive instance Newtype DatabaseSpec _

-- | An DatabaseSpec FixpointSpec specifies a derived function that populates the
-- | DatabaseSpec with the FixpointSpec of the DatabaseSpec's Terms and the given
-- | Rules.
newtype FixpointSpec = FixpointSpec {ruleNames :: Array RuleName}

derive instance Newtype FixpointSpec _

-- | An DatabaseSpec InsertionSpec specifies a derived function that inserts Terms
-- | into the DatabaseSpec.
data InsertionSpec = InsertionSpec RelationName

-- | An DatabaseSpec QuerySpec specifies a derived function that queries Terms of a
-- | particular form from the DatabaseSpec. The QuerySpec is encoded as a Rule,
-- | which corresponds to QuerySpec that assumes the Rule's premises and looks
-- | for a the lattice-maximal derivation of the conclusion.
data QuerySpec = QuerySpec Rule

--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

newtype Name (label :: Symbol) = Name String
derive newtype instance Show (Name label)
derive newtype instance Eq (Name label)
derive newtype instance Ord (Name label)

type TypeName = Name "type"
type TermName = Name "Term"
type FunctionName = Name "function"
type RelationName = Name "Relation"
type RuleName = Name "Rule"
type DatabaseSpecName = Name "index"
type ModuleName = Name "module"
type FixpointSpecName = Name "fixpoint"
type QuerySpecName = Name "query"
type InsertionSpecName = Name "insertion"
