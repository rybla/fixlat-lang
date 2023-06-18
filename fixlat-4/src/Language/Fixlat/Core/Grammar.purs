module Language.Fixlat.Core.Grammar where

import Data.Tuple.Nested
import Data.Variant
import Prelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Hole (hole)
import Prim as Prim

--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------

data Module ty = Module (Array (Declaration ty))

type Declaration (ty :: Prim.Type) = Variant
  ( dataType :: TypeName /\ DataType
  , latticeType :: TypeName /\ LatticeType
  , functionSpecs :: FunctionName /\ FunctionSpec
  , relation :: RelationName /\ Relation
  , rule :: RuleName /\ Rule
  , indexSpec :: IndexSpecName /\ IndexSpec )

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

derive instance Generic LatticeType _
instance Show LatticeType where show x = genericShow x

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

-- A Term, annotated with type information.
type Term = Term_ TermName
data Term_ x ty
  = NeutralTerm FunctionName (Term_ x ty) ty
  | NamedTerm x ty

derive instance Generic (Term_ x ty) _
instance (Show x, Show ty) => Show (Term_ x ty) where show x = genericShow x

trueTerm :: Term LatticeType
trueTerm = hole "trueTerm"

falseTerm :: Term LatticeType
falseTerm = hole "falseTerm"

substituteTerm :: forall ty. Map.Map TermName (Term ty) -> Term ty -> Term ty
substituteTerm _sigma = hole "substituteTerm"

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

-- | A proposition of a particular instance of a Relation.
type Proposition = Proposition_ TermName
data Proposition_ x ty = Proposition (Term_ x ty)

substituteProposition :: forall ty. Map.Map TermName (Term ty) -> Proposition ty -> Proposition ty
substituteProposition _sigma = hole "substituteProposition"

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | An derivation Rule for deriving propositions of a Relation. A Rule has:
-- | - lattice-typed quantifiers, universal or existential, which are introduced
-- |   into scope for the nested Rule
-- | - hypothesis propositions
-- | - filters, which are boolean Terms that can be use quantified variables in
-- |   scope
-- | - a single conclusion proposition
data Rule = Rule (List RuleHypothesis) (Proposition LatticeType)

-- TODO: actually `Array Quantification` isn't optimal, since we know that the
-- order of consecutive existential quantifiers or consecutive universal
-- quantifiers doesn't matter.
data RuleHypothesis = RuleHypothesis
  (Array Quantification) -- quantifications before hypothesis proposition
  (Proposition LatticeType) -- hypothesis proposition
  (Maybe (Term LatticeType)) -- filter term (boolean-valued)

data Quantification = Quantification
  Quantifier
  TermName
  LatticeType

data Quantifier = Forall | Exists

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

data IndexSpec = IndexSpec (Array IndexSpecDeclaration)

type IndexSpecDeclaration = Variant
  ( fixpoint :: FixpointSpecName /\ FixpointSpec
  , query :: QuerySpecName /\ QuerySpec
  , insertion :: InsertionSpecName /\ InsertionSpec )

-- | An IndexSpec FixpointSpec specifies a derived function that populates the
-- | IndexSpec with the FixpointSpec of the IndexSpec's Terms and the given
-- | Rules.
data FixpointSpec = FixpointSpec (Array RuleName)

-- | An IndexSpec InsertionSpec specifies a derived function that inserts Terms
-- | into the IndexSpec.
data InsertionSpec = InsertionSpec RelationName

-- | An IndexSpec QuerySpec specifies a derived function that queries Terms of a
-- | particular form from the IndexSpec. The QuerySpec is encoded as a Rule,
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
type IndexSpecName = Name "index"
type ModuleName = Name "module"
type FixpointSpecName = Name "fixpoint"
type QuerySpecName = Name "query"
type InsertionSpecName = Name "insertion"
