module Language.Fixlat.Core.Grammar where

import Prim hiding (Type)
import Prim as Prim

--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

data Module (ty :: Prim.Type) = Module ModuleName (Array (Declaration ty))

data Declaration (ty :: Prim.Type)
  = DataTypeDeclaration TypeName DataType
  | LatticeTypeDeclaration TypeName LatticeType
  | FunctionDeclaration FunctionName FunctionType
  | RelationDeclaration RelationName Relation
  | RuleDeclaration Rule
  | DatabaseDeclaration DatabaseName Database

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A data type encodes the structure of the terms of a type, but not a lattice
-- | ordering over them.
data DataType
  = NamedDataType TypeName

-- | A lattice type specifies a lattice ordering over a uniquely determined
-- | underlying data type.
data LatticeType
  = NamedLatticeType TypeName

-- A term, annotated with type information.
data Term ty
  = NeutralTerm TermName (Term ty) ty
  | NamedTerm TermName ty

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | A function type, which can either be a data function (lattice-polymorphic)
-- | or a lattice function (lattice-specific).
data FunctionType
  = DataFunctionType (Array DataType) DataType
  | LatticeFunctionType (Array LatticeType) LatticeType

--------------------------------------------------------------------------------
-- Relations
--------------------------------------------------------------------------------

-- | A relation over terms of a type.
-- | 
-- | A relation allows lattice-polymorphism in the argument type.
-- |
-- | A relation must be be covariant in the argument's lattice. Rules that
-- | propositions of this relation yield obligations to ensure this.
data Relation 
  = DataRelation DataType
  | LatticeRelation LatticeType

-- | A proposition of a particular instance of a relation.
data Proposition ty = Proposition (Term ty)

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

-- | An derivation rule for deriving propositions of a relation. A rule has:
-- | - lattice-typed quantifiers, universal or existential, which are introduced
-- |   into scope for the nested rule
-- | - hypothesis propositions
-- | - filters, which are boolean terms that can be use quantified variables in
-- |   scope
-- | - a single conclusion proposition
data Rule 
  = QuantifierRule  Rule
  | HypothesisRule (Proposition LatticeType) Rule
  | FilterRule (Term LatticeType) Rule
  | ConclusionRule (Proposition LatticeType)

data Quantification = Quantification Quantifier TermName LatticeType

data Quantifier = Forall | Exists

--------------------------------------------------------------------------------
-- Databases
--------------------------------------------------------------------------------

data Database = Database (Array Fixpoint) (Array Query) (Array Insertion)

-- | A Database fixpoint specifies a derived function that generates a
-- | fixpoint, which can later be queried from and inserted into.
data Fixpoint

-- | A Database insertion specifies a derived function that inserts terms into
-- | the Database.
data Insertion

-- | A Database query specifies a derived function that queries terms of a
-- | particular form from the Database, once it has been populated by fixpoints
-- | and insertions.
data Query

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

newtype TypeName = TypeName String

newtype TermName = TermName String

newtype FunctionName = FunctionName String

newtype RelationName = RelationName String

newtype RuleName = RuleName String

newtype DatabaseName = DatabaseName String

newtype ModuleName = ModuleName String