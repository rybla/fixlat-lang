module Language.Mfl.Core.Ast where

import Data.Either.Nested
import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bot (Bot, elimBot)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Top (Top)
import Data.Traversable (class Foldable, class Traversable, traverse)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Hole (hole)
import Text.Pretty (class Pretty, commas, parens, pretty, quotes)

--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------

newtype Module = Module
  { dataTypes :: Map.Map TypeName DataType
  , latTypes :: Map.Map TypeName LatType
  , functionSpecs :: Map.Map FunctionName FunctionSpec
  , relations :: Map.Map RelationName Relation
  , rules :: Map.Map RuleName Rule
  , axioms :: Map.Map AxiomName Axiom
  , fixpoints :: Map.Map FixpointSpecName FixpointSpec
  , queries :: Map.Map QuerySpecName QuerySpec
  , insertions :: Map.Map InsertionSpecName InsertionSpec
  }

derive instance Newtype Module _

newtype FunctionSpec = FunctionSpec
  { functionType :: FunctionType
  , internalImplementation :: Maybe (Array EvaluatedTerm -> EvaluatedTerm) }

derive instance Newtype FunctionSpec _

newtype FunctionType = FunctionType 
  { params :: Array DataType
  , output :: DataType }

derive instance Newtype FunctionType _

newtype Relation = Relation
  { domain :: LatType }

derive instance Newtype Relation _

newtype Axiom = Axiom
  { prop :: ConcreteProp }

derive instance Newtype Axiom _

newtype FixpointSpec = FixpointSpec
  { axiomNames :: Array AxiomName
  , ruleNames :: Array RuleName }

derive instance Newtype FixpointSpec _

newtype QuerySpec = QuerySpec
  { rule :: Rule }

derive instance Newtype QuerySpec _

newtype InsertionSpec = InsertionSpec
  { relationName :: RelationName }

derive instance Newtype InsertionSpec _

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | If `isLat = Top`, then the `Type` corresponds to a lattice. There are some
-- constructors that are only relevant when the type corresponds to a lattice.
-- If `isLat = Bot`, then the type does not correspond to a lattice.

type LatType = Type Top
type DataType = Type Bot

data Type isLat
  = BoolType
  | NatType
  | StringType
  | SetType (Type isLat)
  | TupleType (Type isLat) (Type isLat)
  | PowerSetType (Type Bot)
  -- must correspond to lattice
  | OpType isLat (Type isLat)

toDataType :: forall isLat. Type isLat -> DataType
toDataType BoolType = BoolType
toDataType NatType = NatType
toDataType StringType = StringType
toDataType (SetType s) = SetType (toDataType s)
toDataType (TupleType s t) = TupleType (toDataType s) (toDataType t)
toDataType (OpType _ s) = toDataType s
toDataType (PowerSetType s) = s

derive instance Generic (Type isLat) _
instance Show isLat => Show (Type isLat) where show x = genericShow x
derive instance Eq isLat => Eq (Type isLat) 
derive instance Ord isLat => Ord (Type isLat)

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

type RawTerm = Term (Maybe LatType) TermName NeuName -- unevaluated neutrals, open, partially annotated
type SymbolicTerm = Term LatType TermName NeuName -- unevaluated neutrals, open, annotated
type ConcreteTerm = Term LatType Bot NeuName -- unevaluated neutrals, closed, annotated
type EvaluatedTerm = Term LatType Bot Bot -- evaluated neutrals, closed, annotated

data Term sig x f = Term (PreTerm sig x f) sig

derive instance Generic (Term sig x f) _
instance (Show f, Show x, Show sig) => Show (Term sig x f) where show x = genericShow x
instance (Pretty sig, Pretty x, Pretty f) => Pretty (Term sig x f) where
  pretty (Term t _) = pretty t
instance (Eq f, Eq x, Eq sig) => Eq (Term sig x f) where eq x = genericEq x
instance (Ord f, Ord x, Ord sig) => Ord (Term sig x f) where compare x = genericCompare x

derive instance Bifunctor (Term sig)

fromEvaluatedTerm :: forall a b. EvaluatedTerm -> Term LatType a b
fromEvaluatedTerm = bimap elimBot elimBot

data PreTerm sig x f
  = NeuTerm f (Array (Term sig x f))
  | ConstrTerm (Constr (Term sig x f))
  | QuantTerm x

derive instance Generic (PreTerm sig x f) _
instance (Show f, Show x, Show sig) => Show (PreTerm sig x f) where show x = genericShow x
instance (Eq f, Eq x, Eq sig) => Eq (PreTerm sig x f) where eq x = genericEq x
instance (Ord f, Ord x, Ord sig) => Ord (PreTerm sig x f) where compare x = genericCompare x

instance (Pretty f, Pretty x, Pretty sig) => Pretty (PreTerm sig x f) where
  pretty (NeuTerm f ts) = pretty f <> " " <> parens (commas (pretty <$> ts))
  pretty (ConstrTerm c) = pretty c
  pretty (QuantTerm q) = pretty q

derive instance Bifunctor (PreTerm sig)

data Constr term
  = NatConstr (NatConstr term)
  | BoolConstr BoolConstr
  | StringConstr StringConstr
  | SetConstr (SetConstr term)
  | TupleConstr (TupleConstr term)

derive instance Generic (Constr term) _
instance Show term => Show (Constr term) where show x = genericShow x
instance Eq term => Eq (Constr term) where eq x = genericEq x
instance Ord term => Ord (Constr term) where compare x = genericCompare x
derive instance Functor Constr
derive instance Foldable Constr
derive instance Traversable Constr

instance Pretty term => Pretty (Constr term) where
  pretty (NatConstr c) = pretty c
  pretty (BoolConstr c) = pretty c
  pretty (StringConstr c) = pretty c 
  pretty (SetConstr c) = pretty c
  pretty (TupleConstr c) = pretty c

data NatConstr term
  = ZeroConstr
  | SucConstr term
  | InfinityConstr

derive instance Generic (NatConstr term) _
instance Show term => Show (NatConstr term) where show x = genericShow x
instance Eq term => Eq (NatConstr term) where eq x = genericEq x
instance Ord term => Ord (NatConstr term) where compare x = genericCompare x
derive instance Functor NatConstr
derive instance Foldable NatConstr
derive instance Traversable NatConstr

instance Pretty term => Pretty (NatConstr term) where
  pretty ZeroConstr = "Z"
  pretty (SucConstr n) = parens ("S" <> pretty n)
  pretty InfinityConstr = "∞"

data StringConstr
  = LiteralStringConstr String
  | ZetaConstr

derive instance Generic StringConstr _
instance Show StringConstr where show x = genericShow x
instance Eq StringConstr where eq x = genericEq x
instance Ord StringConstr where compare x = genericCompare x

instance Pretty StringConstr where
  pretty (LiteralStringConstr str) = quotes str
  pretty ZetaConstr = "ζ"

type BoolConstr = Boolean

trueTerm = Term (ConstrTerm (BoolConstr true)) BoolType
falseTerm = Term (ConstrTerm (BoolConstr false)) BoolType

data TupleConstr term = MakeTupleConstr term term

derive instance Generic (TupleConstr term) _
instance Show term => Show (TupleConstr term) where show x = genericShow x
instance Eq term => Eq (TupleConstr term) where eq x = genericEq x
instance Ord term => Ord (TupleConstr term) where compare x = genericCompare x
derive instance Functor TupleConstr
derive instance Foldable TupleConstr
derive instance Traversable TupleConstr

instance Pretty term => Pretty (TupleConstr term) where
  pretty (MakeTupleConstr t1 t2) = parens (commas (pretty <$> [t1, t2]))

data SetConstr term
  = LiteralSetConstr (Array term)
  | DomainConstr

derive instance Generic (SetConstr term) _
instance Show term => Show (SetConstr term) where show x = genericShow x
instance Eq term => Eq (SetConstr term) where eq x = genericEq x
instance Ord term => Ord (SetConstr term) where compare x = genericCompare x
derive instance Functor SetConstr
derive instance Foldable SetConstr
derive instance Traversable SetConstr

instance Pretty term => Pretty (SetConstr term) where
  pretty (LiteralSetConstr set) = pretty set
  pretty DomainConstr = "DOMAIN"

--------------------------------------------------------------------------------
-- Proposition
--------------------------------------------------------------------------------

type RawProp = Prop (Maybe LatType) TermName NeuName -- unevaluated neutrals, open, partially annotated
type SymbolicProp = Prop LatType TermName NeuName -- unevaluated neutrals, open, annotated
type ConcreteProp = Prop LatType Bot NeuName -- unevaluated neutrals, closed, annotated
type EvaluatedProp = Prop LatType Bot Bot -- evaluated neutrals, closed, annotated

data Prop sig x f = Prop RelationName (Term sig x f) 

derive instance Generic (Prop sig x f) _
instance (Show f, Show x, Show sig) => Show (Prop sig x f) where show x = genericShow x
derive instance Bifunctor (Prop LatType)

fromEvaluatedProp :: forall a b. EvaluatedProp -> Prop LatType a b
fromEvaluatedProp = bimap elimBot elimBot

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

data Rule
  = FilterRule SymbolicTerm Rule
  | QuantRule Quant Rule
  | LetRule TermName SymbolicTerm Rule
  | PremiseRule SymbolicProp Rule
  | ConclusionRule SymbolicProp

derive instance Generic Rule _
instance Show Rule where show x = genericShow x

data Quant = Quant QuantType TermName LatType

derive instance Generic Quant _
instance Show Quant where show x = genericShow x

data QuantType = Forall | Exists

derive instance Generic QuantType _
instance Show QuantType where show x = genericShow x

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
type ModuleName = Name "Module"
type FixpointSpecName = Name "Fixpoint"
type QuerySpecName = Name "Query"
type InsertionSpecName = Name "Insertion"

type NeuName = FunctionName \/ TermName

instance Pretty (Name label) where pretty (Name str) = str

