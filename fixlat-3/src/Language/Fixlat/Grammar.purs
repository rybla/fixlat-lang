module Language.Fixlat.Grammar where

import Prelude
import Prim hiding (Type)

import Control.Biapplicative (class Biapplicative)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, rmap)
import Data.Bitraversable (class Bitraversable, rtraverse)
import Data.Lattice (class JoinSemilattice, class Lattice, class MeetSemilattice, class PartialOrd)
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable, class Traversable, foldr, traverse)
import Effect.Exception.Unsafe (unsafeThrow)

newtype Label = Label String
derive newtype instance Eq Label
derive newtype instance Ord Label

newtype Var = Var String
derive newtype instance Eq Var
derive newtype instance Ord Var

type CX = Var -- type of variables in a concrete structure

-- | Module
newtype Module y = Module
  { label :: Label
  , stmts :: Array (Stmt y)
  }

derive instance Functor Module
derive instance Foldable Module
derive instance Traversable Module

-- | Statement
data Stmt y
  = PredStmt Pred
  | RuleStmt (Rule y CX)

derive instance Functor Stmt
derive instance Foldable Stmt
derive instance Traversable Stmt

-- | Predicate
type Pred =
  { label :: Label
  , bind :: CX
  , param :: CLat
  }

-- | Inference Rule
newtype Rule y xt = Rule
  { label :: Label
  , params :: Array (Param xt)
  , hyps :: Array (Prop y xt)
  , con :: Prop y xt
  }

derive instance Newtype (Rule y xt) _
derive instance Bifunctor Rule 
derive instance Bifoldable Rule 
derive instance Bitraversable Rule

instance Functor (Rule y) where map f x = rmap f x
instance Foldable (Rule y) where
  foldr f b x = bifoldr (flip const) f b x
  foldl f b x = bifoldl const f b x
  foldMap f x = bifoldMap mempty f x
instance Traversable (Rule y) where
  traverse f x = rtraverse f x
  sequence x = traverse identity x

-- instance PartialOrd CRule where comparePartial = unsafeThrow "!TODO PartialOrd CRule"
-- instance MeetSemilattice CRule where meet = unsafeThrow "!TODO MeetSemilattice CRule"
-- instance JoinSemilattice CRule where join = unsafeThrow "!TODO JoinSemilattice CRule"
-- instance Lattice CRule

-- instance PartialOrd MRule where comparePartial = unsafeThrow "!TODO PartialOrd MRule"
-- instance MeetSemilattice MRule where meet = unsafeThrow "!TODO MeetSemilattice MRule"
-- instance JoinSemilattice MRule where join = unsafeThrow "!TODO JoinSemilattice MRule"
-- instance Lattice MRule

-- | A parameter is quantified, named, and typed.
type Param xt = { quant :: Quant, bind :: xt, type_ :: CLat }

data Quant = UnivQuant | ExistQuant

-- | A proposition is a predicate applied to an argument term.
newtype Prop y xt = Prop { pred :: CX, arg :: Term y xt }
derive instance Newtype (Prop y xs) _
derive instance Bifunctor Prop 
derive instance Bifoldable Prop 
derive instance Bitraversable Prop 

instance Functor (Prop y) where map f x = rmap f x
instance Foldable (Prop y) where
  foldr f b x = bifoldr (flip const) f b x
  foldl f b x = bifoldl const f b x
  foldMap f x = bifoldMap mempty f x
instance Traversable (Prop y) where
  traverse f x = rtraverse f x
  sequence x = traverse identity x

fromPropToRule :: Label -> CProp -> CRule
fromPropToRule label con = Rule
  { label
  , params: []
  , hyps: []
  , con
  }

-- | Lattice
data Lat xy
  = AtomicLat AtomicLat
  | -- | the lattice where `∀ a b. a >< b`
    DiscreteLat (Type xy)
  | -- | the lattice over integers where join is minimum and meet is maximum
    PointLat (Type xy)
  | VarLat xy
  | SumLat (Lat xy) (Lat xy)
  | ProdLat (Lat xy) (Lat xy)
  | NegLat (Lat xy)
  | FixLat xy (Lat xy)

data AtomicLat
  = IntLat 

-- over type variables
derive instance Functor Lat
derive instance Foldable Lat
derive instance Traversable Lat

-- | Type
data Type xy
  = AtomicType AtomicType
  | VarType xy
  | SumType (Type xy) (Type xy)
  | ProdType (Type xy) (Type xy)
  | FixyType xy (Type xy)

data AtomicType
  = IntType

-- over lattice annotations and type variables
derive instance Functor Type
derive instance Foldable Type
derive instance Traversable Type

-- | Term 
data Term y xt
  = AtomicTerm AtomicTerm y
  | VarTerm xt y
  -- product
  | ProdTerm (Term y xt) (Term y xt) y
  | Proj1Term (Term y xt) y
  | Proj2Term (Term y xt) y
  -- sum
  | Inj1Term (Term y xt) y
  | Inj2Term (Term y xt) y
  | ElimTerm (Term y xt) CX (Term y xt) CX (Term y xt) y

data AtomicTerm
  = IntTerm Int
  
-- over type annotations
derive instance Bifunctor Term
derive instance Bifoldable Term
derive instance Bitraversable Term

instance Functor (Term y) where map f x = rmap f x
instance Foldable (Term y) where
  foldr f b x = bifoldr (flip const) f b x
  foldl f b x = bifoldl const f b x
  foldMap f x = bifoldMap mempty f x
instance Traversable (Term y) where
  traverse f x = rtraverse f x
  sequence x = traverse identity x

-- | Aliases

-- typed
type TModule = Module CType
type TStmt = Stmt CType
type TRule = Rule CType
type TProp = Prop CType
type TTerm = Term CType

-- latticed (=> typed)
type LModule = Module CLat
type LStmt = Stmt CLat
type LType = Type CLat
type LProp = Prop CLat
type LTerm = Term CLat
type LRule = Rule CLat

-- concrete (=> latticed)
type CRule = LRule CX
type CParam = Param CX
type CProp = LProp CX
type CLat = Lat CX
type CType = Type CX
type CTerm = LTerm CX
