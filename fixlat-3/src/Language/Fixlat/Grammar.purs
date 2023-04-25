module Language.Fixlat.Grammar where

import Prelude
import Prim hiding (Type)

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Lattice (class JoinSemilattice, class Lattice, class MeetSemilattice, class PartialOrd)
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable, class Traversable)
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
  , param :: Param CX -- must have concrete param
  }

-- | Inference Rule
newtype Rule y xt = Rule
  { label :: Label
  , quantParams :: Array (QuantParam xt)
  , hyps :: Array (Prop y xt)
  , con :: Prop y xt
  }

derive instance Newtype (Rule y xt) _
derive instance Bifunctor Rule 
derive instance Bifoldable Rule 
derive instance Bitraversable Rule 

instance PartialOrd CRule where comparePartial = unsafeThrow "!TODO PartialOrd CRule"
instance MeetSemilattice CRule where meet = unsafeThrow "!TODO MeetSemilattice CRule"
instance JoinSemilattice CRule where join = unsafeThrow "!TODO JoinSemilattice CRule"
instance Lattice CRule

-- !TODO when/if will i need this?
-- instance PartialOrd MRule where comparePartial = unsafeThrow "!TODO PartialOrd MRule"
-- instance MeetSemilattice MRule where meet = unsafeThrow "!TODO MeetSemilattice MRule"
-- instance JoinSemilattice MRule where join = unsafeThrow "!TODO JoinSemilattice MRule"
-- instance Lattice MRule

-- | A parameter is quantified, named, and typed.
type QuantParam xt = { quant :: Quant, bind :: xt, type_ :: Lat CX }

data Quant = UnivQuant | ExistQuant

-- | A parameter is named and typed.
type Param xt = { bind :: xt, type_ :: Lat CX }

-- | A proposition is a predicate applied to an argument term.
newtype Prop y xt = Prop { bind :: CX, arg :: Term y xt }
derive instance Newtype (Prop y xs) _
derive instance Bifunctor Prop 
derive instance Bifoldable Prop 
derive instance Bitraversable Prop 

fromPropToRule :: Label -> CProp -> CRule
fromPropToRule label con = Rule
  { label
  , quantParams: []
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

-- over type annotations
derive instance Bifunctor Term
derive instance Bifoldable Term
derive instance Bitraversable Term

data AtomicTerm
  = IntTerm Int

-- | Aliases

-- typed
type TModule = Module (Type CX)
type TStmt = Stmt (Type CX)
type TRule = Rule (Type CX)
type TProp = Prop (Type CX)
type TTerm = Term (Type CX)

-- latticed (=> typed)
type LModule = Module (Lat CX)
type LStmt = Stmt (Lat CX)
type LType = Type (Lat CX)
type LProp = Prop (Lat CX)
type LTerm = Term (Lat CX)
type LRule = Rule (Lat CX)

-- concrete (=> latticed)
type CRule = LRule CX
type CQuantParam = QuantParam CX
type CParam = Param CX
type CProp = LProp CX
type CTerm = LTerm CX
