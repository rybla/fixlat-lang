module Language.Fixlat.Grammar where

import Prelude
import Prim hiding (Type)

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Either.Nested (type (\/))
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

newtype MVar = MVar String
derive newtype instance Eq MVar
derive newtype instance Ord MVar

type CX = Var -- type of variables in a concrete structure
type MX = MVar \/ Var -- type of variables in a meta structure

-- | Module
newtype Module y = Module
  { label :: Label
  , stmts :: Array (Stmt y)
  }
type TModule l = Module (Type l CX) -- typed
type LModule = TModule (Lat CX) -- latticed

derive instance Functor Module
derive instance Foldable Module
derive instance Traversable Module

-- | Statement
data Stmt y
  = PredStmt Pred
  | RuleStmt (Rule y CX)
type TStmt l = Stmt (Type l CX) -- typed
type LStmt = TStmt (Lat CX) -- latticed

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
type TRule l = Rule (Type l CX) -- typed
type LRule = TRule (Lat CX) -- latticed
type CRule = LRule CX -- concrete
type MRule = LRule MX -- meta

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

-- | A parameter is named and typed.
type QuantParam xt = { bind :: xt, type_ :: Type (Lat CX) CX }
type CQuantParam = QuantParam CX
type MQuantParam = QuantParam MX

-- | A parameter is named and typed.
type Param xt = { bind :: xt, type_ :: Type (Lat CX) CX }
type CParam = Param CX
type MParam = Param MX

-- | A proposition is a predicate applied to an argument term.
type Prop y xt = { bind :: CX, arg :: Term y xt }
type TProp xt = Prop (Type (Lat CX) CX) xt
type CProp = TProp CX
type MProp = TProp MX

fromPropToRule :: Label -> CProp -> CRule
fromPropToRule label con = Rule
  { label
  , quantParams: []
  , hyps: []
  , con
  }

-- | Lattice
data Lat xl
  = AtomicLat AtomicLat 
  | VarLat xl
  | SumLat (Lat xl) (Lat xl)
  | ProdLat (Lat xl) (Lat xl)
  | NegLat (Lat xl)
  | FixlLat CX (Lat xl)

data AtomicLat
  -- | the lattice where `∀ a b. a >< b`
  = DiscreteLat 
  -- | the lattice where `∀ a b. a == b`
  | PointLat 
  -- | the lattice over integers where join is minimum and meet is maximum
  | IntLat 

derive instance Functor Lat
derive instance Foldable Lat
derive instance Traversable Lat

-- | Type
data Type l xy
  = AtomicType AtomicType l
  | VarType xy l
  | SumType (Type l xy) (Type l xy) l
  | ProdType (Type l xy) (Type l xy) l
  | FixyType CX (Type l xy) l

data AtomicType
  = IntType

type LType = Type (Lat CX) -- latticed

-- over lattice annotations
derive instance Bifunctor Type
derive instance Bifoldable Type
derive instance Bitraversable Type

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
  
type TTerm l = Term (Type l CX) -- typed
type LTerm = TTerm (Lat CX) -- latticed
type CTerm = LTerm CX -- concrete
type MTerm = LTerm MX

-- over type annotations
derive instance Bifunctor Term
derive instance Bifoldable Term
derive instance Bitraversable Term

data AtomicTerm
  = IntTerm Int
