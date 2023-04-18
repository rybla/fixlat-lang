module Language.Fixlat.Grammar where

import Data.Variant
import Prelude (class Functor)
import Prim hiding (Type)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable, class Traversable)

newtype Label = Label String
newtype Name = Name String

-- | Module
newtype Module l y = Module
  { label :: Label
  , stmts :: Array (Stmt l y)
  }

type Stmt l y = Variant
  ( pred :: Pred l
  , rule :: Rule l y
  )

-- | Predicate
newtype Pred l = Pred 
  { label :: Label
  , name :: Name 
  , param :: Param l
  }

derive instance Generic (Pred l) _
derive instance Newtype (Pred l) _

-- | Inference Rule
newtype Rule l y = Rule
  { label :: Label 
  , name :: Name
  , params :: Param l
  , hyps :: Prop y
  , con :: Prop y
  }

-- | A parameter is named and typed.
newtype Param l = Param { name :: Name, type_ :: Type l }

-- | A proposition is a predicate applied to an argument term.
newtype Prop y = Prop { name :: Name, arg :: Term y }

-- | Lattice

type Lat = Lat' Name

data Lat' x
  = AtomicLat AtomicLat 
  | NamedLat x
  | SumLat (Lat' x) (Lat' x)
  | ProdLat (Lat' x) (Lat' x)
  | NegLat (Lat' x)
  | FixLat x (Lat' x)

data AtomicLat
  -- | the lattice where for all `x y, x </> y``
  = DiscreteLat 
  -- | the lattice where `∀ x y. x == y`
  | PointLat 
  -- | the lattice over integers where join is minimum and meet is maximum
  | IntLat 

-- over names
derive instance Functor Lat'
derive instance Foldable Lat'
derive instance Traversable Lat'

-- | Type

type Type = Type' Name

data Type' x l
  = AtomicType AtomicType l
  | NamedType x l
  | SumType (Type' x l) (Type' x l) l
  | ProdType (Type' x l) (Type' x l) l
  | FixType x (Type' x l) l

data AtomicType
  = IntType

-- over names and lattice annotations
derive instance Bifunctor Type'
derive instance Bifoldable Type'
derive instance Bitraversable Type'

-- over lattice annotations
derive instance Functor Type
derive instance Foldable Type
derive instance Traversable Type

-- | Term 

type Term = Term' Name

data Term' x y
  = AtomicTerm AtomicTerm y
  | NamedTerm x y
  -- constructor product
  | ProdTerm (Term' x y) (Term' x y) y
  -- eliminate product
  | Proj1Term (Term' x y) y
  | Proj2Term (Term' x y) y
  -- construct sum
  | Inj1Term (Term' x y) y
  | Inj2Term (Term' x y) y
  -- eliminate sum
  | ElimTerm (Term' x y) x (Term' x y) x (Term' x y) y
  
-- over names and type annotations
derive instance Bifunctor Term'
derive instance Bifoldable Term'
derive instance Bitraversable Term'

-- over type annotations
derive instance Functor Term
derive instance Foldable Term
derive instance Traversable Term

data AtomicTerm
  = IntTerm Int
