module Data.Lattice where

import Prelude

import Data.Maybe (Maybe(..))

type Lattice a =
  { compare :: a -> a -> Maybe Ordering
  , join :: a -> a -> a
  , meet :: a -> a -> a
  , top :: a
  , bot :: a }

leLattice :: forall a. Lattice a -> a -> a -> Boolean
leLattice lat x y = case lat.compare x y of
  Just LT -> true
  Just EQ -> true
  _ -> false

ltLattice :: forall a. Lattice a -> a -> a -> Boolean
ltLattice lat x y = case lat.compare x y of
  Just LT -> true
  _ -> false

geLattice :: forall a. Lattice a -> a -> a -> Boolean
geLattice lat x y = case lat.compare x y of
  Just GT -> true
  Just EQ -> true
  _ -> false

gtLattice :: forall a. Lattice a -> a -> a -> Boolean
gtLattice lat x y = case lat.compare x y of
  Just GT -> true
  _ -> false
