module Data.Set.Lattice 
  ( Set
  , insert
  , fromFoldable, toUnfoldable
  , maxSet
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Lattice (class Lattice, (<?), (>?))
import Data.List (List, reverse, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable)

newtype Set a = Set (List a)

derive newtype instance Functor Set
derive newtype instance Foldable Set
derive newtype instance Traversable Set

-- private
unwrap ∷ forall a. Lattice a => Set a → List a
unwrap (Set as) = as
wrap :: forall a. Lattice a => List a -> Set a
wrap as = Set as
over :: forall a b. Lattice a => Lattice b => (List a -> List b) -> Set a -> Set b
over f = unwrap >>> f >>> wrap

fromFoldable :: forall t117 t119. Foldable t117 => Lattice t119 => t117 t119 -> Set t119
fromFoldable = List.fromFoldable >>> wrap

toUnfoldable :: forall t16 t17. Lattice t17 => Unfoldable t16 => Set t17 -> t16 t17
toUnfoldable = unwrap >>> List.toUnfoldable

-- | Insert `a` into a lattice set such that:
-- | - if for any `b` in the set, `a < b`, then don't insert `a`
-- | - for each `b` in the set, if `b < a`, then remove `b`, then finally insert `a` once
insert :: forall a. Lattice a => a -> Set a -> Set a
insert a = over $ f1 <<< foldr f2 (false /\ mempty)
  where 
  -- if `a` is a new element, then append it
  f1 (new /\ bs) = (if new then (a : _) else identity) (reverse bs)

  f2 :: a -> (Boolean /\ List a) -> (Boolean /\ List a)
  f2 b (new /\ bs) = case a >? b of 
    Nothing    -> (true  /\ b : bs) -- `a >< b`, so INSERT `a`
    Just true  -> (true  /\     bs) -- `a >  b`, so INSERT `a` and REMOVE `b`
    Just false -> (new   /\ b : bs) -- `a <  b`

-- | If there are any elements in the set that are part of a chain of
-- | comparisons in the set that includes an element that is greater than `a`,
-- | collects the lattice set of all these elements.
maxSet :: forall a. Lattice a => a -> Set a -> Maybe (Set a)
maxSet a = f1 <<< foldr f2 (false /\ wrap (List.singleton a))
  where
  -- if found at least one element comparable with `a`, then yield just the
  -- found maxs
  f1 :: Boolean /\ Set a -> Maybe (Set a)
  f1 (found /\ maxs) = if found then Just maxs else Nothing

  f2 :: a -> Boolean /\ Set a -> Boolean /\ Set a
  f2 b (found /\ maxs) = case a <? b of
    Nothing    -> (found /\ maxs)         -- `a >< b` so IGNORE
    Just false -> (found /\ maxs)         -- `a > b`, so IGNORE
    Just true  -> (true /\ insert b maxs) -- `a < b`, so INSERT `b` into `maxs`

