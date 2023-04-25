module Data.LatList where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Lattice (class Lattice, (>?))
import Data.List (List, reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))

newtype LatList a = LatList (List a)

derive instance Newtype (LatList a) _
derive newtype instance Functor LatList
derive newtype instance Foldable LatList
derive newtype instance Traversable LatList

-- private
unwrap ∷ forall a. Lattice a => LatList a → List a
unwrap (LatList as) = as
wrap :: forall a. Lattice a => List a -> LatList a
wrap as = LatList as
over :: forall a b. Lattice a => Lattice b => (List a -> List b) -> LatList a -> LatList b
over f = unwrap >>> f >>> wrap

-- | Insert `a` into a lattice set such that:
-- | - if for any `b` in the set, `a < b`, then don't insert `a`
-- | - for each `b` in the set, if `b < a`, then remove `b`, then finally insert `a` once
insert :: forall a. Lattice a => a -> LatList a -> LatList a
insert a = over $ f1 <<< foldr f2 (false /\ mempty)
  where 
  -- if `a` is a new element, then append it
  f1 (new /\ bs) = (if new then (a : _) else identity) (reverse bs)

  f2 :: a -> (Boolean /\ List a) -> (Boolean /\ List a)
  f2 b (new /\ bs) = case a >? b of 
    Nothing    -> (true  /\ b : bs) -- `a >< b`, so INSERT `a`
    Just true  -> (true  /\     bs) -- `a >  b`, so INSERT `a` and REMOVE `b`
    Just false -> (new   /\ b : bs) -- `a <  b`

-- !TODO necessary?
-- -- | If there are any elements in the set that are part of a chain of
-- -- | comparisons in the set that includes an element that is greater than `a`,
-- -- | collects the lattice set of all these elements.
-- maxLatList :: forall a. Lattice a => a -> LatList a -> Maybe (LatList a)
-- maxLatList a = f1 <<< foldr f2 (false /\ wrap (List.singleton a))
--   where
--   -- if found at least one element comparable with `a`, then yield just the
--   -- found maxs
--   f1 :: Boolean /\ LatList a -> Maybe (LatList a)
--   f1 (found /\ maxs) = if found then Just maxs else Nothing

--   f2 :: a -> Boolean /\ LatList a -> Boolean /\ LatList a
--   f2 b (found /\ maxs) = case a <? b of
--     Nothing    -> (found /\ maxs)         -- `a >< b` so IGNORE
--     Just false -> (found /\ maxs)         -- `a > b`, so IGNORE
--     Just true  -> (true /\ insert b maxs) -- `a < b`, so INSERT `b` into `maxs`

