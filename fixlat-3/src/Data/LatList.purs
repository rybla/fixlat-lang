module Data.LatList where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Lattice (class Lattice, (>?))
import Data.List (List, reverse, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap, over)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))

newtype LatList a = LatList (List a)

derive instance Newtype (LatList a) _
derive newtype instance Show a => Show (LatList a)
derive newtype instance Functor LatList
derive newtype instance Foldable LatList
derive newtype instance Traversable LatList

empty :: forall a. LatList a
empty = LatList List.Nil

-- | Insert `a` into a lattice set such that:
-- | - if for any `b` in the set, `a < b`, then Nothing
-- | - for each `b` in the set, if `b < a`, then Just remove `b` and insert `a` once
insert :: forall a. Lattice a => a -> LatList a -> Maybe (LatList a)
insert a = map LatList <<< f1 <<< foldr f2 (false /\ mempty) <<< unwrap
  where 
  -- if `a` is a new element, then append it
  f1 :: Boolean /\ List a -> Maybe (List a)
  f1 (new /\ bs) = (if new then Just <<< (a : _) else const Nothing) (reverse bs)

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

