module PriorityQueue where

import Data.Tuple.Nested
import Prelude
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)

-- stores items in order from greatest to least
newtype PriorityQueue a
  = PriorityQueue (List a)

derive instance newtypePriorityQueue :: Newtype (PriorityQueue a) _

derive newtype instance showPriorityQueue :: Show a => Show (PriorityQueue a)

insert :: forall a. Ord a => a -> PriorityQueue a -> PriorityQueue a
insert a = over PriorityQueue go
  where
  go = case _ of
    Nil -> Cons a Nil
    Cons a' as ->
      if a > a' then
        Cons a (Cons a' as)
      else
        Cons a' (go as)

pop :: forall a. PriorityQueue a -> Maybe (PriorityQueue a /\ a)
pop q = case unwrap q of
  Nil -> Nothing
  Cons a as -> Just $ wrap as /\ a

empty :: forall a. PriorityQueue a
empty = wrap mempty

fromList :: forall a. Ord a => List a -> PriorityQueue a
fromList = PriorityQueue <<< List.sort

fromArray :: forall a. Ord a => Array a -> PriorityQueue a
fromArray = PriorityQueue <<< List.fromFoldable <<< Array.sort
