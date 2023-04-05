module PriorityQueue where

import Data.Tuple.Nested
import Prelude
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)

-- stores items in order from greatest to least
newtype PriorityQueue p a
  = PriorityQueue
  { priority :: a -> p
  , items :: List a
  }

derive instance newtypePriorityQueue :: Newtype (PriorityQueue p a) _

-- derive newtype instance showPriorityQueue :: Show a => Show (PriorityQueue p a)
insert :: forall p a. Ord p => a -> PriorityQueue p a -> PriorityQueue p a
insert a q = over PriorityQueue (\q -> q { items = go q.items }) q
  where
  priority = (unwrap q).priority

  go = case _ of
    Nil -> Cons a Nil
    Cons a' as ->
      if priority a > priority a' then
        Cons a (Cons a' as)
      else
        Cons a' (go as)

pop :: forall p a. PriorityQueue p a -> Maybe (PriorityQueue p a /\ a)
pop q = case (unwrap q).items of
  Nil -> Nothing
  Cons a as -> Just $ over PriorityQueue (_ { items = as }) q /\ a

clear :: forall p a. PriorityQueue p a -> PriorityQueue p a
clear = over PriorityQueue (_ { items = Nil })

fromList :: forall p a. Ord p => (a -> p) -> List a -> PriorityQueue p a
fromList priority items =
  PriorityQueue
    { priority
    , items:
        List.sortBy
          (\a1 a2 -> compare (priority a1) (priority a2))
          items
    }

toList :: forall p a. PriorityQueue p a -> List a
toList q = (unwrap q).items

fromArray :: forall p a. Ord p => (a -> p) -> Array a -> PriorityQueue p a
fromArray priority = fromList priority <<< Array.toUnfoldable

null :: forall p a. PriorityQueue p a -> Boolean
null = List.null <<< toList

empty :: forall p a. (a -> p) -> PriorityQueue p a
empty priority =
  PriorityQueue
    { priority
    , items: mempty
    }
