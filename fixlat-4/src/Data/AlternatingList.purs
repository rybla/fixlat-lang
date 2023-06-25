module Data.AlternatingList where

import Prelude

import Data.Bifoldable (class Bifoldable, bifoldl)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Text.Pretty (class Pretty, pretty)

data AlternatingList a b
  = LeftCons a (AlternatingList a b)
  | RightCons b (AlternatingList a b)
  | AlternatingNil

derive instance Generic (AlternatingList a b) _
instance (Show a, Show b) => Show (AlternatingList a b) where show x = genericShow x
derive instance (Eq a, Eq b) => Eq (AlternatingList a b)
derive instance Functor (AlternatingList a)
derive instance Foldable (AlternatingList a)
derive instance Traversable (AlternatingList a)
derive instance Bifunctor AlternatingList
derive instance Bifoldable AlternatingList
derive instance Bitraversable AlternatingList

infix 6 LeftCons as -:
infix 6 RightCons as :-

toList :: forall a b. AlternatingList a b -> List (Either a b)
toList AlternatingNil = Nil
toList (LeftCons a as) = Left a List.: toList as
toList (RightCons b bs) = Right b List.: toList bs

reverse :: forall a b. AlternatingList a b -> AlternatingList a b
reverse = bifoldl (flip LeftCons) (flip RightCons) AlternatingNil

instance (Pretty a, Pretty b) => Pretty (AlternatingList a b) where
  pretty = pretty <<< toList