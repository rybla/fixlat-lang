module Utility where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldM)
import Data.List (List(..))
import Data.Maybe (Maybe(..))

churchIf a1 a2 = if _ then a1 else a2

mapFlipped2 a f = map (map f) a
infixl 1 mapFlipped2 as <##>

map2 f a = map (map f) a
infixr 1 map2 as <$$>

anyListM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m Boolean
anyListM _ Nil = pure false
anyListM f (Cons x xs) = f x >>= case _ of
  true -> pure true
  false -> anyListM f xs

allArrayM :: forall m a. Monad m => (a -> m Boolean) -> Array a -> m Boolean
allArrayM f xs = case Array.uncons xs of
  Nothing -> pure true
  Just {head: x, tail: xs'} -> f x >>= if _ then allArrayM f xs' else pure false 

anyArrayM :: forall m a. Monad m => (a -> m Boolean) -> Array a -> m Boolean
anyArrayM f xs = case Array.uncons xs of
  Nothing -> pure false
  Just {head: x, tail: xs'} -> f x >>= if _ then pure true else anyArrayM f xs'