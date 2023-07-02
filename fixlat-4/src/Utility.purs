module Utility where

import Prelude

import Data.Foldable (class Foldable, foldM)
import Data.List (List(..))

churchIf a1 a2 = if _ then a1 else a2

mapFlipped2 a f = map (map f) a
infixl 1 mapFlipped2 as <##>

map2 f a = map (map f) a
infixr 1 map2 as <$$>

anyListM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m Boolean
anyListM _ Nil = pure true
anyListM f (Cons x xs) = f x >>= case _ of
  true -> anyListM f xs
  false -> pure false

