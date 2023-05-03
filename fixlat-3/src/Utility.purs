module Utility where

import Prelude

import Data.Foldable (class Foldable, foldr)

if_ :: forall a. a -> a -> Boolean -> a
if_ t _f true = t

if_ _t f false = f

comps :: forall a f. Foldable f => f (a -> a) -> a -> a
comps ff = foldr compose identity ff

map2 f = map (map f)
infixl 4 map2 as <$$>

map3 f = map (map2 f)
infixl 4 map3 as <$$$>