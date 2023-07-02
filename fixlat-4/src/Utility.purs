module Utility where

import Prelude

churchIf a1 a2 = if _ then a1 else a2

mapFlipped2 a f = map (map f) a
infixl 1 mapFlipped2 as <##>

map2 f a = map (map f) a
infixr 1 map2 as <$$>