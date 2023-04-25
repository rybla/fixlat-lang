module Data.Cast where

import Type.Proxy (Proxy)

class Cast a b where cast :: Proxy b -> a -> b
