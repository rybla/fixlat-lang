module Data.Fix where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)

data Fix (f :: Type -> Type -> Type) (a :: Type) = Fix (Fix' f a)
type Fix' f a = f (Fix f a) a

unFix (Fix fa) = fa

mapFix :: forall f a b. Bifunctor f => (a -> b) -> Fix f a -> Fix f b
mapFix f (Fix fa) = Fix (bimap (mapFix f) f fa)


