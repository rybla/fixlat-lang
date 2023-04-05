module Fixpoint where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

fixpoint :: forall a b. a -> (a -> Either b a) -> b
fixpoint a f = case f a of
  Left b -> b
  Right a' -> fixpoint a' f

type LoopT :: (Type -> Type) -> Type -> Type -> Type
type LoopT m a b
  = a -> m (Either b a)

fixpointM :: forall m a b. Monad m => (a -> m (Either b a)) -> a -> m b
fixpointM f a =
  f a
    >>= case _ of
        Left b -> pure b
        Right a' -> fixpointM f a'
