module Fixpoint where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

fixpoint :: forall a. a -> (a -> Either a a) -> a
fixpoint a f = case f a of
  Left a' -> a'
  Right a' -> fixpoint a' f

fixpointM :: forall m a. Monad m => a -> (a -> m (Maybe a)) -> m a
fixpointM a f =
  f a
    >>= case _ of
        Nothing -> pure a
        Just a' -> fixpointM a' f
