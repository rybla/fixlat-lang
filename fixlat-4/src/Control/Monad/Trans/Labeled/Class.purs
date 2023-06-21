module Control.Monad.Trans.Labeled.Class where

import Prelude

import Prelude (class Monad)
import Type.Proxy (Proxy)

class MonadLabeledTrans (l :: Symbol) t | l -> t where
  labeledLift :: forall m a. Monad m => Proxy l -> m a -> t m a

