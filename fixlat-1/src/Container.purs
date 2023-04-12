module Container where

import Prelude

class Container (t :: Type -> Type) where
  open :: forall a. t a -> a
  mapContainer :: forall a. (a -> a) -> t a -> t a
