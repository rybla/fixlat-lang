module Control.Assert.Refined where

import Control.Assert
import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

class IsSymbol name <= Refined name a | a -> name where
  validate' :: a -> Maybe String

validate :: forall name a. Refined name a => Assertion a Unit
validate =
  { check: \a -> case validate' a of
      Nothing -> pure unit
      Just err -> Left $ "Failed refinement validation: " <> err
  , label: reflectSymbol (Proxy :: Proxy name) }

-- class RefinedM m a | m -> a where
--   validateM' :: a -> m (Maybe String)

