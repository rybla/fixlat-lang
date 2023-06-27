module Control.Assert.Refined where

import Control.Assert
import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested
import Type.Proxy (Proxy(..))

class IsSymbol name <= Refined name a | a -> name where
  validate' :: a -> Either String Unit

validate :: forall name a. Refined name a => Assertion a Unit
validate =
  { check: \a -> case validate' a of
      Left err -> Left $ "Failed refinement validation: " <> err
      Right _ -> pure unit
  , label: reflectSymbol (Proxy :: Proxy name) }

instance (Refined aName a, Refined bName b) => Refined "Tuple" (Tuple a b) where
  validate' (a /\ b) = do
    _ <- validate' a
    _ <- validate' b
    pure unit
