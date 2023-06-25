module Control.Debug where

import Prelude

import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)

_DEBUG :: Boolean
_DEBUG = true

foreign import _debug :: forall a. String -> (Unit -> a) -> a

debug :: forall a. String -> (Unit -> a) -> a
debug = if _DEBUG then _debug else \_ k -> k unit

debugA :: forall f. Applicative f => String -> f Unit
debugA = if _DEBUG then \msg -> _debug msg (\_ -> pure unit) else \_ -> pure unit
