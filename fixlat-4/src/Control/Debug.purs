module Control.Debug where

import Prelude

import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)

_DEBUG :: Boolean
_DEBUG = true

debug :: forall a. String -> (Unit -> a) -> a
debug = if not _DEBUG then \_ k -> k unit else \msg k ->
  unsafePerformEffect do
    Console.log ("[>] " <> msg)
    pure (k unit)

debugA :: forall f. Applicative f => String -> f Unit
debugA = if not _DEBUG then \_ -> pure unit else \msg ->
  unsafePerformEffect do
    Console.log ("[>] " <> msg)
    pure (pure unit)
