module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Test.Parsing as Parsing

main :: Effect Unit
main = do
  Console.log "begin tests"
  Parsing.test
  Console.log "end tests"
