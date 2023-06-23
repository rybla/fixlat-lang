module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Test.Add as Add

main :: Effect Unit
main = do
  Console.log "[Test.Main.main]"
  Add.main