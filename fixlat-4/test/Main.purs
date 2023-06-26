module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Test.Add as Add
import Test.Example1 as Example1
import Test.Dijkstra as Dijkstra

main :: Effect Unit
main = do
  Console.log "[Test.Main.main]"
  -- Add.main
  -- Example1.main
  Dijkstra.main