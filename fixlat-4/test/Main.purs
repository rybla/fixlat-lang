module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

-- import Test.Example1 as Example1
-- main :: Effect Unit
-- main = do
--   Console.log "[Test.Main.main]"
--   Example1.main

-- import Test.Add as Add
-- main :: Effect Unit
-- main = do
--   Console.log "[Test.Main.main]"
--   Add.main

-- import Test.Dijkstra as Dijkstra
-- main :: Effect Unit
-- main = do
--   Console.log "[Test.Main.main]"
--   Dijkstra.main

import Test.Parsing as Parsing
main :: Effect Unit
main = do
  Console.log "[Test.Main.main]"
  Parsing.main
