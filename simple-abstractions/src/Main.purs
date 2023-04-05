module Main where

{-
import Prelude
import DijkstraTest (shortestPath_graph1)
import Effect (Effect)
import Effect.Console (log)
import Graph (showGraph)

main :: Effect Unit
main = do
  { w, path } <- shortestPath_graph1
  log $ "w = " <> show path
  log $ "path = " <> show path
-}
import Prelude
import DijkstraRelations1 as DijkstraRelations1
import DijkstraRelations3Test as DijkstraRelations3Test

main = DijkstraRelations3Test.main
