module DijkstraTest where

import Data.Newtype
import Dijkstra
import Prelude
import Data.Array as Array
import Data.Foldable (foldrDefault)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List ((:))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Fixpoint (fixpoint)
import Graph (Graph)
import Graph as Graph
import Partial.Unsafe (unsafeCrashWith)
import Utility (fromJust', lookup')

{-
type Vertex
  = Int

type Weight
  = Additive Int

-- derive instance newtypeWeight :: Newtype Weight _
-- instance semigroupWeight :: Semigroup Weight where
--   append w1 w2 = over2 (?a :: Int -> Weight) add ?a ?a :: Weight
type Edge
  = { v0 :: Vertex, v1 :: Vertex, w :: Weight }

graph1 :: Graph Vertex Edge Weight
graph1 =
  Graph.fromArray
    [ { v0: 0, v1: 1, w: Additive 1 }
    , { v0: 1, v1: 2, w: Additive 1 }
    ]

shortestPath_graph1 :: Effect _
shortestPath_graph1 =
  shortestPath
    { graph: graph1
    , vStart: 0
    , vEnd: 2
    }
-}
