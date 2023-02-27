module Graph where

import Data.Map
import Prelude
import Data.Array as Array
import Data.Lazy (Lazy)
import Data.List (List, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Utility (lookup')

-- `v` is the type of vertices
-- `e` is the type of edges
-- `w` is the type of weights
type Graph v e w
  = { vertices :: Lazy (Set v)
    , edges :: Lazy (Set e)
    , edgeVertices :: e -> { v0 :: v, v1 :: v }
    , vertexNeighbors :: v -> Array { e :: e, v :: v }
    , vertexEdges :: v -> Array e
    , edgeWeight :: e -> w
    }

-- showGraph :: forall v e w. Show v => Show e => Show w => Graph v e w -> String
-- showGraph graph =
--   let
--     es = Set.toUnfoldable graph.edges :: Array e
--     vs = Set.toUnfoldable graph.vertices :: Array v
--   in
--     Array.intercalate "\n"
--       [ "Graph:"
--       , " • vertices     : " <> show graph.vertices
--       , " • edges        : " <> show graph.edges
--       , " • edgeVertices : " <> show ((\e -> { e, vs: graph.edgeVertices e }) <$> es)
--       , " • vertexEdges  : " <> show ((\v -> { v, es: graph.vertexEdges v }) <$> vs)
--       , " • weights      : " <> show ((\e -> { e: e, w: graph.weight e }) <$> es)
--       ]
-- neighbors :: forall v e w. Eq v => Graph v e w -> v -> Array { e :: e, v :: v }
-- neighbors graph v =
--   map
--     ( \e ->
--         let
--           vs = graph.edgeVertices e
--         in
--           if vs.v0 == v then { e, v: vs.v1 } else { e, v: vs.v0 }
--     )
--     (graph.vertexEdges v)
-- fromArray ::
--   forall v w.
--   Eq v =>
--   Ord v =>
--   Ord w =>
--   Array { v0 :: v, w :: w, v1 :: v } -> Graph v { v0 :: v, w :: w, v1 :: v } w
-- fromArray =
--   ( \{ vertices, edges, vertexEdges } ->
--       { vertices
--       , edges
--       , edgeVertices: \{ v0, v1 } -> { v0, v1 }
--       , vertexEdges: \v -> Array.fromFoldable $ lookup' v vertexEdges "cannot find vertex in vertexEdges"
--       , weight: _.w
--       }
--   )
--     <<< Array.foldr
--         ( \e@{ v0, v1 } d ->
--             { vertices: Set.insert v0 $ Set.insert v1 $ d.vertices
--             , edges: Set.insert e d.edges
--             , vertexEdges:
--                 Map.alter
--                   ( case _ of
--                       Nothing -> Just $ List.fromFoldable [ e ]
--                       Just es -> Just $ e : es
--                   )
--                   v0
--                   $ Map.alter
--                       ( case _ of
--                           Nothing -> Just $ List.fromFoldable [ e ]
--                           Just es -> Just $ e : es
--                       )
--                       v1
--                       d.vertexEdges
--             }
--         )
--         { vertices: mempty :: Set v
--         , edges: mempty :: Set { v0 :: v, w :: w, v1 :: v }
--         , vertexEdges: Map.empty :: Map.Map v (List { v0 :: v, w :: w, v1 :: v })
--         }
