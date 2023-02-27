module Dijkstra where

import Pointed
import Prelude
import Control.Monad.State (StateT, execStateT, get, modify, modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldrDefault, traverse_)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Lazy (force)
import Data.List ((:))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Fixpoint (fixpoint, fixpointM)
import Graph (Graph)
import Graph as Graph
import Partial.Unsafe (unsafeCrashWith)
import Utility (fromJust', lookup', showMap)

type Input v e w
  = { graph :: Graph v e w
    , vStart :: v
    , vEnd :: v
    }

shortestPath ::
  forall m mem v e w.
  -- m
  MonadEffect m =>
  -- v
  Eq v =>
  Ord v =>
  Show v =>
  -- e
  Show e =>
  -- w
  Monoid w =>
  Show w =>
  Input v e w ->
  IMemory mem v e w ->
  m { w :: w, path :: List v }
shortestPath input imem = do
  Console.log "START"
  Console.log $ "vStart = " <> show input.vStart
  Console.log $ "vEnd  = " <> show input.vEnd
  mem <- execStateT (fixpointM unit step) (imem.init input)
  let
    w = case imem.getDistance mem input.vEnd of
      Nothing -> unsafeCrashWith $ "failed to get distance of vEnd = '" <> show input.vEnd <> "'"
      Just dist -> dist

    { path } =
      fixpoint { path: mempty, v: input.vEnd } \{ path, v } -> case imem.getPrevious mem v of
        Nothing -> unsafeCrashWith $ "cannot get previous of v = '" <> show v <> "'"
        Just v'
          | v' == input.vStart -> Left { path: unsafeCrashWith "TODO", v: unsafeCrashWith "TODO" }
          | otherwise -> Right { path: unsafeCrashWith "TODO", v: unsafeCrashWith "TODO" }
  pure { w, path }
  where
  step :: Unit -> StateT mem m (Maybe Unit)
  step _ = do
    mem <- get
    let
      v = imem.getVertex mem

      w = imem.getWeight mem
    if v == input.vEnd then do
      Console.log $ "DONE"
      pure Nothing
    else do
      Console.log $ "STEP"
      Console.log $ "mem.v = " <> show v
      Console.log $ "mem.unvisiteds = " <> show (imem.getUnvisiteds mem)
      -- visit this vertex
      let
        vs = imem.getUnvisistedNeighbors mem
      traverse_
        ( \{ e, v: v' } -> do
            modify_ $ imem.setDistance v' (w <> input.graph.edgeWeight e)
            modify_ $ imem.setPrevious v' v
        )
        vs
      -- compute closest vertex
      let
        { e, v: v' } = imem.getClosestStep mem
      -- update curent vertex and distance
      modify_ $ imem.setVertex v'
      modify_ $ imem.setWeight (w <> input.graph.edgeWeight e)
      -- continue
      pure $ Just unit

type IMemory :: Type -> Type -> Type -> Type -> Type
type IMemory mem v e w
  = { init :: Input v e w -> mem
    , getVertex :: mem -> v -- current vertex 
    , setVertex :: v -> mem -> mem
    , getWeight :: mem -> w -- current weight
    , setWeight :: w -> mem -> mem
    , getUnvisiteds :: mem -> Set v -- vertices not visited so far
    , getPrevious :: mem -> v -> Maybe v -- known previous vertex in shortest path to this vertex
    , setPrevious :: v -> v -> mem -> mem
    , getDistance :: mem -> v -> Maybe w -- known min distances
    , setDistance :: v -> w -> mem -> mem
    , getClosestStep :: mem -> { e :: e, v :: v }
    , getUnvisistedNeighbors :: mem -> List { e :: e, v :: v }
    }

-- showMemory mem =
--   Array.intercalate "\n"
--     [ "Memory:"
--     , " • v          : " <> show mem.v
--     , " • unvisiteds : " <> show mem.unvisiteds
--     , " • distance  :  " <> show mem.distance
--     ]
-- initMemory :: mem
--   initMemory =
--     { distance: Map.singleton vStart { w: mempty, path: mempty } -- all vertices have infinite distance except for starting vertex
--     , unvisiteds: Set.fromFoldable (force graph.vertices) -- all vertices are unvisited
--     , previous: Map.empty
--     , v: vStart
--     }
