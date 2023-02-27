module DijkstraRelations where

import Effect.Random
import Prelude
import Control.Monad.State (StateT, execStateT, get, gets, lift, modify_)
import Data.Array (range)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldr, traverse_)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception.Unsafe (unsafeThrow)
import Fixpoint (fixpointM)
import Lattice (class JoinLattice, class OrdPartial, comparePartial, maxPartial)
import PriorityQueue (PriorityQueue)
import PriorityQueue as PriorityQueue
import Utility (showMap, showMap')

{-
This file defines a framework for applying Dijkstra to relationally-defined
graph distance
-}
{-
The relation of graph distance
- indexing: source => target => weight
- dependency: source, target => weight
-}
-- node identifier
newtype Node
  = Node Int

derive newtype instance eqNode :: Eq Node

derive newtype instance ordNode :: Ord Node

derive newtype instance showNode :: Show Node

-- edge weight
newtype Weight
  = Weight (Reversed (Pointed Int))

derive newtype instance eqWeight :: Eq Weight

derive newtype instance ordWeight :: Ord Weight

instance showWeight :: Show Weight where
  show (Weight w) = show w

makeFiniteWeight :: Int -> Weight
makeFiniteWeight = Weight <<< Reversed <<< Finite

makeInfiniteWeight :: Weight
makeInfiniteWeight = Weight $ Reversed $ Infinity

addWeight :: Weight -> Weight -> Weight
addWeight (Weight (Reversed n1)) (Weight (Reversed n2)) = Weight (Reversed (n1 `addPointedInt` n2))

-- graph step
type Step
  = { source :: Node, weight :: Weight, target :: Node }

showStep :: Step -> String
showStep step = show step.source <> " --> " <> show step.target <> " <= " <> show step.weight

-- graph distance
data Dist
  -- a -> b <= w
  = Step Step
  -- a ->> b <= w1, b -> c <= w2 |- a ->> c <= w1 + w2
  | Trans
    { source :: Node
    , middle :: Node
    , target :: Node
    , weight1 :: Weight
    , weight2 :: Weight
    , dist1 :: Dist
    , dist2 :: Dist
    }

derive instance genericDist :: Generic Dist _

instance showDist :: Show Dist where
  show (Step step) = showStep step
  show d = show (getSource d) <> " -->> " <> show (getTarget d) <> " <= " <> show (getWeight d)

instance eqDist :: Eq Dist where
  eq x y = genericEq x y

getSource :: Dist -> Node
getSource = case _ of
  Step step -> step.source
  Trans trans -> trans.source

getTarget :: Dist -> Node
getTarget = case _ of
  Step step -> step.target
  Trans trans -> trans.target

getWeight :: Dist -> Weight
getWeight = case _ of
  Step step -> step.weight
  Trans trans -> trans.weight1 `addWeight` trans.weight2

newtype DistOrdWeight
  = DistOrdWeight Dist

derive instance newtypeDistOrdWeight :: Newtype DistOrdWeight _

derive newtype instance eqDistOrdWeight :: Eq DistOrdWeight

instance showDistOrdWeight :: Show DistOrdWeight where
  show (DistOrdWeight d) = show d

instance ordDistOrdWeight :: Ord DistOrdWeight where
  compare (DistOrdWeight d1) (DistOrdWeight d2) = compare (getWeight d1) (getWeight d2)

type Rule
  = { premises :: Array DistPat
    , conclusion :: DistPat
    }

newtype DistPat
  = DistPat { source :: NodePat, weight :: WeightPat, target :: NodePat }

data NodePat
  = NodeVar String
  | NodeCon Node

data WeightPat
  = WeightVar String
  | WeightCon Weight

type Store
  = { {- rules :: Array Rule -- TODO: enable in order to generalize rules
    , -} steps :: Map.Map Node (Map.Map Node Step) -- steps in graph
    , dists :: Map.Map Node Dist -- distance to each visited nodes
    , nexts :: PriorityQueue DistOrdWeight -- next distances to explore, ordered by weight
    }

showStore :: Store -> String
showStore store =
  Array.intercalate "\n"
    [ "Store:"
    -- , " • steps: " <> showMap' show (showMap' show showStep) store.steps
    , " • steps:\n"
        <> Array.intercalate "\n"
            ( ("    • " <> _)
                <$> ( ( (List.toUnfoldable :: List _ -> Array _)
                        >>> map (List.toUnfoldable :: List _ -> Array _)
                        >>> Array.concat
                    )
                      ((showStep <$> _) <$> (Map.values <$> (Map.values store.steps)))
                  )
            )
    , " • dists:\n"
        <> Array.intercalate "\n"
            ( ("    • " <> _)
                <$> (List.toUnfoldable (show <$> Map.values store.dists))
            )
    , " • nexts: " <> show store.nexts
    ]

type M a
  = StateT Store Effect a

insertDist :: Dist -> M Unit
insertDist d = do
  modify_ \store ->
    store
      { dists =
        Map.alter
          ( case _ of
              Nothing -> Just d
              Just d'
                | getWeight d < getWeight d' -> pure d'
                | otherwise -> pure d
          )
          (getTarget d)
          store.dists
      }

popNext :: M (Maybe Dist)
popNext = do
  gets (PriorityQueue.pop <<< _.nexts)
    >>= case _ of
        Nothing -> pure Nothing
        Just (nexts /\ d) -> do
          modify_ (_ { nexts = nexts })
          pure $ Just (unwrap d)

insertNext :: Dist -> M Unit
insertNext d = do
  lift $ Console.log $ "insertNext: d = " <> show d
  modify_ \store -> store { nexts = PriorityQueue.insert (wrap d) store.nexts }

isVisited :: Node -> M Boolean
isVisited node = do gets \store -> Map.member node store.dists

visit :: Dist -> M Unit
visit d = do
  insertDist d
  --
  -- update nexts based on discovering this dist EXAMPLE: if we assume
  -- that we are using the rule
  -- 
  --   a ->> b <= w1       b -> c <= w2
  --   ----------------------------------------
  --   a ->> c <= w1 + w2
  --
  -- that means we have to check all `b -> c <= w2` whenever we visit a
  -- `a ->> b`
  -- 
  -- for each step `b -> c <= w2` add `a ->> b <= w1, b -> c <= w2 |- a
  -- ->> c <= w1 + w2` to the known distances
  void $ gets (Map.lookup (getTarget d) <<< _.steps)
    >>= case _ of
        -- there are no steps from here
        Nothing -> pure unit
        -- steps :: c => (b -> c <= w2)
        Just steps ->
          traverse_
            ( \step ->
                -- only step to nodes that we haven't visited already
                unlessM (isVisited step.target)
                  $ insertNext
                  $ Trans
                      { source: getSource d
                      , target: step.target
                      , middle: step.source
                      , weight1: getWeight d
                      , weight2: step.weight
                      , dist1: d
                      , dist2: Step step
                      }
            )
            (Map.values steps)

visitNextStep :: Unit -> M (Maybe Unit)
visitNextStep _ = do
  popNext
    >>= case _ of
        Nothing -> do
          lift $ Console.log "done"
          -- done 
          pure Nothing
        -- d : a ->> b <= w1
        Just d -> do
          store <- get
          lift $ Console.log $ "visit: " <> show d
          -- visit
          visit d
          -- continue
          pure (Just unit)

-- Reversed
newtype Reversed a
  = Reversed a

derive newtype instance eqReversed :: Eq a => Eq (Reversed a)

instance showReversed :: Show a => Show (Reversed a) where
  show (Reversed x) = show x

instance ordReversed :: Ord a => Ord (Reversed a) where
  compare (Reversed x) (Reversed y) = compare y x

-- Pointed
data Pointed a
  = Finite a
  | Infinity

derive instance genericPointed :: Generic (Pointed a) _

instance showPointed :: Show a => Show (Pointed a) where
  show = case _ of
    Finite x -> show x
    Infinity -> "∞"

instance eqPointed :: Eq a => Eq (Pointed a) where
  eq x y = genericEq x y

instance ordPointed :: Ord a => Ord (Pointed a) where
  compare Infinity Infinity = EQ
  compare Infinity _ = GT
  compare _ Infinity = LT
  compare (Finite x) (Finite y) = compare x y

instance ordPartialPointed :: OrdPartial a => OrdPartial (Pointed a) where
  comparePartial Infinity Infinity = pure EQ
  comparePartial Infinity _ = pure GT
  comparePartial _ Infinity = pure LT
  comparePartial (Finite x) (Finite y) = comparePartial x y

instance joinLatticePointed :: OrdPartial a => JoinLattice (Pointed a) where
  join = maxPartial
  top = Infinity

addPointedInt :: Pointed Int -> Pointed Int -> Pointed Int
addPointedInt Infinity _ = Infinity

addPointedInt _ Infinity = Infinity

addPointedInt (Finite x) (Finite y) = Finite (x + y)

-- main
main :: Effect Unit
main = do
  -- let
  --   steps =
  --     [ { source: Node 0, target: Node 1, weight: makeFiniteWeight 1 }
  --     , { source: Node 1, target: Node 2, weight: makeFiniteWeight 1 }
  --     , { source: Node 2, target: Node 3, weight: makeFiniteWeight 1 }
  --     ]
  steps <- randomGraph 2
  let
    store =
      { steps: indexSteps steps
      , dists: Map.empty
      , nexts:
          PriorityQueue.fromArray
            [ wrap (Step { source: Node 0, target: Node 0, weight: makeFiniteWeight 0 })
            ]
      }
  Console.log $ showStore store
  store' <-
    execStateT
      ( do
          fixpointM unit visitNextStep
      )
      store
  Console.log $ showStore store'
  pure unit

randomGraph :: Int -> Effect (Array Step)
randomGraph n = go (2 * n) (Node 0) []
  where
  -- random node ther than the given node
  randomNode :: Node -> Effect Node
  randomNode (Node i) = do
    i' <- randomInt 0 n
    pure <<< Node $ if i' >= i then i' + 1 else i'

  randomWeight :: Effect Weight
  randomWeight = (Weight <<< Reversed <<< Finite) <$> randomInt 1 10

  go :: Int -> Node -> Array Step -> Effect (Array Step)
  go gas node steps
    | gas <= 0 = pure steps
    | otherwise = do
      -- choose a node at random
      node' <- randomNode node
      -- choose a random reasonable weight
      weight <- randomWeight
      -- step to that node by the weight
      go (gas - 1) node' (Array.cons { source: node, target: node', weight } steps)

indexSteps :: Array Step -> Map.Map Node (Map.Map Node Step)
indexSteps =
  foldr
    ( \step ->
        Map.alter
          ( case _ of
              Nothing -> pure $ Map.singleton step.target step
              Just targets -> pure $ Map.insert step.target step targets
          )
          step.source
    )
    Map.empty
