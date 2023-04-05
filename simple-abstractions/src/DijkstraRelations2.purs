module DijkstraRelations2 where

import Prelude
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Fixpoint (LoopT, fixpointM)
import PriorityQueue (PriorityQueue)
import PriorityQueue as PriorityQueue
import Utility (maxBy)

-- | Node identifier
newtype Node
  = Node Int

derive newtype instance eqNode :: Eq Node

derive newtype instance ordNode :: Ord Node

derive newtype instance showNode :: Show Node

-- | Weight of an edge 
data Weight
  = FiniteWeight Int
  | InfiniteWeight

derive instance genericWeight :: Generic Weight _

instance eqWeight :: Eq Weight where
  eq x y = genericEq x y

instance ordWeight :: Ord Weight where
  compare InfiniteWeight InfiniteWeight = EQ
  compare InfiniteWeight _ = GT
  compare _ InfiniteWeight = LT
  compare (FiniteWeight w1) (FiniteWeight w2) = compare w1 w2

instance showWeight :: Show Weight where
  show InfiniteWeight = "∞"
  show (FiniteWeight w) = show w

addWeight :: Weight -> Weight -> Weight
addWeight (FiniteWeight x) (FiniteWeight y) = FiniteWeight (x + y)

addWeight InfiniteWeight _ = InfiniteWeight

addWeight _ InfiniteWeight = InfiniteWeight

-- | Dist 
newtype Dist
  = MakeDist { src :: Node, tgt :: Node, wgt :: Weight }

derive instance newtypeDist :: Newtype Dist _

newtype Step
  = MakeStep { src :: Node, tgt :: Node, wgt :: Weight }

derive instance newtypeStep :: Newtype Step _

data Rel
  = Dist Dist
  | Step Step

getWeight :: Rel -> Weight
getWeight = case _ of
  Dist (MakeDist d) -> d.wgt
  Step (MakeStep s) -> s.wgt

getSource :: Rel -> Node
getSource = case _ of
  Dist (MakeDist d) -> d.src
  Step (MakeStep s) -> s.src

getTarget :: Rel -> Node
getTarget = case _ of
  Dist (MakeDist d) -> d.tgt
  Step (MakeStep s) -> s.tgt

-- | Rule 
type Rule
  = { hypotheses :: List RelExpr
    , conclusion :: RelExpr
    }

data RelExpr
  = StepRelExpr StepExpr
  | DistRelExpr DistExpr

type StepExpr
  = { src :: NodeExpr, tgt :: NodeExpr, wgt :: WeightExpr }

type DistExpr
  = { src :: NodeExpr, tgt :: NodeExpr, wgt :: WeightExpr }

data NodeExpr
  = VarNodeExpr String
  | ConNodeExpr Node

data WeightExpr
  = VarWeightExpr String
  | ConWeightExpr Weight
  | AddWeightExpr WeightExpr WeightExpr

injectDist :: Dist -> DistExpr
injectDist (MakeDist d) = { src: injectNode d.src, tgt: injectNode d.tgt, wgt: injectWeight d.wgt }

injectNode :: Node -> NodeExpr
injectNode = ConNodeExpr

injectWeight :: Weight -> WeightExpr
injectWeight = ConWeightExpr

-- substitution of free variables in a relation expression
type Sub
  = { nodes :: Map.Map String Node
    , weights :: Map.Map String Weight
    }

idSub :: Sub
idSub =
  { nodes: Map.empty
  , weights: Map.empty
  }

unionSub :: Sub -> Sub -> Sub
unionSub sub1 sub2 =
  { nodes: Map.union sub1.nodes sub2.nodes
  , weights: Map.union sub1.weights sub2.weights
  }

class Subst a where
  subst :: Sub -> a -> a

instance substRelExpr :: Subst RelExpr where
  subst sub = case _ of
    StepRelExpr s -> StepRelExpr { src: subst sub s.src, tgt: subst sub s.tgt, wgt: subst sub s.wgt }
    DistRelExpr d -> DistRelExpr { src: subst sub d.src, tgt: subst sub d.tgt, wgt: subst sub d.wgt }

instance substNodeExpr :: Subst NodeExpr where
  subst sub = case _ of
    VarNodeExpr x -> case Map.lookup x sub.nodes of
      Nothing -> VarNodeExpr x
      Just n -> ConNodeExpr n
    ConNodeExpr n -> ConNodeExpr n

instance substWeightExpr :: Subst WeightExpr where
  subst sub = case _ of
    VarWeightExpr x -> case Map.lookup x sub.weights of
      Nothing -> VarWeightExpr x
      Just w -> ConWeightExpr w
    ConWeightExpr n -> ConWeightExpr n
    AddWeightExpr w1 w2 -> case subst sub w1 /\ subst sub w2 of
      ConWeightExpr w1' /\ ConWeightExpr w2' -> ConWeightExpr $ addWeight w1' w2'
      w1' /\ w2' -> AddWeightExpr w1' w2'

-- Matching
matchRel :: RelExpr -> Rel -> Maybe Sub
matchRel re r = unsafeThrow "TODO"

matchNode :: NodeExpr -> Node -> Maybe Sub
matchNode ne n = unsafeThrow "TODO"

matchWeight :: WeightExpr -> Weight -> Maybe Sub
matchWeight ne n = unsafeThrow "TODO"

maybeList :: forall a. Maybe a -> List a
maybeList = case _ of
  Nothing -> Nil
  Just a -> List.singleton a

{- Abstraction

The goal is to abstract the following components:
- how the givens are indexed
- how the deriveds are indexed
- how the nexts are prioritized

The algorithm works in this abstract way:

  loop:
  1. If `nexts` is empty, then done. Otherwise, pop the highest priority `dist`
     from `nexts`.
  1. Insert `dist` into the `deriveds`.
  2. Find rule instances that `dist` can be used as a premise for. This yields
     some partial rule instances that each have a collection of premises they
     need satisfied. Some variables in the partial rule instances can still be
     free.
  3. For each partial rule instance, look through `givens` for distances to
     satisfy the rest of the premises. This yields rule instances in which all
     variales are instantiated.
  4. Add all of these rule instances into `nexts`.

nexts:
- priority queue of dists

givens
- given a rule, yields partial rule instances

deriveds
- envs dists
- given a dist, inserts in a way that respects functional dependency (resolves
  duplicate via lattice max)

-}
type Index carry query value info
  = { carry :: carry
    , insert :: value -> carry -> carry -- insert a value into the index
    , lookup :: query -> carry -> List (value /\ info) -- lookup all values that satisfy the query, and info produced by querying them
    }

insertIndex :: forall carry query value info. value -> Index carry query value info -> Index carry query value info
insertIndex v ix = ix { carry = ix.insert v ix.carry }

queryIndex :: forall carry query value info. query -> Index carry query value info -> List (value /\ info)
queryIndex q ix = ix.lookup q ix.carry

-- | Ctx, Env
type Givens givensCarry
  = Index givensCarry RelExpr Rel Sub

type Deriveds derivedsCarry
  = Index derivedsCarry RelExpr Rel Sub

-- immtable context
type Ctx givensCarry
  = { rules :: List Rule -- all inference rules
    , givens :: Givens givensCarry -- index of the given relations
    , tryApplyHypothesis :: Rel -> Rule -> List (Sub /\ Rule) -- finds all ways of using the given distance as a hypothesis for the rule
    }

-- mutable environment
type Env derivedsCarry nextPriority
  = { deriveds :: Deriveds derivedsCarry -- index of the derived relations
    , nexts :: PriorityQueue nextPriority Rel -- queue of next relations to explore
    }

emptyEnv ::
  forall derivedsCarry nextPriority.
  { deriveds :: Deriveds derivedsCarry
  , priority :: Rel -> nextPriority
  } ->
  Env derivedsCarry nextPriority
emptyEnv { deriveds, priority } =
  { deriveds
  , nexts: PriorityQueue.empty priority
  }

-- | M
type M a
  = Effect a

loop ::
  forall givensCarry derivedsCarry nextPriority.
  Ord nextPriority =>
  Ctx givensCarry ->
  LoopT M (Env derivedsCarry nextPriority) (Deriveds derivedsCarry)
loop ctx env = do
  case PriorityQueue.pop env.nexts of
    Nothing -> do
      -- if `nexts` is empty, then done
      pure $ Left env.deriveds
    Just (nexts /\ r) -> do
      -- otherwise, pop next item and continue
      -- insert `dist` into deriveds
      env <- pure env { deriveds = insertIndex r env.deriveds }
      -- for each rule, produce all partial instances that use `d` as a premise
      let
        partRuleInsts = List.concat (ctx.tryApplyHypothesis r <$> ctx.rules)
      -- for each partial rule instance, try to satisfy rest of premises using
      -- `givens`
      let
        ds =
          List.concat
            $ ( \(sub /\ rule) ->
                  let
                    go :: Sub -> List RelExpr -> List (List Rel)
                    go _asub1 Nil = Nil : Nil

                    go sub (hyp : hyps) = do
                      -- apply current sub in hyp
                      hyp <- pure $ subst sub hyp
                      -- find all instantiations of `hyp'` using `env.givens`
                      hypInst /\ sub' <- queryIndex hyp ctx.givens
                      -- union current substitution with substitution resulting
                      -- from index query; these substitutions should never
                      -- conflict
                      sub <- pure $ unionSub sub sub'
                      (hypInst : _) <$> go sub hyps
                  in
                    List.concat $ go sub rule.hypotheses
              )
            <$> partRuleInsts
      -- add them all to `nexts` 
      env <- pure env { nexts = foldr PriorityQueue.insert nexts ds }
      -- yield new env
      pure $ Right env

run ::
  forall givensCarry derivedsCarry nextPriority.
  Ord nextPriority =>
  Ctx givensCarry ->
  Env derivedsCarry nextPriority ->
  M (Deriveds derivedsCarry)
run ctx = fixpointM (loop ctx)

-- | runExample1
--
-- givensCarry: src ==> (src --> tgt <= wgt)
-- derivedsCarry: tgt ==> (src -->> tgt <= wgt)
runExample1 = run ctx env
  where
  ctx :: Ctx (Map.Map Node (List Step))
  ctx =
    { rules: unsafeThrow "TODO"
    , tryApplyHypothesis: unsafeThrow "TODO"
    , givens: unsafeThrow "TODO"
    }

  -- givensCarry: src ==> tgt ==> (src --> tgt <= wgt)
  givens :: Givens (Map.Map Node (Map.Map Node Step))
  givens = unsafeThrow "TODO"

  env :: Env (Map.Map Node Dist) Weight
  env =
    emptyEnv
      { deriveds
      , priority: getWeight
      }

  -- derivedsCarry: tgt ==> (src -->> tgt <= wgt)
  deriveds :: Deriveds (Map.Map Node Dist)
  deriveds =
    { carry: Map.empty
    , insert:
        case _ of
          Step _ -> identity
          Dist d ->
            let
              tgt = (unwrap d).tgt

              src = (unwrap d).src
            in
              Map.alter
                ( case _ of
                    Nothing -> Just d
                    -- Just m ->
                    --   Just
                    --     ( Map.alter
                    --         ( case _ of
                    --             Nothing -> Just d
                    --             Just d' -> Just (maxBy (unwrap >>> _.wgt) d' d)
                    --         )
                    --         src
                    --         m
                    --     )
                    Just d' -> Just (maxBy (unwrap >>> _.wgt) d' d)
                )
                tgt
    , lookup:
        case _ of
          DistRelExpr { src: ConNodeExpr src, tgt, wgt } -> \m -> case Map.lookup src m of
            Nothing -> Nil
            Just d -> do
              -- TODO
              Nil
          _ -> const Nil
    }

{-
        \re m ->
            
          let
            -- expects src .
            -- first, look for matching tgt
            -- then, insert
            _ = unit
          in
            ?a

-}
