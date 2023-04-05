module DijkstraRelations3Test where

import Data.Tuple.Nested
import DijkstraRelations3
import Prelude
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.State (StateT, gets, modify_, runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Exists (mkExists)
import Data.Foldable (foldMap, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List (List(..), foldr)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import Fixpoint (LoopT, fixpointM)
import Index (Index, IndexF(..))
import Index as Index
import PriorityQueue (PriorityQueue)
import PriorityQueue as PriorityQueue
import Utility (fromJust')

test :: Array _
test = do
  x <- [ 1, 2, 3 ]
  y <- [ 4, 5, 6 ]
  -- pure [ x, y ]
  pure (x /\ y)

type P
  = Weight

-- | Ctx
initCtx :: Ctx
initCtx = { rules, givens }

{- Rules

  a -->> b <= w1
  b --> c <= w2
  --------------------
  a -->> c <= w1 + w2

-}
rules :: Rules
rules ix = do
  -- for each distance `r1 : a -->> b <= w1`
  sigma1 /\ r1 <- Index.lookup (DistRelExpr $ DistExpr { src: VarNodeExpr "a", tgt: VarNodeExpr "b", wgt: VarWeightExpr "w1" }) ix
  -- Debug.traceM $ "considering r1 = " <> show r1
  -- for each step `r2 : b --> c <= w2`
  sigma2 /\ r2 <- Index.lookup (StepRelExpr $ StepExpr { src: ConNodeExpr (getTarget r1), tgt: VarNodeExpr "c", wgt: VarWeightExpr "w2" }) ix
  -- Debug.traceM $ " • found r2 = " <> show r2
  -- yield `a -->> c <= w1 + w2`
  pure $ DistRel
    $ Dist
        { src: Map.lookup "a" sigma1.nodes `fromJust'` "sigma1 didn't determine 'a'"
        , tgt: Map.lookup "c" sigma2.nodes `fromJust'` "sigma2 didn't determine 'c'"
        , wgt:
            addWeight
              (Map.lookup "w1" sigma1.weights `fromJust'` "sigma1 didn't determine 'w1'")
              (Map.lookup "w2" sigma2.weights `fromJust'` "sigma2 didn't determine 'w2'")
        }

exampleIx :: RelIndex
exampleIx =
  mkExists
    $ Index
        { carry: Nil :: List Rel
        , lookup:
            case _ of
              DistRelExpr (DistExpr { src: VarNodeExpr src, tgt: VarNodeExpr tgt, wgt: VarWeightExpr wgt }) ->
                map (idSub /\ _)
                  <<< List.filter case _ of
                      DistRel _ -> true
                      StepRel _ -> false
              StepRelExpr (StepExpr { src, tgt, wgt }) ->
                map (idSub /\ _)
                  <<< List.filter case _ of
                      DistRel _ -> false
                      StepRel _ -> true
              _ -> const Nil
        , insert: \_ _ -> unsafeThrow "this is just an example! dont insert"
        }

-- given the steps in the graph
givens :: RelIndex
givens =
  mkExists
    $ Index
        { carry: steps :: Map.Map Node (List Rel)
        , insert: insert
        , lookup:
            case _ of
              -- a --> ?b <= ?w
              StepRelExpr (StepExpr { src: ConNodeExpr src, tgt: VarNodeExpr tgtVar, wgt: VarWeightExpr wgtVar }) -> \m -> do
                r <- maybe Nil identity $ Map.lookup src m
                pure $ (subNode tgtVar (getTarget r) $ subWeight wgtVar (getWeight r) idSub) /\ r
              -- ?a --> ?b <= ?w
              StepRelExpr (StepExpr { src: VarNodeExpr srcVar, tgt: VarNodeExpr tgtVar, wgt: VarWeightExpr wgtVar }) -> \m -> do
                (src /\ rs) <- Map.toUnfoldable m
                rs <#> \r -> (subNode srcVar src $ subNode tgtVar (getTarget r) $ subWeight wgtVar (getWeight r) $ idSub) /\ r
              -- ?a -->> ?b <= ?w
              DistRelExpr (DistExpr { src: VarNodeExpr srcVar, tgt: VarNodeExpr tgtVar, wgt: VarWeightExpr wgtVar }) -> \m -> do
                src /\ rs <- Map.toUnfoldable m :: List _
                rs
                  >>= case _ of
                      r@(DistRel (Dist d)) ->
                        List.singleton
                          $ (subNode srcVar src $ subNode tgtVar d.tgt $ subWeight wgtVar d.wgt $ idSub)
                          /\ r
                      StepRel _ -> Nil
              _ -> const Nil
        }
  where
  insert r = Map.alter (Just <<< maybe (List.singleton r) (Cons r)) (getSource r)

  -- a ==> (a --> b <= w)
  -- steps :: Map.Map Node (List Rel)
  -- steps =
  --   Array.foldr (\(n /\ x) -> Map.alter (Just <<< maybe (List.singleton x) (Cons x)) n) Map.empty <<< map makeStep
  --     $ Array.concat
  --         [ [ (0 /\ 1) /\ 1, (1 /\ 3) /\ 2 ] -- 0 --[0]--> 0 --[3]--> 3
  --         , [ (0 /\ 2) /\ 1, (2 /\ 3) /\ 1 ] -- 0 --[2]--> 3 --[0]--> 3
  --         ]
  -- 
  -- makeStep :: _ -> Node /\ Rel
  -- makeStep ((src /\ tgt) /\ wgt) = Node src /\ StepRel (Step { src: Node src, tgt: Node tgt, wgt: FiniteWeight wgt })
  -- 
  steps :: Map.Map Node (List Rel)
  steps =
    Array.foldr insert Map.empty
      $ Array.concat
          [ makeStep 0 0 1 1
          , makeStep 0 1 1 2
          , makeStep 0 2 1 3
          , makeStep 1 0 2 1
          , makeStep 1 1 2 2
          , makeStep 1 2 2 3
          , makeStep 2 0 3 1
          , makeStep 2 1 3 2
          , makeStep 2 2 3 3
          ]
    where
    makeStep x1 y1 x2 y2 =
      let
        n1 = Node (toNodeId x1 y1)

        n2 = Node (toNodeId x2 y2)

        w = FiniteWeight 1
      in
        [ StepRel (Step { src: n1, tgt: n2, wgt: w })
        , StepRel (Step { src: n2, tgt: n1, wgt: w })
        ]

    toNodeId :: Int -> Int -> Int
    toNodeId x y = x + 4 * y

-- Array.foldr (\(n /\ rs) -> Map.insert (Node n) rs) Map.empty do
-- Array.foldr insert Map.empty do
--   x <- Array.range 0 3
--   y <- Array.range 0 3
--   let
--     n = toNodeId x y
--     makeSteps n1 n2 w =
--       [ StepRel (Step { src: Node n1, tgt: Node n2, wgt: FiniteWeight w })
--       , StepRel (Step { src: Node n2, tgt: Node n1, wgt: FiniteWeight w })
--       ]
--     toNodeId :: Int -> Int -> Int
--     toNodeId x y = x + 4 * y
--     implies :: Boolean -> Boolean -> Boolean
--     implies a b = a || not b
--   -- pure (n /\ rs)
--   Array.concat
--     [ if x < 3 && ((x == 1) `implies` (y == 3)) then makeSteps n (toNodeId (x + 1) y) 1 else []
--     , if y < 3 then makeSteps n (toNodeId x (y + 1)) 1 else []
--     ]
--   x <- Array.range 0 3
--   y <- Array.range 0 3
--   let
--     n = toNodeId x y
--     makeSteps n1 n2 w =
--       [ StepRel (Step { src: Node n1, tgt: Node n2, wgt: FiniteWeight w })
--       , StepRel (Step { src: Node n2, tgt: Node n1, wgt: FiniteWeight w })
--       ]
--     rs =
--       List.fromFoldable
--         $ Array.concat
--             [ if x < 3 && y >= 2 then makeSteps n (toNodeId (x + 1) y) 1 else []
--             , if y < 3 then makeSteps n (toNodeId x (y + 1)) 1 else []
--             ]
--   pure (n /\ rs)
-- where
-- toNodeId :: Int -> Int -> Int
-- toNodeId x y = x + 4 * y
-- implies :: Boolean -> Boolean -> Boolean
-- implies a b = a || not b
-- | Env
initEnv :: Env P
initEnv = { deriveds, nexts }

startNode :: Node
startNode = Node 0

-- b ==> (startNode -->> b <= w)
deriveds :: RelIndex
deriveds =
  mkExists
    ( Index
        { carry: Map.empty :: Map.Map Node Weight
        , insert:
            case _ of
              DistRel (Dist d)
                | d.src == startNode -> Map.alter (maybe (Just d.wgt) (Just <<< min d.wgt)) d.tgt
              _ -> identity
        , lookup:
            case _ of
              DistRelExpr (DistExpr { src: ConNodeExpr src, tgt: ConNodeExpr tgt, wgt: VarWeightExpr wgtVar })
                | src == startNode ->
                  Map.lookup tgt
                    >>> maybe
                        Nil
                        ( \wgt ->
                            List.singleton
                              ( subWeight wgtVar wgt idSub
                                  /\ DistRel (Dist { src: startNode, tgt, wgt })
                              )
                        )
              DistRelExpr (DistExpr { src: VarNodeExpr srcVar, tgt: ConNodeExpr tgt, wgt: VarWeightExpr wgtVar }) ->
                Map.lookup tgt
                  >>> maybe
                      Nil
                      ( \wgt ->
                          List.singleton
                            ( (subNode srcVar startNode $ subWeight wgtVar wgt $ idSub)
                                /\ DistRel (Dist { src: startNode, tgt, wgt })
                            )
                      )
              DistRelExpr (DistExpr { src: ConNodeExpr src, tgt: VarNodeExpr tgtVar, wgt: VarWeightExpr wgtVar })
                | src == startNode -> \m -> do
                  tgt /\ wgt <- Map.toUnfoldable m :: List (Node /\ Weight)
                  pure
                    $ (subNode tgtVar tgt $ subWeight wgtVar wgt $ idSub)
                    /\ DistRel (Dist { src: startNode, tgt, wgt })
              DistRelExpr (DistExpr { src: VarNodeExpr srcVar, tgt: VarNodeExpr tgtVar, wgt: VarWeightExpr wgtVar }) -> \m -> do
                tgt /\ wgt <- Map.toUnfoldable m :: List (Node /\ Weight)
                pure
                  $ (subNode srcVar startNode $ subNode tgtVar tgt $ subWeight wgtVar wgt $ idSub)
                  /\ DistRel (Dist { src: startNode, tgt, wgt })
              _ -> const Nil
        }
    )

derivedsList :: RelIndex -> List Rel
derivedsList ix = Index.lookup (DistRelExpr $ DistExpr { src: ConNodeExpr startNode, tgt: VarNodeExpr "a", wgt: VarWeightExpr "w" }) ix <#> \(_sigma /\ r) -> r

nexts :: PriorityQueue P Rel
nexts =
  PriorityQueue.insert (DistRel $ Dist { src: startNode, tgt: startNode, wgt: FiniteWeight 0 })
    $ PriorityQueue.empty getWeight

main :: Effect Unit
main = do
  -- traverse_ (\(_ /\ r) -> Console.log $ show (getSource r) <> " -> " <> show (getTarget r))
  --   (Index.lookup (StepRelExpr (StepExpr { src: VarNodeExpr "src", tgt: VarNodeExpr "tgt", wgt: VarWeightExpr "wgt" })) givens)
  traverse_ (\(_ /\ r) -> Console.log $ show (getSource r) <> " -> " <> show (getTarget r))
    (Index.lookup (StepRelExpr (StepExpr { src: ConNodeExpr (Node 9), tgt: VarNodeExpr "tgt", wgt: VarWeightExpr "wgt" })) givens)
  Console.log "-----"
  deriveds' /\ _ <- runStateT (runReaderT run initCtx) initEnv
  let
    rs = derivedsList deriveds'
  Console.log "deriveds:"
  Console.log
    $ "["
    <> List.intercalate ", " (rs <#> \r -> "[" <> show (getTarget r) <> ", " <> show (getWeight r) <> "]")
    <> "]"
