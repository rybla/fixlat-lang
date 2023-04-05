module DijkstraRelations3 where

import Prelude
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, gets, modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), foldr)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception.Unsafe (unsafeThrow)
import Fixpoint (LoopT, fixpointM)
import Index (Index)
import Index as Index
import PriorityQueue (PriorityQueue)
import PriorityQueue as PriorityQueue

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
  = Dist { src :: Node, tgt :: Node, wgt :: Weight }

derive instance newtypeDist :: Newtype Dist _

instance showDist :: Show Dist where
  show (Dist d) = Array.intercalate " " [ show d.src, "-->>", show d.tgt, "<=", show d.wgt ]

newtype Step
  = Step { src :: Node, tgt :: Node, wgt :: Weight }

derive instance newtypeStep :: Newtype Step _

instance showStep :: Show Step where
  show (Step s) = Array.intercalate " " [ show s.src, "-->", show s.tgt, "<=", show s.wgt ]

data Rel
  = DistRel Dist
  | StepRel Step

instance showRel :: Show Rel where
  show = case _ of
    StepRel s -> show s
    DistRel d -> show d

getWeight :: Rel -> Weight
getWeight = case _ of
  DistRel (Dist d) -> d.wgt
  StepRel (Step s) -> s.wgt

getSource :: Rel -> Node
getSource = case _ of
  DistRel (Dist d) -> d.src
  StepRel (Step s) -> s.src

getTarget :: Rel -> Node
getTarget = case _ of
  DistRel (Dist d) -> d.tgt
  StepRel (Step s) -> s.tgt

-- | *Expr
data RelExpr
  = StepRelExpr StepExpr
  | DistRelExpr DistExpr

newtype StepExpr
  = StepExpr { src :: NodeExpr, tgt :: NodeExpr, wgt :: WeightExpr }

newtype DistExpr
  = DistExpr { src :: NodeExpr, tgt :: NodeExpr, wgt :: WeightExpr }

data NodeExpr
  = VarNodeExpr String
  | ConNodeExpr Node

data WeightExpr
  = VarWeightExpr String
  | ConWeightExpr Weight
  | AddWeightExpr WeightExpr WeightExpr

injectDist :: Dist -> DistExpr
injectDist (Dist d) = DistExpr { src: injectNode d.src, tgt: injectNode d.tgt, wgt: injectWeight d.wgt }

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

subNode :: String -> Node -> Sub -> Sub
subNode x v sub = sub { nodes = Map.insert x v sub.nodes }

subWeight :: String -> Weight -> Sub -> Sub
subWeight x v sub = sub { weights = Map.insert x v sub.weights }

class Subst a where
  subst :: Sub -> a -> a

instance substRelExpr :: Subst RelExpr where
  subst sub = case _ of
    StepRelExpr (StepExpr s) -> StepRelExpr (StepExpr { src: subst sub s.src, tgt: subst sub s.tgt, wgt: subst sub s.wgt })
    DistRelExpr (DistExpr d) -> DistRelExpr (DistExpr { src: subst sub d.src, tgt: subst sub d.tgt, wgt: subst sub d.wgt })

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

-- | match*
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

-- | RelIndex
type RelIndex
  = Index RelExpr Rel Sub

-- | Env
type Env p
  = { deriveds :: RelIndex -- derived relations
    , nexts :: PriorityQueue p Rel -- next relations to consider
    }

-- | Ctx  
type Ctx
  = { rules :: Rules -- produces all relations directly inferrable from using the givens relations as hypotheses
    , givens :: RelIndex -- assumed relations
    }

type Rules
  = RelIndex -> List Rel

-- | M 
type M p
  = ReaderT Ctx (StateT (Env p) Effect)

-- | loop
run :: forall p. Ord p => M p RelIndex
run = fixpointM loop unit

loop ::
  forall p.
  Ord p =>
  LoopT
    (ReaderT Ctx (StateT (Env p) Effect))
    Unit
    RelIndex
loop _ =
  -- try to pop next relation of `nexts`
  gets (PriorityQueue.pop <<< _.nexts)
    >>= case _ of
        Nothing -> do
          -- if `nexts` is empty, then done
          Console.log $ "done"
          Left <$> gets _.deriveds
        Just (nexts /\ r) -> do
          -- otherwise, pop next relation of `nexts`
          Console.log $ "pop: " <> show r
          modify_ \env -> env { nexts = nexts }
          -- insert `r` into deriveds
          modify_ \env -> env { deriveds = Index.insert r env.deriveds }
          -- infer all possible relations given `r` and `givens`
          rs <- asks _.rules <*> (Index.insert r <$> asks _.givens)
          -- push the unvisited inferred relations into `nexts`
          rs' <- List.filterM isUnvisited rs
          Console.log $ "new: " <> show (Array.fromFoldable rs')
          modify_ \env -> env { nexts = foldr PriorityQueue.insert env.nexts rs' }
          -- continue 
          pure $ Right unit
  where
  isUnvisited :: _
  isUnvisited r =
    gets \env ->
      List.null
        $ Index.lookup
            ( DistRelExpr
                ( DistExpr
                    { src: VarNodeExpr "a"
                    , tgt: ConNodeExpr (getTarget r)
                    , wgt: VarWeightExpr "w"
                    }
                )
            )
            env.deriveds
