module Test.Dijkstra where

import Data.Tuple
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude hiding (add)

import Control.Bug (bug)
import Control.Monad.Reader (runReaderT)
import Data.AlternatingList (AlternatingList(..), (-:), (:-))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Make (make)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Set as Set
import Data.Variant (Variant, case_, on)
import Effect (Effect)
import Effect.Class.Console as Console
import Hole (hole)
import Language.Fixlat.Core.InternalFixpoint (emptyDatabase, fixpoint)
import Language.Fixlat.Core.ModuleT (ModuleCtx, runModuleT)
import Text.Pretty (pretty, ticks)
import Type.Proxy (Proxy(..))

lex = TupleLatticeType LexicographicTupleOrdering
-- discrete = DiscreteLatticeType
-- op = OpLatticeType
-- int = IntLatticeType
-- data_int = IntDataType
-- lit_int x = ConstructorTerm (IntConstructor x) [] int
-- var_int x = VarTerm x int
-- var_op_int x = VarTerm x (op int)

forAll = map (Left <<< uncurry UniversalQuantification)
exists = map (Right <<< uncurry ExistentialQuantification)

node = DiscreteLatticeType IntDataType :: LatticeType
lit_node x = ConstructorTerm (IntConstructor x) [] node
var_node x = VarTerm x node

weight = OpLatticeType IntLatticeType :: LatticeType
lit_weight x = ConstructorTerm (IntConstructor x) [] weight
var_weight x = VarTerm x weight

-- distance

_distance = Name "distance" :: RelationName

distance_type :: LatticeType
distance_type = (node `lex` node) `lex` weight

distance :: forall x. Term LatticeType x → Term LatticeType x → Term LatticeType x → Proposition LatticeType x
distance n1 n2 w = Proposition _distance $
  ConstructorTerm TupleConstructor 
    [ ConstructorTerm TupleConstructor [n1, n2] (node `lex` node)
    , w ] 
    distance_type

-- edge

_edge = Name "edge" :: RelationName

edge_type :: LatticeType
edge_type = (node `lex` node) `lex` weight

edge :: forall x. Term LatticeType x → Term LatticeType x → Term LatticeType x → Proposition LatticeType x
edge n1 n2 w = Proposition _edge $
  ConstructorTerm TupleConstructor 
    [ ConstructorTerm TupleConstructor [n1, n2] (node `lex` node)
    , w ]
    edge_type

-- add

_add = Name "add" :: FunctionName
add x y ty = ApplicationTerm _add [x, y] ty
add_weight x y = add x y weight

-- db

_fix = Name "fix" :: FixpointSpecName

data Graph = Graph
  { endpoint :: Int
  , edges :: Array ((Int /\ Int) /\ Int) }

makeGraphAxioms :: Graph -> Map.Map AxiomName Axiom
makeGraphAxioms (Graph graph) = Map.fromFoldable $ Array.concat
  [ [ 
      makeAxiomItem distance "graph_axiom_distance_endpoint" 
        graph.endpoint graph.endpoint 0
    ]
  , Array.mapWithIndex 
      (\i ((a /\ b) /\ w) -> makeAxiomItem edge ("graph_axiom_edge_" <> show i) a b w)
      graph.edges
  ]
  where
  makeAxiomItem rel name a b w = 
    Name name /\
    Axiom (rel (lit_node a) (lit_node b) (lit_weight w))

-- rules
_distance_transitivity = Name "distance_transitivity" :: RuleName
_distance_single_edge = Name "distance_single_edge" :: RuleName
_distance_edge = Name "distance_edge" :: RuleName
_edge_distance = Name "edge_distance" :: RuleName

-- module

makeModule :: Graph -> Module
makeModule graph = do
  let db_graph = makeGraphAxioms graph
  emptyModule # Newtype.over Module _
    { 
      functionSpecs = Map.fromFoldable
        [ 
          Tuple _add $ FunctionSpec
            { functionType: FunctionType [IntDataType, IntDataType] IntDataType
            , implementation: Just case _ of
                [ ConstructorTerm (IntConstructor x1) [] lty1
                , ConstructorTerm (IntConstructor x2) [] lty2 ] 
                | lty1 == lty2 -> 
                  ConstructorTerm (IntConstructor (x1 + x2)) [] lty1
                _ -> bug $ "invalid arguments to " <> ticks (pretty _add)
            }
        ]
    , 
      relations = Map.fromFoldable 
        [ 
          Tuple _distance $ Relation $ distance_type
        , 
          Tuple _edge $ Relation $ edge_type
        ]
    , 
      axioms = 
        Map.unions
          [ db_graph ]
    , 
      rules = Map.fromFoldable
        [ 
        --   Tuple _distance_single_edge $
        --   let 
        --     a = Name "a" :: TermName
        --     b = Name "b" :: TermName
        --     w = Name "w" :: TermName
        --   in
        --   HypothesisRule
        --     { quantifications: make (forAll [a /\ node, b /\ node, w /\ weight])
        --     , proposition: edge (var_node a) (var_node b) (var_weight w)
        --     , filter: Nothing } $ Right $
        --   -- 
        --   distance (var_node a) (var_node b) (var_weight w)
        -- ,
        --   -- ╭
        --   -- │ ∀(a: discrete(int)), ∀(b: discrete(int)), ∀(v: op(int)). distance(tuple(tuple(a, b), v))
        --   -- │ ∀(c: discrete(int)), ∀(w: op(int)). distance(tuple(tuple(b, c), w))
        --   -- ├──────────────────────────────────────────────────────────────────────────────────────────────
        --   -- │ distance(tuple(tuple(a, c), add(v, w)))
        --   -- ╰ 
        --   Tuple _distance_transitivity $
        --   let
        --     a = Name "a" :: TermName
        --     b = Name "b" :: TermName
        --     c = Name "c" :: TermName
        --     v = Name "v" :: TermName
        --     w = Name "w" :: TermName
        --   in
        --     HypothesisRule 
        --       { quantifications: make (forAll [a /\ node, b /\ node, v /\ weight])
        --       , proposition: distance (var_node a) (var_node b) (var_weight v)
        --       , filter: Nothing }
        --     $ Left $ -- ────────────────────────────────────────────────────────
        --     HypothesisRule 
        --       { quantifications: make (forAll [c /\ node, w /\ weight])
        --       , proposition: distance (var_node b) (var_node c) (var_weight w)
        --       , filter: Nothing } 
        --     $ Right $ -- ───────────────────────────────────────────────────────
        --     distance (var_node a) (var_node c) (add_weight (var_weight v) (var_weight w)) 
        -- , 
          
          -- ╭
          -- │ ∀(a: node), ∀(b: node), ∀(v: weight). distance(((a, b), v))
          -- │ ∀(c: node), ∀(w: weight). edge(((b, c), w))
          -- ├──────────────────────────────────────────────────────────────────
          -- │ distance(((a, c), v + w))
          -- ╰ 
          let
            a = Name "a" :: TermName
            b = Name "b" :: TermName
            c = Name "c" :: TermName
            v = Name "v" :: TermName
            w = Name "w" :: TermName
          in
          Tuple _distance_edge $
          -- HypothesisRule 
          --   { quantifications: make (forAll [a /\ node, b /\ node, v /\ weight])
          --   , proposition: distance (var_node a) (var_node b) (var_weight v)
          --   , filter: Nothing }
          -- $ Left $ -- ────────────────────────────────────────────────────────
          -- HypothesisRule 
          --   { quantifications: make (forAll [c /\ node, w /\ weight])
          --   , proposition: edge (var_node b) (var_node c) (var_weight w)
          --   , filter: Nothing }
          -- $ Right $ -- ───────────────────────────────────────────────────────
          -- distance (var_node a) (var_node c) (add_weight (var_weight v) (var_weight w)) 

          QuantificationRule (Left (UniversalQuantification a node)) $
          QuantificationRule (Left (UniversalQuantification b node)) $
          QuantificationRule (Left (UniversalQuantification v weight)) $
          PremiseRule (distance (var_node a) (var_node b) (var_weight v)) $
          QuantificationRule (Left (UniversalQuantification c node)) $
          QuantificationRule (Left (UniversalQuantification w weight)) $
          PremiseRule (edge (var_node b) (var_node c) (var_weight w)) $
          ConclusionRule (distance (var_node a) (var_node c) (add_weight (var_weight v) (var_weight w)))


        -- , 
        --   -- ╭
        --   -- │ ∀(c: node), ∀(w: weight). distance((b, c), w)
        --   -- │ ∀(a: node), ∀(b: node), ∀(v: weight). edge((a, b), v))
        --   -- ├─────────────────────────────────────────────────────────
        --   -- │ distance((a, c), v + w)
        --   -- ╰ 
        --   let
        --     a = Name "a" :: TermName
        --     b = Name "b" :: TermName
        --     c = Name "c" :: TermName
        --     v = Name "v" :: TermName
        --     w = Name "w" :: TermName
        --   in
        --   Tuple _edge_distance $
        --     HypothesisRule 
        --       { quantifications: make (forAll [c /\ node, w /\ weight])
        --       , proposition: distance (var_node b) (var_node c) (var_weight w)
        --       , filter: Nothing } 
        --     $ Left $ -- ────────────────────────────────────────────────────────
        --     HypothesisRule
        --       { quantifications: make (forAll [a /\ node, b /\ node, v /\ weight])
        --       , proposition: edge (var_node a) (var_node b) (var_weight v)
        --       , filter: Nothing }
        --     $ Right $ -- ───────────────────────────────────────────────────────
        --     distance (var_node a) (var_node c) (add_weight (var_weight v) (var_weight w)) 
        ]
    , 
      fixpoints = Map.fromFoldable
        [ 
          Tuple _fix $ FixpointSpec 
            { axiomNames: Just $ Array.fromFoldable (Map.keys db_graph)
            , ruleNames: Just $ 
                [
                  -- _distance_transitivity, _distance_single_edge
                  _distance_edge
                  -- _edge_distance
                ] }
        ]
    }

makeRawEdge :: Int -> Int -> Int -> (Int /\ Int) /\ Int
makeRawEdge a b w = (a /\ b) /\ w
infix 1 makeRawEdge as ~~>

main :: Effect Unit
main = do
  Console.log "[Dijkstra.main] Start"
  let
    graph = Graph
      { endpoint: 1
      , edges: 
          [ (1 ~~> 2) 10
          , (2 ~~> 4) 90 
          , (1 ~~> 3) 20
          , (3 ~~> 4) 30 ]  }

    ctx :: ModuleCtx
    ctx = 
      { module_: makeModule graph
      , initial_gas: 100 }

  let db = emptyDatabase
  Console.log $ "[Dijkstra.main] Input database:\n" <> pretty db <> "\n"
  db' <- runReaderT (runModuleT (fixpoint db _fix)) ctx
  Console.log $ "[Dijkstra.main] Output database:\n" <> pretty db' <> "\n"

  pure unit
