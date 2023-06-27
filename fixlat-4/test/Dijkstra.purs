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
import Effect (Effect)
import Effect.Class.Console as Console
import Hole (hole)
import Language.Fixlat.Core.InternalFixpoint (emptyDatabase, fixpoint)
import Language.Fixlat.Core.ModuleT (runModuleT)
import Text.Pretty (pretty, ticks)

lex = TupleLatticeType LexicographicTupleOrdering
-- discrete = DiscreteLatticeType
-- op = OpLatticeType
-- int = IntLatticeType
-- data_int = IntDataType
-- lit_int x = PrimitiveTerm (IntPrimitive x) [] int
-- var_int x = NamedTerm x int
-- var_op_int x = NamedTerm x (op int)

forAll = map (Left <<< uncurry UniversalQuantification)
exists = map (Right <<< uncurry ExistentialQuantification)

node = DiscreteLatticeType IntDataType :: LatticeType
lit_node x = PrimitiveTerm (IntPrimitive x) [] node
var_node x = NamedTerm x node

weight = OpLatticeType IntLatticeType :: LatticeType
lit_weight x = PrimitiveTerm (IntPrimitive x) [] weight
var_weight x = NamedTerm x weight

-- distance

_distance = Name "distance" :: RelationName

distance_type :: LatticeType
distance_type = (node `lex` node) `lex` weight

distance :: forall x. Term LatticeType x → Term LatticeType x → Term LatticeType x → Proposition LatticeType x
distance n1 n2 w = Proposition _distance $
  PrimitiveTerm TuplePrimitive 
    [ PrimitiveTerm TuplePrimitive [n1, n2] (node `lex` node)
    , w ] 
    distance_type

-- step

_step = Name "step" :: RelationName

step_type :: LatticeType
step_type = (node `lex` node) `lex` weight

step :: forall x. Term LatticeType x → Term LatticeType x → Term LatticeType x → Proposition LatticeType x
step n1 n2 w = Proposition _step $
  PrimitiveTerm TuplePrimitive 
    [ PrimitiveTerm TuplePrimitive [n1, n2] (node `lex` node)
    , w ]
    step_type

-- add

_add = Name "add" :: FunctionName
add x y ty = NeutralTerm _add [x, y] ty
add_weight x y = add x y weight

-- db

_db = Name "db" :: DatabaseSpecName
_db_fix1 = Name "db_fix1" :: FixpointSpecName
_db_fix2 = Name "db_fix2" :: FixpointSpecName

db_graph1 :: Map.Map AxiomName Axiom
db_graph1 = Map.fromFoldable $
  (\i (a /\ b /\ w) -> Tuple (Name ("graph1_axiom_distance_" <> show i) :: AxiomName) $ Axiom $ distance (lit_node a) (lit_node b) (lit_weight w))
  `Array.mapWithIndex`
  [ 1 /\ 2 /\ 10
  , 2 /\ 4 /\ 90
  , 1 /\ 3 /\ 20
  , 3 /\ 4 /\ 10
  ]

db_graph2 :: Map.Map AxiomName Axiom
db_graph2 = Map.fromFoldable $ Array.concat
  [ [ Name "graph1_axiom_distance_start" /\ 
        Axiom (distance (lit_node 1) (lit_node 1) (lit_weight 0)) 
    ]
  , Array.mapWithIndex
      (\i (a /\ b /\ w) -> Tuple (Name ("graph2_axiom_step_" <> show i)) $ 
        Axiom $ step (lit_node a) (lit_node b) (lit_weight w))
      [ 1 /\ 2 /\ 10
      , 2 /\ 4 /\ 90
      , 1 /\ 3 /\ 20
      , 3 /\ 4 /\ 30
    ] 
  ]

-- rules
_distance_transitivity = Name "distance_transitivity" :: RuleName
_distance_step = Name "distance_step" :: RuleName

-- module

module_ :: Module
module_ = Module 
  { 
    dataTypes: Map.empty
  , 
    latticeTypes: Map.empty
  ,
    functionSpecs: Map.fromFoldable
      [ 
        Tuple _add $ FunctionSpec
          { functionType: FunctionType [IntDataType, IntDataType] IntDataType
          , implementation: Just case _ of
              [ PrimitiveTerm (IntPrimitive x1) [] lty1
              , PrimitiveTerm (IntPrimitive x2) [] lty2 ] 
              | lty1 == lty2 -> 
                PrimitiveTerm (IntPrimitive (x1 + x2)) [] lty1
              _ -> bug $ "invalid arguments to " <> ticks (pretty _add)
          }
      ]
  , 
    relations: Map.fromFoldable 
      [ 
        Tuple _distance $ Relation $ (node `lex` node) `lex` weight
      ]
  , 
    axioms:
      Map.unions
        [ db_graph1 
        , db_graph2
        ]
  , 
    rules: Map.fromFoldable
      [ 
        -- distance (a, b, v)
        -- distance (b, c, w)
        -- -------------------------
        -- distance (a, c, v + w)
        let
          a = Name "a" :: TermName
          b = Name "b" :: TermName
          c = Name "c" :: TermName
          v = Name "v" :: TermName
          w = Name "w" :: TermName
        in
        Tuple _distance_transitivity $
          HypothesisRule 
            { quantifications: make (forAll [a /\ node, b /\ node, v /\ weight])
            , proposition: distance (var_node a) (var_node b) (var_weight v)
            , filter: Nothing } $ Left $
          HypothesisRule 
            { quantifications: make (forAll [c /\ node, w /\ weight])
            , proposition: distance (var_node b) (var_node c) (var_weight w)
            , filter: Nothing } $ Right $
          distance (var_node a) (var_node c) (add_weight (var_weight v) (var_weight w)) 
      , 
        -- distance (a, b, v)
        -- step (b, c, w)
        -- -------------------------
        -- distance (a, c, v + w)
        let
          a = Name "a" :: TermName
          b = Name "b" :: TermName
          c = Name "c" :: TermName
          v = Name "v" :: TermName
          w = Name "w" :: TermName
        in
        Tuple _distance_step $
          HypothesisRule 
            { quantifications: make (forAll [a /\ node, b /\ node, v /\ weight])
            , proposition: distance (var_node a) (var_node b) (var_weight v)
            , filter: Nothing } $ Left $
          HypothesisRule 
            { quantifications: make (forAll [c /\ node, w /\ weight])
            , proposition: step (var_node b) (var_node c) (var_weight w)
            , filter: Nothing } $ Right $
          distance (var_node a) (var_node c) (add_weight (var_weight v) (var_weight w)) 
      ]
  , 
    databaseSpecs: Map.fromFoldable
      [ Tuple _db $ emptyDatabaseSpec # Newtype.over DatabaseSpec _
          { fixpoints = Map.fromFoldable
              [ Tuple _db_fix1 $ FixpointSpec 
                  { axiomNames: Array.fromFoldable (Map.keys db_graph1)
                  , ruleNames: [_distance_transitivity] }
              , Tuple _db_fix2 $ FixpointSpec 
                  { axiomNames: Array.fromFoldable (Map.keys db_graph2)
                  , ruleNames: [_distance_step] }
              ]
          }
      ]
  }

main :: Effect Unit
main = do
  Console.log "[Dijkstra.main] Start"
  let ctx = 
        { module_
        , initial_gas: 1000 }
  when true do
    let db = emptyDatabase
    Console.log $ "[Dijkstra.main] Input database:\n" <> pretty db <> "\n"
    -- db' <- runReaderT (runModuleT (fixpoint db _db _db_fix1)) ctx
    db' <- runReaderT (runModuleT (fixpoint db _db _db_fix2)) ctx
    Console.log $ "[Dijkstra.main] Output database:\n" <> pretty db' <> "\n"

  pure unit
