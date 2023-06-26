module Test.Dijkstra where

import Data.Tuple
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude hiding (add)
import Control.Bug (bug)
import Control.Monad.Reader (runReaderT)
import Data.AlternatingList (AlternatingList(..), (-:), (:-))
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

node = DiscreteLatticeType IntDataType
lit_node x = PrimitiveTerm (IntPrimitive x) [] node
var_node x = NamedTerm x node

weight = OpLatticeType IntLatticeType
lit_weight x = PrimitiveTerm (IntPrimitive x) [] weight
var_weight x = NamedTerm x weight

-- distance

_distance = Name "distance" :: RelationName
distance_type ∷ LatticeType
distance_type = (node `lex` node) `lex` weight
distance ∷ forall x. Term LatticeType x → Term LatticeType x → Term LatticeType x → Proposition LatticeType x
distance n1 n2 w = Proposition _distance $
  PrimitiveTerm TuplePrimitive 
    [ PrimitiveTerm TuplePrimitive [n1, n2] (node `lex` node)
    , w ] 
    distance_type

-- TODO: enable this once i get it working with just distance
-- -- step

-- -- _step = Name "step" :: RelationName
-- -- step_type ∷ LatticeType
-- -- step_type = (node `lex` node) `lex` weight
-- -- step ∷ forall x. Term LatticeType x → Term LatticeType x → Term LatticeType x → Proposition LatticeType x
-- -- step n1 n2 w = Proposition _step $ 
-- --   PrimitiveTerm TuplePrimitive 
-- --     [ PrimitiveTerm TuplePrimitive [n1, n2] (node `lex` node)
-- --     , w ] 
-- --     weight

-- add

_add = Name "add" :: FunctionName
add x y ty = NeutralTerm _add [x, y] ty
add_weight x y = add x y weight

-- db1

_db1 = Name "db1" :: DatabaseSpecName
_db1_fix1 = Name "db1_fix1" :: FixpointSpecName

-- axioms
_distance1 = Name "distance1" :: AxiomName
_distance2 = Name "distance2" :: AxiomName
_distance3 = Name "distance3" :: AxiomName
_distance4 = Name "distance4" :: AxiomName

-- rules
_distance_trans = Name "distance_trans" :: RuleName

-- module

module_ ∷ Module
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
    axioms: Map.fromFoldable
      [
        Tuple _distance1 $ Axiom $ distance (lit_node 0) (lit_node 1) (lit_weight 1)
      , 
        Tuple _distance2 $ Axiom $ distance (lit_node 1) (lit_node 2) (lit_weight 2)
      , 
        Tuple _distance2 $ Axiom $ distance (lit_node 1) (lit_node 2) (lit_weight 1)
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
        Tuple _distance_trans $
          HypothesisRule 
            { quantifications: make (forAll [a /\ node, b /\ node, v /\ weight])
            , proposition: distance (var_node a) (var_node b) (var_weight v)
            , filter: Nothing } $ Left $
          HypothesisRule 
            { quantifications: make (forAll [c /\ node, w /\ weight])
            , proposition: distance (var_node b) (var_node c) (var_weight w)
            , filter: Nothing } $ Right $
          distance (var_node a) (var_node c) (add_weight (var_weight v) (var_weight w)) ]
  , 
    databaseSpecs: Map.fromFoldable
      [ Tuple _db1 $ emptyDatabaseSpec # Newtype.over DatabaseSpec _
          { fixpoints = Map.fromFoldable
              [ Tuple _db1_fix1 $ FixpointSpec 
                  { axiomNames: [_distance1, _distance2]
                  , ruleNames: [_distance_trans] }
              ]
          }
      ]
  }

main :: Effect Unit
main = do
  Console.log "[Dijkstra.main] Start"
  let db = emptyDatabase
  Console.log $ "[Dijkstra.main] Input database:\n" <> pretty db <> "\n"
  let m = fixpoint db _db1 _db1_fix1
  let ctx = {module_}
  db' <- runReaderT (runModuleT m) ctx
  Console.log $ "[Dijkstra.main] Output database:\n" <> pretty db' <> "\n"
  pure unit
