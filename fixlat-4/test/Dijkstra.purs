module Test.Dijkstra where

import Data.Tuple
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude hiding (add)

import Data.AlternatingList (AlternatingList(..), (-:), (:-))
import Data.Either (Either(..))
import Data.Make (make)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Hole (hole)

_step_distance = Name "step_distance" :: RuleName

lex = TupleLatticeType LexicographicTupleOrdering
discrete = DiscreteLatticeType
op = OpLatticeType
int = IntLatticeType
data_int = IntDataType
lit_int x = PrimitiveTerm (IntPrimitive x) [] int
var_int x = NamedTerm x int
var_op_int x = NamedTerm x (op int)

node = int
weight = op int

add_op_int v w = NeutralTerm (Name "add") [PrimitiveTerm TuplePrimitive [v, w] (lex (op int) (op int))] (op int)

var_node = var_int
var_weight = var_op_int

_distance = Name "distance" :: RelationName
distance_type ∷ LatticeType
distance_type = (discrete data_int `lex` discrete data_int) `lex` op int
distance ∷ forall x. Term LatticeType x → Term LatticeType x → Term LatticeType x → Proposition LatticeType x
distance n1 n2 w = Proposition _distance $
  PrimitiveTerm TuplePrimitive 
    [ PrimitiveTerm TuplePrimitive [n1, n2] (discrete data_int `lex` discrete data_int)
    , w ] 
    (op int)

_step = Name "step" :: RelationName
step_type ∷ LatticeType
step_type = (discrete data_int `lex` discrete data_int) `lex` op int
step ∷ forall x. Term LatticeType x → Term LatticeType x → Term LatticeType x → Proposition LatticeType x
step n1 n2 w = Proposition _step $ 
  PrimitiveTerm TuplePrimitive 
    [ PrimitiveTerm TuplePrimitive [n1, n2] (discrete data_int `lex` discrete data_int)
    , w ] 
    (op int)

module_ ∷ Module
module_ = Module 
  { dataTypes: Map.empty
  , latticeTypes: Map.empty
  , functionSpecs: Map.empty
  , relations: 
    Map.fromFoldable 
    [ Tuple _distance $ Relation $
        (discrete data_int `lex` discrete data_int) `lex` op int
    ]
  , axioms: Map.empty
  , rules: 
    Map.fromFoldable
    [ 
      -- distance (a, b, v)
      -- step (b, c, w)
      -- ---------------------
      -- distance (a, c, v + w)
      let
        a = Name "a" :: TermName
        b = Name "b" :: TermName
        c = Name "c" :: TermName
        v = Name "v" :: TermName
        w = Name "w" :: TermName
      in
      Tuple _step_distance $
        HypothesisRule 
          { quantifications: make
              [ Left $ UniversalQuantification a node
              , Left $ UniversalQuantification b node
              , Left $ UniversalQuantification v weight ]
          , proposition: distance (var_node a) (var_node b) (var_weight v)
          , filter: Nothing } $ Left $
        HypothesisRule 
          { quantifications: make
              [ Left $ UniversalQuantification b node
              , Left $ UniversalQuantification c node
              , Left $ UniversalQuantification w weight ]
          , proposition: step (var_node b) (var_node c) (var_weight w)
          , filter: Nothing } $ Right $
        distance (var_node a) (var_node c) (add_op_int (var_weight v) (var_weight w)) ]
  , databaseSpecs: Map.empty
  }