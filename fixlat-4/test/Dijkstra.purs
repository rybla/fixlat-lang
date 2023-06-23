module Test.Dijkstra where

import Data.Tuple
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude

import Data.AlternatingList (AlternatingList(..), (-:), (:-))
import Data.Map as Map
import Data.Set as Set

{-
module_ ∷ Module
module_ = Module 
  { dataTypes: Map.empty
  , latticeTypes: Map.empty
  , functionSpecs: Map.empty
  , relations: 
    let
      lex = LexicographicTupleType
      discrete = DiscreteLatticeType
      op = OpLatticeType
      int = IntLatticeType
    in
    Map.fromFoldable 
    [ Tuple (Name "distance") $ Relation $
        (discrete int `lex` discrete int) `lex` op int
    ]
  , rules: 
    Map.fromFoldable
    [ -- dist (a, b, v)
      -- dist (b, c, w)
      -- ---------------------
      -- dist ()
      Tuple (Name "step") $ 
        HypothesisRule 
        { quantifications: Quantifications $
            Set.fromFoldable [] -: 
            AlternatingNil
        , proposition: ?a 
        , filter: ?a 
        , conclusion: ?a } ]
  , databaseSpecs: Map.empty
  }
-}