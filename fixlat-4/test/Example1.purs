module Test.Example1 where

{-
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude

import Control.Bug (bug)
import Control.Debug as Debug
import Control.Monad.Reader (runReaderT)
import Data.AlternatingList (AlternatingList(..), (-:))
import Data.Either (Either(..))
import Data.Make (make)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Fixlat.Core.InternalFixpoint (emptyDatabase, fixpoint)
import Language.Fixlat.Core.ModuleT (runModuleT)
import Text.Pretty (pretty)

-- term constructors

lit_int x = ConstructorTerm (IntConstructor x) [] IntLatticeType

-- rel1

rel1 a = Proposition _rel1 a
_rel1 = Name "rel1" :: RelationName
rel1_domain = IntLatticeType
-- axioms
_rel1_axiom1 = Name "rel1_axiom1" :: AxiomName
_rel1_axiom2 = Name "rel1_axiom2" :: AxiomName
-- rules
_rel1_rule1 = Name "rel1_rule1" :: RuleName
_rel1_rule2 = Name "rel1_rule2" :: RuleName
_rel1_rule3 = Name "rel1_rule3" :: RuleName

-- db1

_db1 = Name "db1" :: DatabaseSpecName
_db1_fix1 = Name "db1_fix1" :: FixpointSpecName

-- functions

_fn_add = Name "add" :: FunctionName
fn_add x y = ApplicationTerm _fn_add [x, y] IntLatticeType

_fn_lt = Name "lt" :: FunctionName
fn_lt x y = ApplicationTerm _fn_lt [x, y] BoolLatticeType

-- module

module_ :: Module
module_ = emptyModule # Newtype.over Module _ 
  { relations = Map.fromFoldable 
      [ 
        Tuple _rel1 $ Relation $ rel1_domain
      ]
  , axioms = Map.fromFoldable
      [ 
        Tuple _rel1_axiom1 $ 
          Axiom $ rel1 (lit_int 0)
      ]
  , rules = Map.fromFoldable 
      [
        Tuple _rel1_rule1 $ 
          HypothesisRule 
            { quantifications: make []
            , proposition: rel1 (lit_int 0)
            , filter: Nothing } $ Right $
          rel1 (lit_int 1)
      ,
        Tuple _rel1_rule2 $ 
          HypothesisRule 
            { quantifications: make []
            , proposition: rel1 (lit_int 1)
            , filter: Nothing } $ Right $
          rel1 (lit_int 2)
      ,
        let x = Name "x" :: TermName in
        Tuple _rel1_rule3 $
          HypothesisRule
            { quantifications: make [Left $ UniversalQuantification x rel1_domain]
            , proposition: rel1 (VarTerm x rel1_domain)
            , filter: Just (fn_lt (VarTerm x IntLatticeType) (lit_int 4)) } $ Right $
          rel1 (fn_add (VarTerm x IntLatticeType) (lit_int 1))
      ]
  , databaseSpecs = Map.fromFoldable
      [ Tuple _db1 $ emptyDatabaseSpec # Newtype.over DatabaseSpec _
          { fixpoints = Map.fromFoldable
              [ Tuple _db1_fix1 $ FixpointSpec 
                  { axiomNames: [_rel1_axiom1]
                  , ruleNames: [_rel1_rule3] }
              ]
          }
      ]
  , functionSpecs = Map.fromFoldable
      [ 
        Tuple _fn_add $ FunctionSpec
          { functionType: FunctionType [IntDataType, IntDataType] IntDataType
          , implementation: Just case _ of
              [ConstructorTerm (IntConstructor x) [] lty, ConstructorTerm (IntConstructor y) [] _] -> ConstructorTerm (IntConstructor (x + y)) [] lty
              _ -> bug "invalid arguments to add"
          }
      ,
        Tuple _fn_lt $ FunctionSpec
          { functionType: FunctionType [IntDataType, IntDataType] IntDataType
          , implementation: Just case _ of
              [ConstructorTerm (IntConstructor x) [] _, ConstructorTerm (IntConstructor y) [] _] -> 
                Debug.debug ("fn_lt.implemenation(" <> show x <> ", " <> show y <> ")") \_ -> 
                ConstructorTerm (BoolConstructor (x < y)) [] BoolLatticeType
              _ -> bug "invalid arguments to add"
          }
      ]
  }

main :: Effect Unit
main = do
  Console.log "[Test.Add.main]"
  let db = emptyDatabase
  Console.log $ "\nInput database:\n" <> pretty db <> "\n"
  let ctx = 
        { module_
        , initial_gas: 100 }
  let m = fixpoint db _db1 _db1_fix1
  db' <- runReaderT (runModuleT m) ctx
  Console.log $ "\nOutput database:\n" <> pretty db' <> "\n"
  pure unit
-}