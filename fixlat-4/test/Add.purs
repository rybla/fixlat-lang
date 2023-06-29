module Test.Add where

import Language.Fixlat.Core.Grammar
import Prelude

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

_add = Name "add" :: RelationName
  
_add_zero_zero = Name "add_zero_zero" :: AxiomName
_add_n_suc = Name "add_n_suc" :: RuleName

_db_add_zero = Name "db_add_zero" :: DatabaseSpecName
_fix_main = Name "fix_main" :: FixpointSpecName

zero = PrimitiveTerm ZeroPrimitive [] nat
suc a = PrimitiveTerm SucPrimitive [a] nat

lex = TupleLatticeType LexicographicTupleOrdering

-- tuple2 :: forall ty x. Term ty x -> Term ty x -> ty -> Term ty x
tuple2 a b = PrimitiveTerm TuplePrimitive [a, b]

-- tuple3 :: forall x. LatticeTerm x -> LatticeTerm x -> LatticeTerm x -> LatticeTerm x
tuple3 a b c = tuple2 a (tuple2 b c (nat `lex` nat)) (nat `lex` (nat `lex` nat))

namedNat x = NamedTerm x nat

nat = NatLatticeType

add :: forall ty x. Term ty x -> Proposition ty x
add a = Proposition _add a

module_ :: Module
module_ = emptyModule # Newtype.over Module _ 
  { relations = Map.fromFoldable 
      [ Tuple _add $ Relation $ 
          nat `lex` (nat `lex` nat)
      ] 
  , axioms = Map.fromFoldable
      [ 
        -- ------------------------
        --   zero + zero = zero
        Tuple _add_zero_zero $
        Axiom $ add (tuple3 (suc zero) zero zero)
      ]
  , rules = Map.fromFoldable 
      [ 
        --   forall x y z.
        --   x + y = z
        -- ------------------------
        --   x + suc y = suc z
        let
          x = Name "x"
          y = Name "y"
          z = Name "z"
        in
        Tuple _add_n_suc $
        -- HypothesisRule
        --   { -- forall (x y z : Nat)
        --     quantifications: make
        --         [ Left $ UniversalQuantification x nat
        --         , Left $ UniversalQuantification y nat
        --         , Left $ UniversalQuantification z nat ]
        --     -- x + y = z
        --   , proposition: add (tuple3 (namedNat x) (namedNat y) (namedNat z))
        --   , filter: Nothing } $
          -- ------------------------
          -- x + suc y = suc z
        --   Right $ add (tuple3 (namedNat x) (suc (namedNat y)) (suc (namedNat z)))
        
        -- forall (x y z : Nat)
        QuantificationRule (Left (UniversalQuantification x nat)) $
        QuantificationRule (Left (UniversalQuantification y nat)) $
        QuantificationRule (Left (UniversalQuantification z nat)) $
        PremiseRule (add (tuple3 (namedNat x) (namedNat y) (namedNat z))) $
        -- ------------------------
        -- x + suc y = suc z
        ConclusionRule (add (tuple3 (namedNat x) (suc (namedNat y)) (suc (namedNat z))))
      ]
  , databaseSpecs = Map.fromFoldable
      [ Tuple _db_add_zero $ emptyDatabaseSpec # Newtype.over DatabaseSpec _
          { fixpoints = Map.fromFoldable
              [ Tuple _fix_main $ FixpointSpec
                  { axiomNames: [_add_zero_zero]
                  , ruleNames: [_add_n_suc]
                  }
              ]
          } 
      ]
  }

main :: Effect Unit
main = do
  Console.log "[Test.Add.main]"
  let db = emptyDatabase
  Console.log $ "Input database:\n\n" <> show db <> "\n\n"
  let ctx = 
        { module_
        , initial_gas: 10 }
  let m = do
        fixpoint db _db_add_zero _fix_main
  db' <- runReaderT (runModuleT m) ctx
  Console.log $ "Output database:\n\n" <> show db' <> "\n\n"
  pure unit
