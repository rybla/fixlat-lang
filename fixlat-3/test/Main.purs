module Test.Main where

import Data.Tuple.Nested
import Language.Fixlat.Deriv
import Language.Fixlat.Grammar
import Language.Fixlat.Querying
import Prelude
import Utility

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Data.Either (Either(..))
import Data.LatList (LatList(..))
import Data.LatList as LatList
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Language.Fixlat.Querying as Q
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Text.Pretty (pretty)

{-
main :: Effect Unit
main = do
  let 
    p1 = "P1"
    p2 = "P2"
    p3 = "P3"

  let 
    ctx :: Q.Ctx
    ctx = 
      { mvarQuants: Map.empty
      , mvarSubst: Map.empty
      , preds: Map.fromFoldable
          [ Var p1 /\ Pred {bind: Var p1, label: Label p1, param: intLat}
          , Var p2 /\ Pred {bind: Var p2, label: Label p2, param: intLat}
          , Var p3 /\ Pred {bind: Var p3, label: Label p3, param: intLat}
          ]
      }

    st :: Q.St
    st = 
      { predLatLists: Map.fromFoldable
          [ Var p1 /\ LatList (List.fromFoldable 
              [ -- forall x. p2(x) |- p1(x)
                let x = freshMVar (Just (Var "x")) in
                initDerivs
                  { label: Label "r1"
                  , params: [Param {bind: x, quant: UnivQuant, type_: intLat}]
                  , hyps: Prop {pred: Var p2, arg: VarTerm (pure x) intLat} : Nil
                  , con: Prop {pred: Var p1, arg: VarTerm (pure x) intLat}
                  }
              ])
          , Var p2 /\ LatList (List.fromFoldable 
              [ -- forall x. p3(x) |- p2(x)
                let x = freshMVar (Just (Var "y")) in
                initDerivs 
                  { label: Label "r1"
                  , params: [Param {bind: x, quant: UnivQuant, type_: intLat}]
                  , hyps: Prop {pred: Var p3, arg: VarTerm (pure x) intLat} : Nil
                  , con: Prop {pred: Var p2, arg: VarTerm (pure x) intLat}
                  }
              ])
          , Var p3 /\ LatList (List.fromFoldable 
              [ -- forall x. p3(x)
                let x = freshMVar (Just (Var "z")) in
                initDerivs 
                  { label: Label "r1"
                  , params: [Param {bind: x, quant: UnivQuant, type_: intLat}]
                  , hyps: mempty
                  , con: Prop {pred: Var p3, arg: VarTerm (pure x) intLat}
                  }
              ])
          ]
      }
  let prop = Prop {pred: Var p1, arg: AtomicTerm (NatTerm 0) intLat}
  
  mb_deriv /\ st' <- flip runStateT st >>> flip runReaderT ctx $ queryProp prop
  case mb_deriv of
    Nothing -> pure unit
    Just deriv -> log $ "mb_deriv = Just (" <> pretty deriv <> ")"

  pure unit
-}

main :: Effect Unit
main = unsafePartial do
  let
    -- Nat ::= μ nat . nat + unit
    -- natLat = FixLat (Var "nat") (SumLat (VarLat (Var "nat")) unitLat) :: CLat
    natLat = VarLat (Var "nat") :: CLat
    
    -- zeroTerm = Inj2Term (unitTerm (AtomicLat UnitLat)) natLat
    zeroTerm = VarTerm (Left (Var "0")) natLat

    sucTerm :: forall xt. Term _ xt → Term _ xt
    sucTerm n = Inj1Term n natLat
    
    addProp a b c = Prop {pred: Var "add", arg: prodsTerm ProdLat [a, b, c]}
  let 
    ctx :: Q.Ctx
    ctx = 
      { mvarQuants: Map.empty
      , mvarSubst: Map.empty
      , preds: Map.fromFoldable do
          -- add(<a, b, c>): a + b = c
          [ Var "add" /\ 
            Pred 
              { bind: Var "add"
              , label: Label "add"
              , param: ProdLat natLat (ProdLat natLat natLat)
              }
          ]
      }

    st :: Q.St
    st =
      { gas: 100
      , focusMaybeDeriv: Nothing
      , predLatLists: Map.fromFoldable
          [ Var "add" /\
            ( LatList <<< List.fromFoldable
            $ [ do
                -- add-zero (∀ a : Nat) . a + 0 = a
                let a = freshMVar (Just (Var "a"))
                initDerivs 
                  { label: Label "add-zero"
                  , params: [Param {quant: UnivQuant, bind: a, type_: natLat}]
                  , derivsRev: mempty
                  , hyps: mempty
                  -- , con: Prop {pred: Var "add", arg: prodsTerm ProdLat [VarTerm (pure a) natLat, zeroTerm, VarTerm (pure a) natLat]}
                  , con: addProp (VarTerm (pure a) natLat) zeroTerm (VarTerm (pure a) natLat)
                  }
              , do
                -- add-suc (∀ a b c : Nat) . a + b = c |- a + suc b = suc c
                let a = freshMVar (Just (Var "a"))
                let b = freshMVar (Just (Var "b"))
                let c = freshMVar (Just (Var "c"))
                initDerivs 
                  { label: Label "add-suc"
                  , params:   
                      [ Param {quant: UnivQuant, bind: a, type_: natLat}
                      , Param {quant: UnivQuant, bind: b, type_: natLat}
                      , Param {quant: ExistQuant, bind: c, type_: natLat}
                      ]
                  , derivsRev: mempty
                  , hyps: List.fromFoldable [ 
                      addProp (VarTerm (pure a) natLat) (VarTerm (pure b) natLat) (VarTerm (pure c) natLat)
                    ]
                  , con: 
                      addProp (VarTerm (pure a) natLat) (sucTerm (VarTerm (pure b) natLat)) (sucTerm (VarTerm (pure c) natLat))
                  }
              ]
            )
          ]
      }

  -- exists ?result . a + b = ?result
  let a = sucTerm zeroTerm
  let b = sucTerm (sucTerm (sucTerm zeroTerm))
  let rule1 = Rule do
        let result = freshMVar (Just (Var "result"))
        { label: Label "add-test1"
        , params: [Param {quant: ExistQuant, bind: Right result, type_: natLat}]
        , hyps: []
        , con: Prop {pred: Var "add", arg: prodsTerm ProdLat [a, b, VarTerm (Right result) natLat]}
        }

  void $ flip runStateT st >>> flip runReaderT ctx $ queryInitRule rule1

-- main :: Effect Unit
-- main = unsafePartial do
--   let
--     addProp a b c = Prop {pred: Var "add", arg: prodsTerm ProdLat [a, b, c]}
--   let 
--     ctx :: Q.Ctx
--     ctx = 
--       { mvarQuants: Map.empty
--       , mvarSubst: Map.empty
--       , preds: Map.fromFoldable do
--           []
--       }

--     st :: Q.St
--     st =
--       { gas: 100
--       , focusMaybeDeriv: Nothing
--       , predLatLists: Map.fromFoldable
--           []
--       }

--   let test1 = Rule do
--         let result = freshMVar (Just (Var "result"))
--         { label: Label "test1"
--         , params: []
--         , hyps: []
--         , con: unsafeCrashWith "TODO"
--         }

--   void $ flip runStateT st >>> flip runReaderT ctx $ queryInitRule test1