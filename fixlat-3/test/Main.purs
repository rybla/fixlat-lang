module Test.Main where

import Data.Tuple.Nested
import Language.Fixlat.Deriv
import Language.Fixlat.Grammar
import Language.Fixlat.MVar
import Language.Fixlat.Querying
import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Data.LatList (LatList(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Language.Fixlat.Querying as Q
import Text.Pretty (pretty)

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
          [ Var p1 /\ Pred {bind: Var p1, label: Label p1, param: AtomicLat NatLat}
          , Var p2 /\ Pred {bind: Var p2, label: Label p2, param: AtomicLat NatLat}
          , Var p3 /\ Pred {bind: Var p3, label: Label p3, param: AtomicLat NatLat}
          ]
      }

    st :: Q.St
    st = 
      { predLatLists: Map.fromFoldable
          [ Var p1 /\ LatList (List.fromFoldable 
              [ -- forall x. p2(x) |- p1(x)
                Derivs [Deriv 
                  let x = freshMVar Nothing in
                  { label: Label "r1"
                  , sigma: mempty
                  , params: [Param {bind: x, quant: UnivQuant, type_: AtomicLat NatLat}]
                  , derivsRev: mempty
                  , hyps: Prop {pred: Var p2, arg: VarTerm x (AtomicLat NatLat)} : Nil
                  , con: Prop {pred: Var p1, arg: VarTerm x (AtomicLat NatLat)}
                  }]
              ])
          , Var p2 /\ LatList (List.fromFoldable 
              [ -- forall x. p3(x) |- p2(x)
                Derivs [Deriv 
                  let x = freshMVar Nothing in
                  { label: Label "r2"
                  , sigma: mempty
                  , params: [Param {bind: x, quant: UnivQuant, type_: AtomicLat NatLat}]
                  , derivsRev: mempty
                  , hyps: Prop {pred: Var p3, arg: VarTerm x (AtomicLat NatLat)} : Nil
                  , con: Prop {pred: Var p2, arg: VarTerm x (AtomicLat NatLat)}
                  }]
              ])
          , Var p3 /\ LatList (List.fromFoldable 
              [ -- forall x. p3(x)
                Derivs [Deriv 
                  let x = freshMVar Nothing in
                  { label: Label "r3"
                  , sigma: mempty
                  , params: [Param {bind: x, quant: UnivQuant, type_: AtomicLat NatLat}]
                  , derivsRev: mempty
                  , hyps: Nil
                  , con: Prop {pred: Var p3, arg: VarTerm x (AtomicLat NatLat)}
                  }]
              ])
          ]
      }
  let prop = Prop {pred: Var p1, arg: AtomicTerm (NatTerm 0) (AtomicLat NatLat)}
  
  mb_deriv /\ st' <- flip runStateT st >>> flip runReaderT ctx $ queryProp prop
  case mb_deriv of
    Nothing -> pure unit
    Just deriv -> log $ "mb_deriv = Just (" <> pretty deriv <> ")"

  pure unit

