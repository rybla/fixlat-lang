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
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Language.Fixlat.Querying as Q
import Text.Pretty (pretty)

main :: Effect Unit
main = do
  log "🍝"

  let p1 = "P1"

  let 
    ctx :: Q.Ctx
    ctx = 
      { mvarQuants: Map.empty
      , mvarSubst: Map.empty
      , preds: Map.fromFoldable
          [ Var p1 /\ Pred {bind: Var p1, label: Label p1, param: AtomicLat NatLat}
          ]
      }

    st :: Q.St
    st = 
      { predLatLists: Map.fromFoldable
          [ Var p1 /\ LatList (List.fromFoldable 
              [ Derivs [Deriv 
                  let x = freshMVar Nothing in
                  { label: Label "R1"
                  , params: [Param {bind: x, quant: UnivQuant, type_: AtomicLat NatLat}]
                  , derivsRev: mempty
                  , hyps: mempty
                  , con: Prop {pred: Var p1, arg: VarTerm x (AtomicLat NatLat)}
                  }]
              ])
          ]
      }
  let prop = Prop {pred: Var p1, arg: AtomicTerm (NatTerm 0) (AtomicLat NatLat)}
  
  mb_deriv /\ st' <- flip runStateT st >>> flip runReaderT ctx $ queryProp prop
  case mb_deriv of
    Nothing -> pure unit
    Just deriv -> log $ "mb_deriv = Just (" <> pretty deriv <> ")"
  -- log $ "st' = " <> pretty st'

  pure unit

