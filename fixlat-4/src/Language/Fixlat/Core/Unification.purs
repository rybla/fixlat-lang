module Language.Fixlat.Core.Unification where

import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude

import Control.Monad.Except (ExceptT, lift, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Hole (hole)
import Language.Fixlat.Core.ModuleT (ModuleT)

--------------------------------------------------------------------------------
-- UnifyT
--------------------------------------------------------------------------------

type UnifyT m a = ReaderT Ctx (ExceptT String (StateT Sub (ModuleT m))) a

liftUnifyT :: forall m a. Monad m => ModuleT m a -> UnifyT m a
liftUnifyT = lift >>> lift >>> lift

-- !TODO actually the quantifiers shouldn't exactly be a map, since the order of
-- universal and existential quantifiers matters. Not exactly sure what data
-- structure to use
type Ctx = 
  { quantifiers :: Array Quantification }

type Sub = Map.Map TermName (Term LatticeType)

runUnifyT :: forall m a. Monad m => Ctx -> UnifyT m a -> ModuleT m (String \/ (a /\ Sub))
runUnifyT ctx m = runStateT (runExceptT (runReaderT m ctx)) Map.empty >>= case _ of
  Left err /\ sigma -> pure $ Left $ 
    "Unification error: " <> err <> "\n\n" <>
    "Current substitution:" <>
    Array.intercalate "\n" 
      (Map.toUnfoldable sigma <#> 
        \(name /\ term) -> "\n  • " <> show name <> " := " <> show term)
  Right a /\ sigma -> pure $ Right $ a /\ sigma

--------------------------------------------------------------------------------
-- Unification algorithm
--------------------------------------------------------------------------------

-- | Unify expectation with candidate
unifyProposition :: forall m. Monad m => Proposition LatticeType -> Proposition LatticeType -> UnifyT m Unit
unifyProposition = hole "unify"

-- | Unify expectation with candidate
unifyTerm :: forall m. Monad m => Term LatticeType -> Term LatticeType -> UnifyT m Unit
unifyTerm = hole "unify"

