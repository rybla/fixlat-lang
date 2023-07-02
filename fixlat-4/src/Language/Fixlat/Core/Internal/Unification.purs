module Language.Fixlat.Core.Internal.Unification where

import Control.Monad.Trans.Class
import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Language.Fixlat.Core.Internal.Base
import Prelude
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Map as Map
import Hole (hole)
import Language.Fixlat.Core.ModuleT (ModuleT)

--------------------------------------------------------------------------------
-- UnifyT
--------------------------------------------------------------------------------

type UnifyT m a = ReaderT Ctx (ExceptT String (StateT Env (ModuleT m))) a

liftUnifyT :: forall m a. Monad m => ModuleT m a -> UnifyT m a
liftUnifyT = lift >>> lift >>> lift

type Ctx = 
  { originMessage :: String
  , gamma :: QuantCtx }

type Env = TermSub

class Unifiable x y | x -> y where
  unify :: forall m. Monad m => x -> y -> UnifyT m Unit

instance Unifiable SymbolicProposition ConcreteProposition where
  unify expected actual = hole "unifyProposition"

instance Unifiable SymbolicTerm ConcreteTerm where
  unify expected actual = hole "unifyTerm"

{-
import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude

import Control.Assert (assert)
import Control.Assert.Assertions (equal)
import Control.Bug (bug)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (snd, uncurry)
import Hole (hole)
import Language.Fixlat.Core.ModuleT (ModuleT)
import Text.Pretty (pretty, ticks)

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
  { initial :: (SymbolicProposition /\ ConcreteProposition) \/ (SymbolicTerm /\ ConcreteTerm) }

type TypingContext = Map.Map TermName Type

unify :: forall m. Monad m => (SymbolicProposition /\ ConcreteProposition) \/ (SymbolicTerm /\ ConcreteTerm) -> ModuleT m (String \/ Sub)
unify initial = map snd <$> runUnifyT {initial} do
  case initial of
    Left (expected /\ actual) -> unifyProposition expected actual
    Right (expected /\ actual) -> unifyTerm expected actual

runUnifyT :: forall m a. Monad m => Ctx -> UnifyT m a -> ModuleT m (String \/ (a /\ Sub))
runUnifyT ctx m = runStateT (runExceptT (runReaderT m ctx)) Map.empty >>= case _ of
  Left err /\ sigma -> pure $ Left $ 
    "Unification error: " <> err <> "\n\n" <>
    "Current substitution:" <>
    String.joinWith "\n"
      (Map.toUnfoldable sigma <#> 
        \(name /\ term) -> "\n  • " <> pretty name <> " := " <> pretty term)
  Right a /\ sigma -> pure $ Right $ a /\ sigma

--------------------------------------------------------------------------------
-- Unification algorithm
--------------------------------------------------------------------------------

-- | Unify expectation (symbolic, as it could be satisfied with many candidates)
-- | with a candidate.
unifyProposition :: forall m. Monad m => SymbolicProposition -> ConcreteProposition -> UnifyT m Unit
unifyProposition (Proposition rel1 arg1) (Proposition rel2 arg2) 
  | rel1 /= rel2 = throwError $ "Cannot unify propositions of different relations"
  | otherwise = unifyTerm arg1 arg2

-- | Unify expectation (symbolic, as it couldbe satisfied with many candidates)
-- | with candidate.
unifyTerm :: forall m. Monad m => SymbolicTerm -> ConcreteTerm -> UnifyT m Unit
unifyTerm term1 term2 | typeOfTerm term1 /= typeOfTerm term2 = do
  ctx <- ask
  bug $ "In order to unify a symbolic term with a concrete term, they must have the same type. But, got expected type " <> ticks (pretty (typeOfTerm term1)) <> " and actual type " <> ticks (pretty (typeOfTerm term2)) <> ".\nWhile unifying " <> case ctx.initial of
    Left (expected /\ actual) -> ticks (pretty expected) <> " with " <> ticks (pretty actual)
    Right (expected /\ actual) -> ticks (pretty expected) <> " with " <> ticks (pretty actual)
unifyTerm _ (ApplicationTerm _ _ _) = bug $ "In order to unify a symbolic term with a concrete term, the concrete term must be fully simplified."
unifyTerm _ (QuantTerm x _) = absurd x
unifyTerm (QuantTerm x1 _) term2 = addSubstitution x1 term2
unifyTerm (ConstructorTerm p1 args1 _) (ConstructorTerm p2 args2 _) | p1 == p2 =
  for_ (args1 `Array.zip` args2) (uncurry unifyTerm)
unifyTerm term1 term2 = throwError $ "Cannot unify " <> ticks (pretty term1) <> " with " <> ticks (pretty term2) <> "."

addSubstitution :: forall m. Monad m => TermName -> ConcreteTerm -> UnifyT m Unit
addSubstitution name term = do
  sigma <- get
  case Map.lookup name sigma of
    Just term' -> unifyTerm (toSymbolicTerm term) term'
    Nothing -> modify_ $ Map.insert name term
-}