module Language.Fixlat.Core.Internal.Unification where

import Control.Monad.Trans.Class
import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Bug (bug)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, gets, modify_, runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (lookup, traverse_)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (uncurry)
import Hole (hole)
import Text.Pretty (class Pretty, indent, pretty, ticks, (<\>))

--------------------------------------------------------------------------------
-- UnifyT
--------------------------------------------------------------------------------

type UnifyT m a = ExceptT String (StateT Env (GenerateT m)) a

unify :: forall m x y. Monad m => Unifiable x y => Pretty x => Pretty y => 
  x -> y -> GenerateT m (String \/ Env)
unify x y = runUnifyT (unify' x y) >>= case _ of
  sigma /\ Left err -> pure $ Left $
    "Unification error when attempting to unify " <> ticks (pretty x) <> " with " <> ticks (pretty y) <> ":\n\n" <>
    indent err
  sigma /\ Right _ -> pure $ Right sigma

runUnifyT :: forall m a. Monad m => UnifyT m a -> GenerateT m (Env /\ (String \/ a))
runUnifyT m = runStateT (runExceptT m) emptyTermSub >>= case _ of
  Left err /\ sigma -> pure $
    (sigma /\ _) $
    Left $
      "Unification error: " <> err <> "\n\n" <>
      "Current substitution:" <\> indent (pretty sigma)
  Right a /\ sigma -> pure $
    sigma /\ Right a

liftUnifyT :: forall m a. Monad m => GenerateT m a -> UnifyT m a
liftUnifyT = lift >>> lift

type Env = TermSub

class Unifiable x y | x -> y where
  unify' :: forall m. Monad m => x -> y -> UnifyT m Unit

instance Unifiable SymbolicProposition ConcreteProposition where
  unify' prop1@(Proposition rel1 arg1) prop2@(Proposition rel2 arg2)
    | rel1 /= rel2 = throwError $ "Cannot unify propositions of different relations: " <> ticks (pretty prop1) <> " and " <> ticks (pretty prop2) <> "."
    | otherwise = unify' arg1 arg2

instance Unifiable SymbolicTerm ConcreteTerm where

  unify' (BoundTerm name _) actual = do
    term <- gets (lookup name) >>= case _ of
      Nothing -> bug $ "[unify] Unbound term name " <> ticks (pretty name) <> "."
      Just term -> pure term
    unify' (toSymbolicTerm term) actual

  unify' expected (BoundTerm name _) = do
    term <- gets (lookup name) >>= case _ of
      Nothing -> bug $ "[unify] Unbound term name " <> ticks (pretty name) <> "."
      Just term -> pure term
    unify' expected term

  unify' term@(ApplicationTerm _ _ _) _ = bug $ "[unify] The expected term should not have unevaluated applications: " <> ticks (pretty term)

  unify' _ term@(ApplicationTerm _ _ _) = bug $ "[unify] The actual term should not have unevaluated applications: " <> ticks (pretty term)

  unify' term1@(ConstructorTerm con1 args1 _) term2@(ConstructorTerm con2 args2 _)
    | con1 /= con2 = throwError $ "Cannot unify terms of different constructors: " <> ticks (pretty term1) <> " and " <> ticks (pretty term2) <> "."
    | otherwise = uncurry unify' `traverse_` (args1 `Array.zip` args2)

  unify' (QuantTerm name _) actual =
    gets (lookup name) >>= case _ of
      Nothing -> modify_ $ List.Cons (name /\ actual)
      Just term -> unify' (toSymbolicTerm term) actual

  unify' term1 term2 = throwError $ "Cannot unify terms: " <> ticks (pretty term1) <> " and " <> ticks (pretty term2) <> "."
