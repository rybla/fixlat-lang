module Language.Fixlat.Core.Internal.Evaluation where

import Prelude
import Utility

import Control.Assert (assertI)
import Control.Assert.Assertions (keyOfMap)
import Control.Bug (bug)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Class (lift)
import Data.Foldable as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Base (GenerateT, TermSub, liftGenerateT)
import Language.Fixlat.Core.ModuleT (ModuleT, getModuleCtx)
import Text.Pretty (pretty, ticks)

class Evaluatable a where
  -- | Shallow evaluation -- `Just` an atomic step of evaluation, or `Nothing`
  -- | if fully evaluated.
  evaluate' :: forall m. MonadEffect m => a -> GenerateT m (Maybe a)

shallowEvaluate :: forall m a. MonadEffect m => Evaluatable a => a -> GenerateT m a
shallowEvaluate a = evaluate' a >>= case _ of 
  Nothing -> pure a
  Just a' -> pure a'

-- | Fixpoint of evaluate'.
evaluate :: forall m a. MonadEffect m => Evaluatable a => a -> GenerateT m a
evaluate a = evaluate' a >>= case _ of
  Nothing -> pure a
  Just a' -> evaluate a'

instance Evaluatable G.ConcreteTerm where
  -- | An atomic step of evaluation yields a `G.ConstructorTerm`.
  evaluate' (G.ApplicationTerm f args _) = do
    moduleCtx <- liftGenerateT getModuleCtx
    let G.FunctionSpec fun = assertI keyOfMap $ f /\ (unwrap moduleCtx.module_).functionSpecs
    impl <- case fun.implementation of
      Nothing -> bug $ "[evaluate'] The function " <> ticks (pretty f) <> " has no internal implementation."
      Just impl -> pure impl
    args' <- evaluate `traverse` args
    evaluate' (impl args')
  evaluate' (G.ConstructorTerm _ _ _) = pure Nothing
  evaluate' (G.BoundTerm name _) = do
    sigma <- asks _.sigma
    case List.lookup name sigma of
      Nothing -> bug $ "[evaluate'] The variable " <> ticks (pretty name) <> " is unbound."
      Just term -> evaluate' term
  evaluate' (G.QuantTerm quant _) = absurd quant

instance Evaluatable G.SymbolicTerm where
  -- | An atomic step of evaluation yields a `G.ConstructorTerm` or a
  -- | `G.QuantTerm`.
  evaluate' (G.ApplicationTerm f args _) = do
    moduleCtx <- liftGenerateT getModuleCtx
    let G.FunctionSpec fun = assertI keyOfMap $ f /\ (unwrap moduleCtx.module_).functionSpecs
    impl <- case fun.implementation of
      Nothing -> bug $ "[evaluate'] The function " <> ticks (pretty f) <> " has no internal implementation."
      Just impl -> pure impl
    args' <- map (assertI G.concreteTerm) <$> evaluate `traverse` args
    map G.toSymbolicTerm <$> evaluate' (impl args')
  evaluate' (G.ConstructorTerm _ _ _) = pure Nothing
  evaluate' (G.BoundTerm name _) = do
    sigma <- asks _.sigma
    case List.lookup name sigma of
      Nothing -> bug $ "[evaluate'] The variable " <> ticks (pretty name) <> " is unbound."
      Just term -> G.toSymbolicTerm <$$> evaluate' term
  evaluate' (G.QuantTerm _ _) = pure Nothing
