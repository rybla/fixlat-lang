module Language.Fixlat.Core.Internal.Evaluation where

import Prelude

import Control.Assert (assertI)
import Control.Assert.Assertions (keyOfMap)
import Control.Bug (bug)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Base (GenerateT, TermSub)
import Language.Fixlat.Core.ModuleT (ModuleT, getModuleCtx)
import Text.Pretty (pretty, ticks)

type EvaluateT m a = ReaderT Ctx (ModuleT m) a

type Ctx = 
  { sigma :: TermSub }

class Evaluatable a b | a -> b where
  evaluate' :: forall m. MonadEffect m => a -> EvaluateT m b

evaluate :: forall m a b. MonadEffect m => Evaluatable a b => 
  TermSub -> a -> ModuleT m b
evaluate sigma a = runReaderT (evaluate' a) {sigma}

instance Evaluatable G.ConcreteTerm G.ConcreteTerm where
  evaluate' (G.ApplicationTerm f args _) = do
    moduleCtx <- lift getModuleCtx
    let G.FunctionSpec fun = assertI keyOfMap $ f /\ (unwrap moduleCtx.module_).functionSpecs
    impl <- case fun.implementation of
      Nothing -> bug $ "[evaluate] The function " <> ticks (pretty f) <> " has no internal implementation."
      Just impl -> pure impl
    args' <- evaluate' `traverse` args
    evaluate' (impl args')
  evaluate' (G.ConstructorTerm con args ty) = do
    args' <- evaluate' `traverse` args
    pure $ G.ConstructorTerm con args' ty
  evaluate' (G.QuantTerm quant _) = absurd quant
  evaluate' (G.BoundTerm name _) = do
    sigma <- asks _.sigma
    case List.lookup name sigma of
      Nothing -> bug $ "[evaluate] The variable " <> ticks (pretty name) <> " is unbound."
      Just term -> evaluate' term


