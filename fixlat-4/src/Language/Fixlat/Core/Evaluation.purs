module Language.Fixlat.Core.Evaluation where

import Language.Fixlat.Core.Grammar
import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.ModuleT (ModuleT)

type EvaluationT m a = ReaderT Env (ModuleT m) a

type Env = {}

runEvaluationT :: forall m a. Monad m => EvaluationT m a -> ModuleT m a
runEvaluationT m = runReaderT m defaultEnv

defaultEnv :: Env
defaultEnv = hole "defaultEnv"

-- | Evaluate a term, including functions.
evaluate :: forall m. Monad m => MonadEffect m => ConcreteTerm -> EvaluationT m ConcreteTerm
evaluate = hole "evaluate"
