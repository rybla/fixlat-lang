module Language.Fixlat.Core.Evaluation where

import Language.Fixlat.Core.Grammar
import Prelude

import Control.Monad.Reader (ReaderT)
import Hole (hole)
import Language.Fixlat.Core.ModuleT (ModuleT)

type EvaluationT m = ReaderT Env (ModuleT m)

type Env = {}

evaluate :: forall ty m. Term ty -> EvaluationT m Term
evaluate = hole "evaluate"
