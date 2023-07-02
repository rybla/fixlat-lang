module Language.Fixlat.Core.Internal.Evaluation where

import Prelude

import Control.Monad.Reader (ReaderT)
import Effect.Class (class MonadEffect)
import Language.Fixlat.Core.Grammar (TermSub)
import Language.Fixlat.Core.Internal.Base (GenerateT)

type EvaluateT m a = ReaderT Ctx (GenerateT m) a

type Ctx = 
  { sigma :: TermSub }

class Evaluatable a where
  evaluate :: forall m. MonadEffect m => a -> EvaluateT m a
