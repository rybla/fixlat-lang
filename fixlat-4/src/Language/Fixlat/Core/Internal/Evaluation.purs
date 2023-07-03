module Language.Fixlat.Core.Internal.Evaluation where

import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar as G

type EvaluateT m a = ReaderT Ctx (GenerateT m) a

type Ctx = 
  { sigma :: TermSub }

class Evaluatable a b | a -> b where
  evaluate' :: forall m. MonadEffect m => a -> EvaluateT m b

evaluate :: forall m a b. MonadEffect m => Evaluatable a b => 
  TermSub -> a -> GenerateT m b
evaluate sigma a = runReaderT (evaluate' a) {sigma}

instance Evaluatable G.ConcreteTerm G.ConcreteTerm where
  evaluate' = hole "evaluate ConcreteTerm"
