module Language.Fixlat.M where

import Effect
import Prelude
import Control.Monad.Reader
import Control.Monad.State

-- | The M monad.
type M
  = StateT Env (ReaderT Ctx Effect)

type Ctx
  = {}

type Env
  = {}
