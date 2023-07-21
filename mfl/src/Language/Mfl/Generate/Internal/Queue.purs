module Language.Mfl.Generate.Internal.Queue where

import Language.Mfl.Generate.Internal.Base
import Prelude

import Data.Maybe (Maybe)
import Hole (hole)

insert :: forall m. Monad m => Patch -> GenerateT m Unit
insert = hole "insert"

pop :: forall m. Monad m => GenerateT m (Maybe Patch)
pop = hole "pop"
