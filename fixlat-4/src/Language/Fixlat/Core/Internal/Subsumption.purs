module Language.Fixlat.Core.Internal.Subsumption where

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
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.ModuleT (ModuleT)

class Subsumable a where
  subsumes :: forall m. MonadEffect m => a -> a -> GenerateT m Boolean
  isSubsumed :: forall m. MonadEffect m => a -> GenerateT m Boolean

instance Subsumable Patch where
  subsumes _ _ = hole "TODO"
  isSubsumed _ = hole "TODO"

instance Subsumable InstRule where
  subsumes _ _ = hole "TODO"
  isSubsumed _ = hole "TODO"

instance Subsumable NormInstRule where
  subsumes _ _ = hole "TODO"
  isSubsumed _ = hole "TODO"

instance Subsumable ConcreteProposition where
  subsumes _ _ = hole "TODO"
  isSubsumed _ = hole "TODO"

instance Subsumable ConcreteTerm where
  subsumes _ _ = hole "TODO"
  isSubsumed _ = hole "TODO"


