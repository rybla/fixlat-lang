module Language.Fixlat.Core.ModuleT where

import Prelude

import Language.Fixlat.Core.Grammar
import Control.Monad.Reader (Reader, ReaderT(..), ask, runReader)
import Control.Monad.Trans.Class (class MonadTrans)
import Type.Proxy (Proxy(..))

newtype ModuleT (m :: Type -> Type) a = ModuleT (ReaderT ModuleCtx m a)

runModuleT (ModuleT m) = m

type ModuleCtx =
  { module_ :: Module
  , initialGas :: Int }

_dataTypes = Proxy :: Proxy "dataTypes"
_latticeTypes = Proxy :: Proxy "latticeTypes"
_functionSpecs = Proxy :: Proxy "functionSpecs"
_relations = Proxy :: Proxy "relations"
_rules = Proxy :: Proxy "rules"
_indexSpecs = Proxy :: Proxy "indexSpecs"

class Monad m <= MonadModule m where
  moduleT :: forall a. Reader ModuleCtx a -> m a

derive newtype instance Functor m => Functor (ModuleT m)
derive newtype instance Applicative m => Applicative (ModuleT m)
derive newtype instance Apply m => Apply (ModuleT m)
derive newtype instance Bind m => Bind (ModuleT m)
derive newtype instance Monad m => Monad (ModuleT m)
derive newtype instance MonadTrans ModuleT

instance Monad m => MonadModule (ModuleT m) where
  moduleT r = ModuleT (ReaderT (pure <<< runReader r))

getModuleCtx = moduleT ask