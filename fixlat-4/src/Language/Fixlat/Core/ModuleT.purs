module Language.Fixlat.Core.ModuleT where

import Language.Fixlat.Core.Grammar
import Prelude

import Control.Monad.Reader (class MonadTrans, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadTrans, StateT)
import Control.Monad.Trans.Class (lift)
import Data.Map as Map

newtype ModuleT m a = ModuleT (ReaderT ModuleContext m a)
runModuleT (ModuleT m) = m

type ModuleContext =
  { dataTypes :: Map.Map TypeName DataType
  , latticeTypes :: Map.Map TypeName LatticeType
  , functionSpecs :: Map.Map FunctionName FunctionSpec
  , relations :: Map.Map RelationName Relation
  , rules :: Map.Map RuleName Rule
  , indexSpecs :: Map.Map IndexSpecName IndexSpec
  }

class MonadModule m where
  getModuleContext :: m ModuleContext

derive newtype instance Functor m => Functor (ModuleT m)
derive newtype instance Applicative m => Applicative (ModuleT m)
derive newtype instance Apply m => Apply (ModuleT m)
derive newtype instance Bind m => Bind (ModuleT m)
derive newtype instance Monad m => Monad (ModuleT m)
derive newtype instance MonadTrans ModuleT

instance (Monad m, MonadTrans t, MonadModule m) => MonadModule (t m) where
  getModuleContext = lift getModuleContext