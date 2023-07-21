module Language.Mfl.Core.ModuleT where

import Language.Mfl.Core.Ast
import Prelude

import Control.Monad.Computation (ComputationT, asks, throwError)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Hole (hole)
import Text.Pretty (pretty, ticks)

type ModuleT = ComputationT "Module" ModuleCtx ModuleEnv ModuleErr

type ModuleCtx = 
  { module_ :: Module
  , initialGas :: Int }

type ModuleEnv = {}

type ModuleErr = String

getDataType :: forall m. Monad m => TypeName -> ModuleT m DataType
getDataType k = asks (_.module_ >>> unwrap >>> _.dataTypes >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getDataType: Unknown type name: " <> ticks (pretty k))
  Just v -> pure v

getLatType :: forall m. Monad m => TypeName -> ModuleT m LatType
getLatType k = asks (_.module_ >>> unwrap >>> _.latTypes >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getLatType: Unknown type name: " <> ticks (pretty k))
  Just v -> pure v

getFunctionSpec :: forall m. Monad m => FunctionName -> ModuleT m FunctionSpec
getFunctionSpec k = asks (_.module_ >>> unwrap >>> _.functionSpecs >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getFunctionSpec: Unknown function name: " <> ticks (pretty k))
  Just v -> pure v

getRelation :: forall m. Monad m => RelationName -> ModuleT m Relation
getRelation k = asks (_.module_ >>> unwrap >>> _.relations >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getRelation: Unknown relation name: " <> ticks (pretty k))
  Just v -> pure v

getRule :: forall m. Monad m => RuleName -> ModuleT m Rule
getRule k = asks (_.module_ >>> unwrap >>> _.rules >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getRule: Unknown rule name: " <> ticks (pretty k))
  Just v -> pure v

getAxiom :: forall m. Monad m => AxiomName -> ModuleT m Axiom
getAxiom k = asks (_.module_ >>> unwrap >>> _.axioms >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getAxiom: Unknown axiom name: " <> ticks (pretty k))
  Just v -> pure v

getFixpointSpec :: forall m. Monad m => FixpointSpecName -> ModuleT m FixpointSpec
getFixpointSpec k = asks (_.module_ >>> unwrap >>> _.fixpoints >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getFixpointSpec: Unknown fixpoint name: " <> ticks (pretty k))
  Just v -> pure v

getQuerySpec :: forall m. Monad m => QuerySpecName -> ModuleT m QuerySpec
getQuerySpec k = asks (_.module_ >>> unwrap >>> _.queries >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getQuerySpec: Unknown query name: " <> ticks (pretty k))
  Just v -> pure v

getInsertionSpec :: forall m. Monad m => InsertionSpecName -> ModuleT m InsertionSpec
getInsertionSpec k = asks (_.module_ >>> unwrap >>> _.insertions >>> Map.lookup k) >>= case _ of
  Nothing -> throwError ("getInsertionSpec: Unknown insertion name: " <> ticks (pretty k))
  Just v -> pure v
