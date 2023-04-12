{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Fixlat.Querying (submitQuery) where

import Control.Category
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Lattice
import qualified Data.Map as Map
import Language.Fixlat.Grammar
import qualified Language.Fixlat.M as M
import qualified ListT

-- | Querying monad.
type QueryingM = ReaderT QueryingCtx (ListT.ListT (StateT QueryingEnv (ExceptT String M.M)))

data QueryingCtx = QueryingCtx
  { ctxPredicates :: Map.Map PredName (Predicate (Type Lat)),
    ctxRules :: Map.Map PredName (Rule Lat (Type Lat))
  }

-- add all preds
-- add all rules
initQueryingCtx :: Module Lat (Type Lat) -> M.M QueryingCtx
initQueryingCtx = error "!TODO initQueryingCtx"

data QueryingEnv = QueryingEnv
  { envKnowledges :: Map.Map PredName Knowledge -- pred name => pred knowledge
  }

-- init knowledge for each pred
initQueryingEnv :: Module Lat (Type Lat) -> M.M QueryingEnv
initQueryingEnv = error "!TODO initQueryingEnv"

lookupPredicateKnowledge :: PredName -> QueryingM Knowledge
lookupPredicateKnowledge x =
  gets (envKnowledges >>> Map.lookup x) >>= \case
    Nothing -> throwError $ "unknown predicate name:" ++ show x
    Just knw -> return knw

-- | For a particular predicate, a `Knowledge` is an index over rules that can
-- produce instances of that predicate. A new rule can be inserted, and all
-- rules that match a given rule can be queried. This index is aware of the
-- lattice structure over those rules, and ensures the following properties:
-- - `r <= lookup (insert r knw)`
data Knowledge

insertRule :: Rule Lat (Type Lat) -> Knowledge -> Knowledge
insertRule = error "!TODO insertKnowledge"

-- | Lookup all rules that can (immediately) infer this rule.
lookupRuleRules :: Knowledge -> Rule Lat (Type Lat) -> [Rule Lat (Type Lat)]
lookupRuleRules = error "!TODO lookupKnowledge"

-- Lookup all rules that can (immediately) infer this prop.
lookupPropRules :: Knowledge -> Prop (Type Lat) -> [Rule Lat (Type Lat)]
lookupPropRules = error "!TODO lookupProp"

-- | Submitting a query.
submitQuery :: Module Lat (Type Lat) -> Rule Lat (Type Lat) -> M.M (Either String [Proof])
submitQuery mdl r = do
  env <- initQueryingEnv mdl
  ctx <- initQueryingCtx mdl
  let m0 = queryRule r
  let m1 = runReaderT m0 ctx
  let m2 = ListT.toList m1
  let m3 = evalStateT m2 env
  let m4 = runExceptT m3
  m4

-- | Query a rule.
queryRule :: Rule Lat (Type Lat) -> QueryingM Proof
queryRule Rule {..} = do
  -- !TODO handle ruleHyps
  queryProp ruleCon

-- | Query a proposition.
queryProp :: Prop (Type Lat) -> QueryingM Proof
-- look for any rules in the knowledge of this predicate that can infer the goal
-- prop; for each of these rules, query their hypotheses for those proofs, then
-- use the fully instantiated rule as a step in the proof
queryProp prop@(PredProp x _) = do
  kwn <- lookupPredicateKnowledge x
  let rs = lookupPropRules kwn prop
  pfs <- forM rs \r -> do
    -- !TODO handle r's params
    hypProofs <- queryProp `traverse` ruleHyps r
    return $ Proof r hypProofs
  lift $ ListT.fromFoldable pfs
