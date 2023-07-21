module Language.Mfl.Generate.Internal.Normalization where

import Data.Either.Nested
import Data.Tuple.Nested
import Language.Mfl.Core.Ast
import Prelude

import Control.Monad.Computation (ComputationT, asks, liftComputationT, local, throwError)
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Language.Mfl.Core.ModuleT (ModuleT)
import Language.Mfl.Generate.Internal.Base (NormRule(..), QuantCtx, TermSub)
import Language.Mfl.Generate.Internal.Evaluation (concretizeProp, concretizeTerm, evaluateProp, evaluateTerm, runEvaluationT)
import Record as R
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))

type NormalizationT m = ComputationT "Normalization" NormalizationCtx NormalizationEnv NormalizationErr (ModuleT m)

type NormalizationCtx =
  { gamma :: QuantCtx
  , sigma :: TermSub }

type NormalizationEnv = {}

type NormalizationErr = String

-- | Yields Nothing if cannot be normalized.
normalizeRule :: forall m. Monad m => RuleName -> Rule -> NormalizationT m (Maybe (NormRule \/ EvaluatedProp))

normalizeRule ruleName (FilterRule t rule) = do
  t' <- evaluateConcretizeTerm "normalizeRule" t
  if t' == trueTerm
      then normalizeRule ruleName rule
      else pure Nothing

normalizeRule ruleName (QuantRule quant rule) =
  local
    (R.modify (Proxy :: Proxy "gamma") (List.Cons quant))
    (normalizeRule ruleName rule)

normalizeRule ruleName (LetRule x t rule) = do
  t' <- evaluateConcretizeTerm "normalizeRule" t
  local
    (R.modify (Proxy :: Proxy "sigma") (List.Cons (x /\ t')))
    (normalizeRule ruleName rule)

normalizeRule ruleName (PremiseRule p rule) = do
  gamma <- asks _.gamma
  sigma <- asks _.sigma
  p' <- evaluateConcretizeProp "normalizeRule" p
  pure (Just (Left (NormRule {ruleName, gamma, sigma, premise: p', rule})))

normalizeRule _ruleName (ConclusionRule p) = do
  p' <- evaluateConcretizeProp "normalizeRule" p
  pure (Just (Right p'))

evaluateConcretizeTerm source t = do
  sigma <- asks _.sigma
  liftComputationT (runEvaluationT {sigma} ((evaluateTerm <=< concretizeTerm) t)) >>= case _ of
    Left err /\ _env -> throwError (source <> ": " <> err)
    Right t' /\ _env -> pure t'

evaluateConcretizeProp source p = do
  sigma <- asks _.sigma
  liftComputationT (runEvaluationT {sigma} ((evaluateProp <=< concretizeProp) p)) >>= case _ of
    Left err /\ _env -> throwError (source <> ": " <> err)
    Right p' /\ _env -> pure p'