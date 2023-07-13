module Language.Fixlat.Core.Internal.Normalization where

import Data.Either.Nested
import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Assert (assertI)
import Control.Bug (bug)
import Control.Debug (debugA)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, asks, local, mapReaderT)
import Control.Monad.State (StateT, gets, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List as List
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar (Axiom(..), ConcreteProposition)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Evaluation (evaluate, evaluateConcreteProposition, evaluateSymbolicProposition)
import Record as R
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))

class Normalize (a :: Type) (b :: Type) | a -> b where
  normalize :: forall m. MonadEffect m => a -> GenerateT m b

instance Normalize InstRule (String \/ (NormInstRule \/ ConcreteProposition)) where
  normalize :: forall m. MonadEffect m => InstRule -> (GenerateT m) (String \/ (NormInstRule \/ ConcreteProposition))
  normalize (InstRule _rule) = do
    debugA $ "normalize\n" <> pretty _rule.rule
    local (R.modify _sigma (_rule.sigma <> _)) do
      let ctx = {gamma: _rule.gamma}
      runExceptT (runStateT (go _rule.rule) ctx) >>= case _ of
        Left err -> pure $ Left err
        Right (Left {premise, rule} /\ {gamma}) -> do
          sigma <- asks _.sigma
          pure $ Right $ Left $ NormInstRule {originalRule: _rule.originalRule, gamma, sigma, premise, rule}
        -- TODO: maybe `gamma` should also be in the GenerateT context?
        Right (Right prop /\ {gamma}) -> do
          prop' <- evaluateConcreteProposition prop
          pure $ Right $ Right prop'
    where
      _gamma = Proxy :: Proxy "gamma"
      _sigma = Proxy :: Proxy "sigma"

      go :: 
        G.Rule ->
        StateT {gamma :: QuantCtx} (ExceptT String (GenerateT m))
          ({premise :: G.SymbolicProposition, rule :: G.Rule} \/ ConcreteProposition)
      go (G.FilterRule cond rule) = do
        cond' <- lift <<< lift $ evaluate cond
        if not (cond' == G.trueTerm) 
          then throwError $ "Condition failed: " <> pretty cond'
          else go rule
      go (G.QuantificationRule quant rule) = do
        modify_ $ R.modify _gamma (List.Cons quant)
        go rule
      go (G.LetRule name term rule) = do
        let term' = assertI G.concreteTerm term
        local (R.modify _sigma (List.Cons (name /\ term'))) do
          go rule
      go (G.PremiseRule premise rule) = pure $ Left {premise, rule}
      go (G.ConclusionRule prop) = do
        debugA $ "[normalize.go] G.ConclusionRule"
        Right <<< assertI G.concreteProposition <$> 
          (lift <<< lift) (evaluateSymbolicProposition prop)

-- | Normalize a term by evaluating all applications (which requires evaluating
-- | the applications' arguments), but do not otherwise perform inline
-- | substitution.
instance Normalize G.ConcreteTerm G.ConcreteTerm where
  normalize (G.ConstructorTerm con args ty) = do
    args' <- traverse normalize args
    pure $ G.ConstructorTerm con args' ty
  normalize term@(G.ApplicationTerm _ _ _) = evaluate term
  normalize term@(G.BoundTerm _ _) = pure term
  normalize (G.QuantTerm x _) = absurd x
