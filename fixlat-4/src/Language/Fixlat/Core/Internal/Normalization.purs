module Language.Fixlat.Core.Internal.Normalization where

import Data.Either.Nested
import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Assert (assertI)
import Control.Bug (bug)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT, gets, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.List as List
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Evaluation (evaluate)
import Record as R
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))

class Normalize (a :: Type) (b :: Type) | a -> b where
  normalize :: forall m. MonadEffect m => a -> GenerateT m b

instance Normalize InstRule (String \/ NormInstRule) where
  normalize :: forall m. MonadEffect m => InstRule -> (GenerateT m) (String \/ NormInstRule)
  normalize (InstRule _rule) = do
    let ctx = {gamma: _rule.gamma, sigma: _rule.sigma}
    runExceptT (runStateT (go _rule.rule) ctx) >>= case _ of
      Left err -> pure $ Left err
      Right ({premise, rule} /\ {gamma, sigma}) -> pure $ Right $ NormInstRule {originalRule: _rule.originalRule, gamma, sigma, premise, rule}
    where
      _gamma = Proxy :: Proxy "gamma"
      _sigma = Proxy :: Proxy "sigma"

      go :: 
        G.Rule ->
        StateT {gamma :: QuantCtx, sigma :: TermSub} (ExceptT String (GenerateT m))
          {premise :: G.SymbolicProposition, rule :: G.Rule}
      go (G.FilterRule cond rule) = do
        -- sigma <- gets _.sigma
        cond'<- lift <<< lift $ evaluate $ assertI G.concreteTerm cond
        if not (cond' == G.trueTerm) 
          then throwError $ "Condition failed: " <> pretty cond'
          else go rule
      go (G.QuantificationRule quant rule) = do
        modify_ $ R.modify _gamma (List.Cons quant)
        go rule
      go (G.LetRule name term rule) = do
        let term' = assertI G.concreteTerm term
        modify_ $ R.modify _sigma (List.Cons (name /\ term'))
        go rule
      go (G.PremiseRule premise rule) = pure {premise, rule}
      go (G.ConclusionRule _prop) = bug "[normalize] Cannot normalize a ConclusionRule"

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
