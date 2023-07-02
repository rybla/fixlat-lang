module Language.Fixlat.Core.Internal.RuleNormalization where

import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Monad.State (StateT)
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.ModuleT (ModuleT)

normRule :: forall m. MonadEffect m => Rule -> ModuleT m NormRule
normRule (Rule _rule) = do
  let
    go :: G.Rule -> StateT {gammaRev :: Array G.Quantification, sigma :: G.TermSub} (ModuleT m) (G.SymbolicProposition /\ G.Rule)
    -- go (G.FilterRule _ _) = bug $ "A rule cannot have a filter before the first premise: " <> ticks (pretty _rule.originalRule)
    -- go (G.LetRule _ _ _) = bug $ "A rule cannot have a let before the first premise: " <> ticks (pretty _rule.originalRule)
    -- go (G.ConclusionRule _) = bug $ "A rule cannot have 0 premises: " <> ticks (pretty _rule.originalRule)
    -- go (G.FilterRule cond rule) = do
    --   checkCondition (assertI G.concreteTerm $ cond) >>= if _
    --     then ?a 
    --     else ?a
    -- go (G.QuantificationRule quant rule) = do
    --   modify_ \st -> st {gammaRev = quant Array.: st.gammaRev}
    --   go rule
    -- go (G.PremiseRule prop rule) = pure (prop /\ rule)

    go (G.PremiseRule prop rule) = pure (prop /\ rule)
    go _ = hole "TODO"

  pure $ NormRule
    { originalRule: _rule.originalRule
    , gamma: _rule.gamma
    , sigma: hole "sigma"
    , premise: hole "premise" -- _rule.premise
    , rule: hole "rule" }