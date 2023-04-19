-- - !TODO the metavariables in the queried prop are actually _existentially_
--   quantified, not universally. importantly, existentially quantified
--   metavariables can be specified when unifying with a candidate goal's
--   conclusion

module Language.Fixlat.Querying where

import Data.Either.Nested
import Language.Fixlat.Grammar
import Prelude
import Prim hiding (Type)

import Control.Monad.List.Trans (ListT)
import Control.Monad.Reader (class MonadReader, ReaderT, asks)
import Control.Monad.State (class MonadState, State, StateT, gets, modify_, runState)
import Control.MonadPlus (class MonadPlus)
import Data.Foldable (foldM, foldl, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (unwrap)
import Data.Set.Lattice as Lattice
import Partial.Unsafe (unsafeCrashWith)
import Utility (comps, if_)

class 
  ( Monad m
  , MonadReader Ctx m
  , MonadState St m
  , MonadPlus m
  ) <= MonadQuery m

-- | Querying context:
-- |   - `preds`: map of predicate name to predicate declaration
-- |   - `rules`: map of predicate name to array of rules that each can produce
-- |     an instance of that predicate
type Ctx = 
  { preds :: Map.Map Var Pred
  , rules :: Map.Map Var (Array CRule)
  , mvarQuants :: Map.Map MVar Quant
  , mvarSubst :: Map.Map MVar MTerm
  }

data Quant = UnivQuant | ExistQuant

-- | Querying state:
-- |   - `predSets`: map of predicate name to lattice set of known rules that
-- |     can produce instances of that predicate
type St = 
  { predSets :: Map.Map Var (Lattice.Set CRule)
  }

-- | Generalize a rule by replacing its parameters with fresh metavariables, and
-- | localizing quantification info.
generalizeRule :: forall m a. MonadQuery m => CRule -> (MRule -> m a) -> m a
generalizeRule _ _ = 
  unsafeCrashWith "TODO"

specializeRule :: forall m a. MonadQuery m => MRule -> m CRule
specializeRule _ = 
  unsafeCrashWith "TODO"

learnRule :: forall m a. MonadQuery m => CRule -> m a -> m a
learnRule _rule _ = 
  unsafeCrashWith "TODO"

-- | An expected prop unifies with a candidate prop if there is a substitution
-- | of uni-mvars in the candidate prop and a substitution of ex-mvars in the
-- | expected prop that make them syntactically equal.
-- |
-- | Special cases:
-- | - !TODO Always defer to substituting the candidate for the expected?
-- |   - unifying an expected ex-mvar with a candidate uni-mvar
-- |   - unifying an expected ex-mvar with a candidate ex-mvar
-- |   - unifying an expected uni-mvar with a candidate ex-mvar
-- |   - unifying an expected uni-mvar with a candidate uni-mvar
unifyProps :: forall m a. MonadQuery m => MProp -> MProp -> (MProp -> m a) -> m (Maybe a)
unifyProps _mpropExpected _mpropCandidate _k = 
  unsafeCrashWith "TODO"

-- | Attempt to apply a rule to a proposition. Continuation on successs.
applyRule :: forall m a. MonadQuery m => MRule -> MProp -> (CRule -> m a) -> m (Maybe a)
applyRule _rule _prop _onSuccess = unsafeCrashWith "TODO"

-- | Attempt to prove a rule. Continuation on successs.
queryRule :: forall m a. MonadQuery m => MRule -> m a -> m (Maybe a)
queryRule mrule _onSuccess = do
  -- -- learn the hypotheses
  -- join $ foldM
  --   (\k hyp -> do
  --     -- learn hyp
  --     pure (\b -> k ?a)
  --   )
  --   (if_ 
  --     (goProp (unwrap mrule).con) 
  --     (pure Nothing))
  --   (unwrap mrule).hyps
  --   <*> pure true
  unsafeCrashWith "TODO"
  where
  goProp :: MProp -> m (Maybe a)
  goProp _mprop = unsafeCrashWith "TODO"

{-
--
-- !TODO old
--

queryProp' :: forall m. MonadQuery m => MProp -> m Boolean
queryProp' prop = do
  gets (_.predSets >>> Map.lookup prop.bind) >>= case _ of 
    -- there are no known rules that can produce an instance of this
    -- proposition's predicate
    Nothing -> pure false
    -- there is a known set of rules that can produce an instance of this
    -- proposition's predicate
    Just set_cp -> do
      -- for each rule
      (\f -> foldM f false set_cp) case _ of 
        -- already found a good rule
        true -> \_ -> pure true
        false -> \rule -> do
          -- generalize rule
          -- let mrule = generalizeRule rule
          -- -- specialize rule to expected conclusion
          -- case specializeRule prop mrule of 
          --   Nothing -> pure false
          --   Just mrule' -> do
          --     -- for each hyp
          --     isProven <- (\f -> foldM f false (unwrap mrule').hyps) case _ of
          --       -- already failed to a previous hyp query
          --       false -> \_ -> pure false 
          --       -- have succeeded in each previous hyp query, so query this hyp
          --       true -> queryProp'
              
          --     -- when isProven do
          --     --   ?a -- !TODO add to knowledge
          --     ?a
          unsafeCrashWith "TODO"

-- | Yield expanded lattice set of rules by trying to use each old rule as a
-- | hypothesis of the new rule, and the new rule as a hypothesis of each old
-- | rule.
applyRule :: forall m. MonadQuery m => CRule -> Lattice.Set CRule -> m (Lattice.Set CRule)
applyRule _ _ = unsafeCrashWith "TODO"

learnRule :: forall m. MonadQuery m => MRule -> m Unit
learnRule mr = modify_ \st -> st { predSets = Map. }
-}
