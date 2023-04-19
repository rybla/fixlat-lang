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
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set.Lattice as Lattice
import Partial.Unsafe (unsafeCrashWith)

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
  , mvars :: Map.Map Var Quant
  }

data Quant = UnivQuant | ExistQuant

-- | Querying state:
-- |   - `predSets`: map of predicate name to lattice set of known rules that can
-- |     produce instances of that predicate
type St = 
  { predSets :: Map.Map Var (Lattice.Set CRule)
  }

-- !TODO can make this simpler by instead just allowing the querying of
-- predicates -- then dont even need to define lattice over rules, right??

-- queryRule :: forall m. MonadQuery m => CRule -> m (Maybe (Lattice.Set CRule))
-- queryRule rule = do
--   let predVar = getRuleConPredVar rule 
--   gets (_.predSets >>> Map.lookup predVar) >>= case _ of 
--     -- no known rules that can produce the conclusion of the rule
--     Nothing -> pure Nothing
--     Just set -> do
--       -- find the max set of known rules that can produce the conclusion of the
--       -- rule
--       case Lattice.maxSet rule set of
--         -- no such rules found
--         Nothing -> pure Nothing
--         Just maxSet -> do
--           -- if any of these rules have no hypotheses, then done
--           -- otherwise, need to query the hypotheses of each of these rules
--           ?a

-- -- | Yield expanded lattice set of rules by trying to use each old rule as a
-- -- | hypothesis of the new rule, and the new rule as a hypothesis of each old
-- -- | rule.
-- applyRule :: forall m. MonadQuery m => CRule -> Lattice.Set CRule -> m (Lattice.Set CRule)
-- applyRule _ _ = unsafeCrashWith "TODO"

-- learnRule :: forall m. MonadQuery m => MRule -> m Unit
-- learnRule mr = modify_ \st -> st { predSets = Map. }

queryProp :: forall m. MonadQuery m => MProp -> m Boolean
queryProp prop = do
  gets (_.predSets >>> Map.lookup prop.name) >>= case _ of 
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
          let mrule = generalizeRule rule
          -- specialize rule to expected conclusion
          case specializeRule prop mrule of 
            Nothing -> pure false
            Just mrule' -> do
              -- for each hyp
              isProven <- (\f -> foldM f false (unwrap mrule').hyps) case _ of
                -- already failed to a previous hyp query
                false -> \_ -> pure false 
                -- have succeeded in each previous hyp query, so query this hyp
                true -> queryProp
              
              -- when isProven do
              --   ?a -- !TODO add to knowledge
              ?a
                

-- | Replace the rule's parameters with fresh metavariables.
generalizeRule :: CRule -> MRule
generalizeRule _ = unsafeCrashWith "TODO"

-- unifyRules :: MRule -> MRule -> MRule
-- unifyRules mr1 mr2 = unsafeCrashWith "TODO"

-- | An expected prop unifies with a candidate prop if there is a substitution of
-- metavariables in the candidate prop that makes it equal to the expected prop
unifyProps :: forall m. 
  MonadQuery m => MonadState (Map.Map MVar MProp) m =>
  MProp -> MProp -> m (Maybe MProp)
unifyProps mpExpected mpCandidate = unsafeCrashWith "TODO"

specializeRule :: MProp -> MRule -> Maybe MRule
specializeRule expectedMProp mrule = unsafeCrashWith "TODO"

