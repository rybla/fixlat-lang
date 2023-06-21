module Language.Fixlat.Core.InternalFixpoint where

import Prelude

import Control.Assert (assertI)
import Control.Assert.Assertions (just)
import Control.Monad.State (StateT, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lattice ((~?))
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Evaluation (evaluate, runEvaluationT)
import Language.Fixlat.Core.Grammar (toSymbolicProposition)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.ModuleT (ModuleT, getModuleCtx)
import Language.Fixlat.Core.Unification (runUnifyT, unifyProposition)
import Type.Proxy (Proxy(..))

-- | Internal fixpoint implementation.
fixpoint :: forall m. MonadEffect m => G.DatabaseSpecName -> G.FixpointSpecName -> ModuleT m Database
fixpoint databaseSpecName fixpointSpecName = do
  -- TODO: initialize everything from input in queue?
  moduleCtx <- getModuleCtx
  let databaseSpec = assertI just $ databaseSpecName `Map.lookup` (unwrap moduleCtx.module).databaseSpecs
  let fixpointSpec = assertI just $ fixpointSpecName `Map.lookup` (unwrap databaseSpec).fixpoints
  hole "fixpoint"

--------------------------------------------------------------------------------
-- FixpointT
--------------------------------------------------------------------------------

type FixpointT m = StateT FixpointEnv (ModuleT m)

liftFixpointT :: forall m a. MonadEffect m => ModuleT m a -> FixpointT m a
liftFixpointT = lift

type FixpointEnv =
  { database :: Database
  , queue :: Queue
  , comparePatch :: Patch -> Patch -> Ordering
  }

_database = Proxy :: Proxy "database"
_queue = Proxy :: Proxy "queue"
_comparePatch = Proxy :: Proxy "comparePatch"

data Patch
  = ApplyPatch
      G.Quantifications -- quantifications before hypothesis proposition
      G.SymbolicProposition -- hypothesis proposition
      (Maybe G.SymbolicTerm) -- filter term (boolean-valued)
      Patch -- conclusion patch
  | PropositionPatch 
      G.ConcreteProposition -- conclusion proposition

substitutePatch :: Map.Map G.TermName G.SymbolicTerm -> Patch -> Patch
substitutePatch sigma patch = do
  hole "substitutePatch"

--------------------------------------------------------------------------------
-- loop
--------------------------------------------------------------------------------

loop :: forall m. MonadEffect m => FixpointT m Unit
loop = do
  -- pop next patch from queue
  pop >>= case _ of
    Nothing -> 
      -- no more patches; done
      pure unit
    Just patch -> do
      -- learn patch, yielding new patches
      learn patch >>= case _ of
        [] -> do
          -- finished learning; done
          pure unit
        patches -> do
          -- insert new patches into queue
          traverse_ insert patches
          -- loop
          loop

--------------------------------------------------------------------------------
-- Queue
--------------------------------------------------------------------------------

data Queue = Queue (List Patch)

-- Remove next items from queue until get one that is not subsumed by current
-- knowledge.
pop :: forall m. MonadEffect m => FixpointT m (Maybe Patch)
pop = do
  Queue queue <- gets _.queue
  case List.uncons queue of
    Nothing -> pure Nothing
    Just {head: patch, tail: queue'} -> do
      modify_ _{queue = Queue queue'}
      isSubsumed patch >>=
        if _ then
          -- patch is subsumed; pop next
          pop
        else 
          -- patch is not subsumed; so is new
          pure (Just patch)

insert :: forall m. MonadEffect m => Patch -> FixpointT m Unit
insert patch = do
  Queue queue <- gets _.queue
  comparePatch <- gets _.comparePatch
  modify_ _{queue = Queue (List.insertBy comparePatch patch queue)}

--------------------------------------------------------------------------------
-- Database
--------------------------------------------------------------------------------

-- | A Database stores all current rules, which includes 
data Database = Database (Array G.ConcreteProposition)

-- | Insert a proposition into an Database, respective subsumption (i.e. removes
-- | propositions in the Database that are subsumed by the new proposition, and
-- | ignores the new proposition if it is already subsumed by propositions in
-- | the Database). If `prop` is subsumed by `database`, then
-- | `insertIntoDatabase prop database = Nothing`
insertIntoDatabase :: forall m. MonadEffect m => G.ConcreteProposition -> FixpointT m Boolean
insertIntoDatabase prop =
  getCandidates >>= go >>= case _ of
    Nothing -> pure false
    Just props' -> do
      -- modify_ _{database = Database (Array.fromFoldable props')}
      pure true
  where
  go = flip Array.foldr (pure (Just Nil)) \prop' m_mb_props' -> do
    m_mb_props' >>= case _ of 
      Nothing -> pure Nothing
      Just props' -> subsumes prop prop' >>= if _
        -- If prop is subsumed by a proposition already in the Database (prop'),
        -- then we don't update the Database (encoded by `Nothing` result)
        then pure Nothing
        -- Otherwise, we will update the Database.
        else subsumes prop' prop >>= if _
          -- If a proposition in the Database (prop') is subsumed by the new prop,
          -- then remove prop' from the Database
          then pure (Just props')
          -- Otherwise, keep the old proposition (prop') in the Database
          else pure (Just (Cons prop' props'))

getCandidates :: forall m. MonadEffect m => FixpointT m (Array (G.ConcreteProposition))
getCandidates = gets _.database <#> \(Database props) -> props

-- Learns `patch` by inserting into `database` anything new derived from the patch,
-- and yields any new `patches` that are derived from the patch.
learn :: forall m. MonadEffect m => Patch -> FixpointT m (Array Patch)
learn (PropositionPatch prop) = do
  void $ insertIntoDatabase prop
  -- Yield any new patches that are applications of rules that use the
  -- newly-derived prop
  moduleCtx <- lift getModuleCtx
  join <<< Array.fromFoldable <<< Map.values <$> (unwrap moduleCtx.module).rules `for` \rule -> do
    applyRule rule (toSymbolicProposition prop) >>= case _ of
      Nothing -> pure []
      Just patch -> pure [patch]
learn (ApplyPatch quantifications expectation mb_condition conclusion) = do
  -- For each candidate proposition in the database
  candidates <- getCandidates
  Array.concat <$> for candidates \candidate -> do
    -- TODO: figure out structure of ctx for unification; maybe don't really need to know quantification types?
    -- let ctx = {quantifiers}
    liftFixpointT (runUnifyT {quantifications} (unifyProposition expectation candidate)) >>= case _ of
      Left _err -> do
        -- not unifiable, so ignore candidate
        pure []
      Right (_ /\ sigma) -> do
        -- apply sigma to condition
        let mb_condition' = mb_condition <#> \condition -> assertI G.concreteTerm $ G.substituteTerm sigma condition
        -- check condition
        check <- do
          case mb_condition' of
            Nothing -> pure true
            Just condition -> checkCondition condition
        if not check then pure [] else do
          -- apply sigma to conclusion
          let conclusion' = substitutePatch sigma conclusion
          -- check subsumption
          isSubsumed conclusion' >>= case _ of
            true -> pure []
            false -> pure [conclusion']

applyRule :: forall m. MonadEffect m => G.Rule -> G.SymbolicProposition -> FixpointT m (Maybe Patch)
applyRule rule prop = do
  -- TODO: unify hypothesis of rule with prop, and yield the patch that is the
  -- rest of the rule
  hole "applyRule"

checkCondition :: forall m. MonadEffect m => G.ConcreteTerm -> FixpointT m Boolean
checkCondition term = do
  term' <- liftFixpointT $ runEvaluationT $ evaluate term
  pure $ term' == G.trueTerm

--------------------------------------------------------------------------------
-- Subsumption
--------------------------------------------------------------------------------

isSubsumed :: forall m. MonadEffect m => Patch -> FixpointT m Boolean
isSubsumed (ApplyPatch _quants _hyp _mb_cond _patch) = do
  -- TODO: should apply-patches be able to be subsumed? maybe not
  pure false
isSubsumed (PropositionPatch prop) = do
  -- This patch is subsumed if `prop` is subsumed by any of the propositions in
  -- the Database.
  props <- getCandidates
  (\f -> Array.foldM f false props) case _ of
    false -> \_ -> pure false 
    true -> \prop' -> subsumes prop' prop

-- | `prop1` subsumes `prop2` if `prop1 >= prop2`.
subsumes :: forall m. MonadEffect m => G.ConcreteProposition -> G.ConcreteProposition -> FixpointT m Boolean
subsumes prop1 prop2 = do
  pure $ (prop1 ~? prop2) == Just GT
