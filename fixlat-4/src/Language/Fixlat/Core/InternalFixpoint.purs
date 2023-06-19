module Language.Fixlat.Core.InternalFixpoint where

import Data.Either.Nested
import Data.Tuple.Nested
import Prelude

import Control.Monad.State (class MonadTrans, StateT, gets, lift, modify, modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lattice ((~?))
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Evaluation (evaluate, runEvaluationT)
import Language.Fixlat.Core.Grammar (trueTerm)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.ModuleT (ModuleT)
import Language.Fixlat.Core.Unification (runUnifyT, unifyProposition)
import Record as R
import Type.Proxy (Proxy(..))

-- | Internal fixpoint implementation.
fixpoint :: forall m. MonadEffect m => G.IndexSpecName -> G.FixpointSpecName -> ModuleT m Index
fixpoint indexSpecname fixpointSpecname = hole "fixpoint"
-- TODO: initialize everything from input in queue?

--------------------------------------------------------------------------------
-- FixpointT
--------------------------------------------------------------------------------

type FixpointT m = StateT FixpointEnv (ModuleT m)

liftFixpointT :: forall m a. MonadEffect m => ModuleT m a -> FixpointT m a
liftFixpointT = hole "liftFixpointT"

type FixpointEnv =
  { index :: Index
  , queue :: Queue
  , comparePatch :: Patch -> Patch -> Ordering
  }

_index = Proxy :: Proxy "index"
_queue = Proxy :: Proxy "queue"
_comparePatch = Proxy :: Proxy "comparePatch"

type Patch = Patch_ G.TermName
data Patch_ x 
  = ApplyPatch
      (Array G.Quantification) -- quantifications before hypothesis proposition
      (G.Proposition_ x G.LatticeType) -- hypothesis proposition
      (Maybe (G.Term_ x G.LatticeType)) -- filter term (boolean-valued)
      Patch -- conclusion patch
  | PropositionPatch 
      (G.Proposition_ x G.LatticeType) -- conclusion proposition

substitutePatch :: Map.Map G.TermName (G.TermLattice) -> Patch -> Patch
substitutePatch = hole "substitutePatch"

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
-- Index
--------------------------------------------------------------------------------

-- | An index stores all current rules, which includes 
data Index = Index (Array (G.Proposition G.LatticeType))

-- | Insert a proposition into an index, respective subsumption (i.e. removes
-- | propositions in the index that are subsumed by the new proposition, and
-- | ignores the new proposition if it is already subsumed by propositions in
-- | the index). If `prop` is subsumed by `index`, then `insertIntoIndex prop
-- | index = Nothing`
insertIntoIndex :: forall m. MonadEffect m => G.Proposition G.LatticeType -> FixpointT m Boolean
insertIntoIndex prop = do
  props <- getCandidates
  go props >>= case _ of
    Nothing -> pure false
    Just props' -> do
      modify_ _{index = Index (Array.fromFoldable props')}
      pure true
  where
  go = flip Array.foldr (pure (Just Nil)) \prop' m_mb_props' -> do
    m_mb_props' >>= case _ of 
      Nothing -> pure Nothing
      Just props' -> subsumes prop prop' >>= if _
        -- If prop is subsumed by a proposition already in the index (prop'),
        -- then we don't update the index (encoded by `Nothing` result)
        then pure Nothing
        -- Otherwise, we will update the index.
        else subsumes prop' prop >>= if _
          -- If a proposition in the index (prop') is subsumed by the new prop,
          -- then remove prop' from the index
          then pure (Just props')
          -- Otherwise, keep the old proposition (prop') in the index
          else pure (Just (Cons prop' props'))

getCandidates :: forall m. MonadEffect m => FixpointT m (Array (G.Proposition G.LatticeType))
getCandidates = gets _.index <#> \(Index props) -> props

-- Learns `patch` by inserting into `index` anything new derived from the patch,
-- and yields any new `patches` that are derived from the patch.
learn :: forall m. MonadEffect m => Patch -> FixpointT m (Array Patch)
learn (PropositionPatch prop) = do
  void $ insertIntoIndex prop
  pure []
learn (ApplyPatch quantifiers expectation mb_condition conclusion) = do
  -- For each candidate proposition in the index
  candidates <- getCandidates
  Array.concat <$> for candidates \candidate -> do
    let ctx = {quantifiers}
    liftFixpointT (runUnifyT ctx (unifyProposition expectation candidate)) >>= case _ of
      Left _err -> do
        -- not unifiable, so ignore candidate
        pure []
      Right (_ /\ sigma) -> do
        -- apply sigma to condition
        let mb_condition' = mb_condition <#> \condition -> G.substituteTerm sigma condition
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

checkCondition :: forall m. MonadEffect m => G.TermLattice -> FixpointT m Boolean
checkCondition term = do
  term' <- liftFixpointT $ runEvaluationT $ evaluate term
  pure $ term' == trueTerm

--------------------------------------------------------------------------------
-- Subsumption
--------------------------------------------------------------------------------

isSubsumed :: forall m. MonadEffect m => Patch -> FixpointT m Boolean
isSubsumed (ApplyPatch quants hyp mb_cond patch) = hole "isSubsumed ApplyPatch"
isSubsumed (PropositionPatch prop) = do
  props <- getCandidates
  (\f -> Array.foldM f false props) case _ of
    false -> \_ -> pure false 
    true -> \prop' -> subsumes prop' prop

-- | `prop1` subsumes `prop2` if `prop1 >= prop2`.
subsumes :: forall m. MonadEffect m => G.Proposition G.LatticeType -> G.Proposition G.LatticeType -> FixpointT m Boolean
subsumes prop1 prop2 = pure $ (prop1 ~? prop2) == Just GT
