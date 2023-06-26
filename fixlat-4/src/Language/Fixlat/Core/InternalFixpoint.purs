module Language.Fixlat.Core.InternalFixpoint where

import Prelude

import Control.Assert (assert, assertI)
import Control.Assert.Assertions (just, keyOfMap)
import Control.Bug (bug)
import Control.Debug as Debug
import Control.Monad.State (StateT, execStateT, gets, modify, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Lattice ((~?))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar (Axiom(..), Proposition)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.ModuleT (ModuleT, getModuleCtx)
import Language.Fixlat.Core.Unification (unify)
import Text.Pretty (class Pretty, bullets, indent, pretty, ticks, (<+>))
import Type.Proxy (Proxy(..))

_INITIAL_GAS = 20

-- | Internal fixpoint implementation.
fixpoint :: forall m. MonadEffect m => Database -> G.DatabaseSpecName -> G.FixpointSpecName -> ModuleT m Database
fixpoint (Database props) databaseSpecName fixpointSpecName = do
  Debug.debugA "[fixpoint] start"
  moduleCtx <- getModuleCtx
  let databaseSpec = assertI keyOfMap $ databaseSpecName /\ (unwrap moduleCtx.module_).databaseSpecs
  let fixpointSpec = assertI keyOfMap $ fixpointSpecName /\ (unwrap databaseSpec).fixpoints

  let axioms = (unwrap fixpointSpec).axiomNames <#> \axiomName -> 
        assertI keyOfMap (axiomName /\ (unwrap moduleCtx.module_).axioms)
  let props' = props <> (axioms <#> \(Axiom prop) -> prop)

  -- Initialize queue with patches that conclude with each prop in the database
  -- i.e. everything starts off as out-of-date.
  let queue = Queue (List.fromFoldable (ConclusionPatch <$> props'))
  Debug.debugA $ "[fixpoint] initial queue:" <> pretty queue

  let env = 
        { gas: _INITIAL_GAS
        , database: Database []
        , rules: Map.filterWithKey 
            (\ruleName _ -> ruleName `Array.elem` (unwrap fixpointSpec).ruleNames)
            (unwrap moduleCtx.module_).rules
        , queue
        -- TODO: this isn't right, but not sure how to do ordering yet, so this will
        -- just ensure that the queue is first-in-first-out
        , comparePatch: \_ _ -> GT }

  Debug.debugA $ "[fixpoint] initial env.rules:" <> pretty env.rules

  env' <- execStateT loop env

  Debug.debugA "[fixpoint] end"
  pure env'.database

--------------------------------------------------------------------------------
-- FixpointT
--------------------------------------------------------------------------------

type FixpointT m = StateT FixpointEnv (ModuleT m)

liftFixpointT :: forall m a. MonadEffect m => ModuleT m a -> FixpointT m a
liftFixpointT = lift

type FixpointEnv =
  { gas :: Int
  , database :: Database
  , rules :: Map.Map G.RuleName G.Rule
  , queue :: Queue
  , comparePatch :: Patch -> Patch -> Ordering
  }

_database = Proxy :: Proxy "database"
_queue = Proxy :: Proxy "queue"
_comparePatch = Proxy :: Proxy "comparePatch"

data Patch
  = ApplyPatch G.Rule
  | ConclusionPatch G.ConcreteProposition -- conclusion proposition

derive instance Generic Patch _
instance Show Patch where show x = genericShow x

instance Pretty Patch where
  pretty (ApplyPatch rule) = pretty rule
  pretty (ConclusionPatch prop) = pretty prop

substitutePatch :: Map.Map G.TermName G.SymbolicTerm -> Patch -> Patch
substitutePatch sigma (ConclusionPatch prop) = ConclusionPatch (assertI G.concreteProposition (G.substituteProposition sigma (G.toSymbolicProposition prop)))
substitutePatch sigma (ApplyPatch rule) = ApplyPatch (G.substituteRule sigma rule)

--------------------------------------------------------------------------------
-- loop
--------------------------------------------------------------------------------

loop :: forall m. MonadEffect m => FixpointT m Unit
loop = do
  -- db <- gets _.database
  -- Debug.debugA $ "[fixpoint] database:" <> pretty db

  -- queue <- gets _.queue
  -- Debug.debugA $ "[fixpoint] queue:" <> pretty queue

  gas <- _.gas <$> modify \env -> env {gas = env.gas - 1}
  if gas <= 0 then bug "[loop] out of gas" else do
    pop >>= case _ of
      Nothing -> 
        -- no more patches; done
        pure unit
      Just patch -> do
        Debug.debugA $ "[loop] patch:\n" <> indent (pretty patch)
        -- learn patch, yielding new patches
        patches <- learn patch
        -- insert new patches into queue
        traverse_ insert patches
        -- loop
        loop

--------------------------------------------------------------------------------
-- Queue
--------------------------------------------------------------------------------

data Queue = Queue (List Patch)

derive instance Generic Queue _
instance Show Queue where show x = genericShow x

instance Pretty Queue where
  pretty (Queue queue) = bullets (Array.fromFoldable (pretty <$> queue))

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

derive instance Generic Database _
instance Show Database where show x = genericShow x

instance Pretty Database where
  pretty (Database props) = bullets (Array.fromFoldable (pretty <$> props))

emptyDatabase :: Database
emptyDatabase = Database []

-- | Insert a proposition into an Database, respective subsumption (i.e. removes
-- | propositions in the Database that are subsumed by the new proposition, and
-- | ignores the new proposition if it is already subsumed by propositions in
-- | the Database). If `prop` is subsumed by `database`, then
-- | `insertIntoDatabase prop database = Nothing`
insertIntoDatabase :: forall m. MonadEffect m => G.ConcreteProposition -> FixpointT m Boolean
insertIntoDatabase prop = do
  -- Debug.debugA $ "[insertIntoDatabase] prop:" <+> pretty prop
  getPropositions >>= go >>= case _ of
    Nothing -> pure false
    Just props' -> do
      modify_ _{database = Database (Array.fromFoldable (prop : props'))}
      pure true
  where
  go = flip Array.foldr (pure (Just Nil)) \prop' m_mb_props' -> do
    m_mb_props' >>= case _ of 
      Nothing -> pure Nothing
      Just props' -> subsumes prop' prop >>= if _
        -- If prop is subsumed by a proposition already in the Database (prop'),
        -- then we don't update the Database (encoded by `Nothing` result)
        then do
          pure Nothing
        -- Otherwise, we will update the Database.
        else subsumes prop prop' >>= if _
          -- If a proposition in the Database (prop') is subsumed by the new
          -- prop, then remove prop' from the Database
          then do
            pure (Just props')
          -- Otherwise, keep the old proposition (prop') in the Database
          else do
            pure (Just (Cons prop' props'))

getPropositions :: forall m. MonadEffect m => FixpointT m (Array (G.ConcreteProposition))
getPropositions = gets _.database <#> \(Database props) -> props

-- TODO: take into account e.g. out-of-date-ness
getCandidates :: forall m. MonadEffect m => FixpointT m (Array (G.ConcreteProposition))
getCandidates = gets _.database <#> \(Database props) -> props

-- Learns `patch` by inserting into `database` anything new derived from the patch,
-- and yields any new `patches` that are derived from the patch.
learn :: forall m. MonadEffect m => Patch -> FixpointT m (Array Patch)
learn (ConclusionPatch _prop) = do
  prop <- evaluateProposition _prop
  void $ insertIntoDatabase prop
  -- Yield any new patches that are applications of rules that use the
  -- newly-derived prop
  rules <- gets _.rules
  join <<< Array.fromFoldable <<< Map.values <$> rules `for` \rule -> do
    applyRule rule prop
-- learn (ApplyPatch quantifications expectation mb_condition conclusion) = do
learn (ApplyPatch rule) = do
  -- For each candidate proposition in the database
  candidates <- getCandidates
  Array.concat <$> for candidates \candidate -> do
    applyRule rule candidate

appleRuleAsPatch :: forall m. MonadEffect m => G.Rule -> FixpointT m Patch
appleRuleAsPatch rule = pure $ ApplyPatch rule

applyRule :: forall m. MonadEffect m => G.Rule -> G.ConcreteProposition -> FixpointT m (Array Patch)
applyRule (G.HypothesisRule hyp conc) prop = do
  -- TODO: unify hypothesis of rule with prop, and yield the patch that is the
  -- rest of the rule
  -- liftFixpointT (runUnifyT ctx (unifyProposition hyp.proposition prop)) >>= case _ of
  liftFixpointT (unify hyp.quantifications (Left (hyp.proposition /\ prop))) >>= case _ of
    Left _err -> do
      -- not unifiable, so ignore candidate
      pure []
    Right _sigma -> do
      let sigma = G.toSymbolicTerm <$> _sigma
      -- apply sigma to condition
      let filter' = hyp.filter <#> \condition -> assertI G.concreteTerm $ G.substituteTerm sigma condition
      -- check condition
      check <- do
        case filter' of
          Nothing -> pure true
          Just condition -> checkCondition condition
      if not check 
        then pure [] 
        else do
          let patch = case conc of
                Right prop' -> ConclusionPatch $ assertI G.concreteProposition $ G.substituteProposition sigma prop'
                Left rule' -> ApplyPatch $ G.substituteRule sigma rule'
          let conclusion' = substitutePatch sigma patch
          -- check subsumption
          isSubsumed conclusion' >>= case _ of
            true -> pure []
            false -> pure [conclusion']

checkCondition :: forall m. MonadEffect m => G.ConcreteTerm -> FixpointT m Boolean
checkCondition term = do
  term' <- evaluateTerm term
  let success = term' == G.trueTerm
  pure success

--------------------------------------------------------------------------------
-- Subsumption
--------------------------------------------------------------------------------

isSubsumed :: forall m. MonadEffect m => Patch -> FixpointT m Boolean
isSubsumed (ApplyPatch _rule) = do
  -- TODO: should apply-patches be able to be subsumed? maybe not?
  pure false
isSubsumed (ConclusionPatch prop) = do
  -- This patch is subsumed if `prop` is subsumed by any of the propositions in
  -- the Database.
  props <- getCandidates
  (\f -> Array.foldM f false props) case _ of
    false -> \_ -> pure false 
    true -> \prop' -> subsumes prop' prop

-- | `prop1` subsumes `prop2` if `prop1 >= prop2`.
subsumes :: forall m. MonadEffect m => G.ConcreteProposition -> G.ConcreteProposition -> FixpointT m Boolean
subsumes prop1 prop2 = do
  -- let result = (prop1 ~? prop2) == Just GT
  -- Debug.debugA $ "[subsumes] " <> pretty prop1 <> "  >=?  " <> pretty prop2 <> "  ==>  " <> pretty result
  -- pure result
  pure $ (prop1 ~? prop2) == Just GT

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

evaluateProposition :: forall m. MonadEffect m => G.ConcreteProposition -> FixpointT m G.ConcreteProposition
evaluateProposition (G.Proposition rel a) = do
  a' <- evaluateTerm a
  pure $ G.Proposition rel a'

evaluateTerm :: forall m. MonadEffect m => G.ConcreteTerm -> FixpointT m G.ConcreteTerm
evaluateTerm (G.NamedTerm x _) = absurd x
evaluateTerm (G.NeutralTerm funName args _) = do
  moduleCtx <- lift getModuleCtx
  let G.FunctionSpec funSpec = assertI keyOfMap $ funName /\ (unwrap moduleCtx.module_).functionSpecs
  case funSpec.implementation of
    Nothing -> bug $ "[evaluateTerm]: function has no internal implementation: " <> ticks (pretty funName)
    Just impl -> do
      args' <- evaluateTerm `traverse` args
      let term' = impl args'
      evaluateTerm term'
evaluateTerm (G.PrimitiveTerm prim args ty) = do
  args' <- evaluateTerm `traverse` args
  pure $ G.PrimitiveTerm prim args' ty
