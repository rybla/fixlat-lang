module Language.Fixlat.Core.InternalFixpoint 
  ( Database(..), emptyDatabase
  , fixpoint )
where

import Data.Tuple.Nested
import Prelude

import Control.Assert (assert, assertI)
import Control.Assert.Assertions (just, keyOfMap)
import Control.Bug (bug)
import Control.Debug as Debug
import Control.Monad.Reader (ask)
import Control.Monad.State (State, StateT, execStateT, gets, modify, modify_, runState)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Lattice ((~?))
import Data.List (List(..), (:))
import Data.List as List
import Data.Make (class Make, make)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (curry)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar (Axiom(..), Proposition)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.ModuleT (ModuleT, getModuleCtx)
-- import Language.Fixlat.Core.Unification (unify)
import Record as R
import Text.Pretty (class Pretty, bullets, indent, pretty, ticks, (<+>), (<\>))
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

newtype Rule = Rule
  { originalRule :: G.Rule
  , quantifications :: Array G.Quantification
  , sigma :: G.TermSub
  , rule :: G.Rule }

instance Make Rule G.Rule where
  make originalRule = Rule
    { originalRule
    , quantifications: []
    , sigma: Map.empty
    , rule: originalRule }

--------------------------------------------------------------------------------
-- NormRule
--------------------------------------------------------------------------------

newtype NormRule = NormRule
  { originalRule :: G.Rule
  , quantifications :: Array G.Quantification
  , sigma :: G.TermSub
  , premise :: G.SymbolicProposition
  , rule :: G.Rule }

normRule :: forall m. MonadEffect m => Rule -> ModuleT m NormRule
normRule (Rule _rule) = do
  let
    go :: G.Rule -> StateT {quantificationsRev :: Array G.Quantification, sigma :: G.TermSub} (ModuleT m) (G.SymbolicProposition /\ G.Rule)
    -- go (G.FilterRule _ _) = bug $ "A rule cannot have a filter before the first premise: " <> ticks (pretty _rule.originalRule)
    -- go (G.LetRule _ _ _) = bug $ "A rule cannot have a let before the first premise: " <> ticks (pretty _rule.originalRule)
    -- go (G.ConclusionRule _) = bug $ "A rule cannot have 0 premises: " <> ticks (pretty _rule.originalRule)
    -- go (G.FilterRule cond rule) = do
    --   checkCondition (assertI G.concreteTerm $ cond) >>= if _
    --     then ?a 
    --     else ?a
    -- go (G.QuantificationRule quant rule) = do
    --   modify_ \st -> st {quantificationsRev = quant Array.: st.quantificationsRev}
    --   go rule
    -- go (G.PremiseRule prop rule) = pure (prop /\ rule)
    go = hole "TODO"

  -- premise /\ rule = runState (go _rule.originalRule) {quantificationsRev: _rule.quantifications, sigma: _rule.sigma}
  
  pure $ NormRule
    { originalRule: _rule.originalRule
    , quantifications: _rule.quantifications
    , sigma: hole "sigma"
    , premise: hole "premise" -- _rule.premise
    , rule: hole "rule" }

-- instance Make NormRule G.Rule where
--   make originalRule = NormRule
--     { originalRule
--     , quantifications: Array.reverse quantificationsRev
--     , sigma: Map.empty
--     , premise
--     , rule }
--     where
--     go :: G.Rule -> State {quantificationsRev :: Array G.Quantification, sigma :: G.Sub} (G.SymbolicProposition /\ G.Rule)
--     go (G.FilterRule _ _) = bug $ "A rule cannot have a filter before the first premise: " <> ticks (pretty originalRule)
--     go (G.LetRule _ _ _) = bug $ "A rule cannot have a let before the first premise: " <> ticks (pretty originalRule)
--     go (G.ConclusionRule _) = bug $ "A rule cannot have 0 premises: " <> ticks (pretty originalRule)
--     go (G.QuantificationRule quant rule') = do
--       modify_ \st -> st {quantificationsRev = quant Array.: st.quantificationsRev}
--       go rule'
--     go (G.PremiseRule prop rule') = pure (prop /\ rule')

--     (premise /\ rule) /\ {quantificationsRev, sigma} = runState (go originalRule) {quantificationsRev: [], sigma: Map.empty}

derive instance Newtype NormRule _
derive newtype instance Show NormRule
derive newtype instance Eq NormRule

instance Pretty NormRule where
  pretty (NormRule rule) = pretty rule.rule

-- | Internal fixpoint implementation.
fixpoint :: forall m. MonadEffect m => Database -> G.FixpointSpecName -> ModuleT m Database
fixpoint (Database initialProps) fixpointSpecName = do
  Debug.debugA "[fixpoint] start"
  moduleCtx <- getModuleCtx

  let fixpointSpec = assertI keyOfMap $ fixpointSpecName /\ (unwrap moduleCtx.module_).fixpoints

  axioms <- do
    let axiomNames = (unwrap fixpointSpec).axiomNames
    let axioms = (unwrap moduleCtx.module_).axioms
          # Map.filterWithKey \axiomName _ -> axiomName `Array.elem` axiomNames
    pure $ Array.fromFoldable $ Map.values axioms

  let props = initialProps <> (axioms <#> \(Axiom prop) -> prop)

  -- Initialize queue with patches that conclude with each prop in the database
  -- i.e. everything starts off as out-of-date.
  let queue = Queue (List.fromFoldable (ConclusionPatch <$> props))
  Debug.debugA $ "[fixpoint] env.queue:" <> pretty queue

  initialGas <- getModuleCtx <#> _.initialGas

  rules <- do
    let ruleNames = (unwrap fixpointSpec).ruleNames
    let rules = (unwrap moduleCtx.module_).rules
          # Map.filterWithKey (\ruleName _ -> ruleName `Array.elem` ruleNames)
          # map make
    normRule `traverse` rules

  let 
    env :: FixpointEnv
    env = 
      { gas: initialGas
      , database: Database []
      , rules
      , partialRules: []
      , queue
      , comparePatch: curry case _ of
          -- TODO: this doesn't need to be enabled if we are accounting for
          -- partialRules
          -- -- conclusions should be processed BEFORE applications, since a
          -- -- conclusion can teach something that could be used by an
          -- -- application
          -- ConclusionPatch _ /\ ApplyPatch _ -> LT
          _ -> GT }

  Debug.debugA $ "[fixpoint] env.rules:" <> pretty env.rules

  env' <- execStateT loop env

  Debug.debugA "[fixpoint] end"
  Debug.debugA $ "[fixpoint] gas used: " <> show (initialGas - env'.gas)
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
  , rules :: Map.Map G.RuleName NormRule
    -- This is organized to allow quickly "looking up" partially-instantiated
    -- rules that can be applied to a given proposition.
  , partialRules :: Array NormRule
  , queue :: Queue
  , comparePatch :: Patch -> Patch -> Ordering
  }

_database = Proxy :: Proxy "database"
_queue = Proxy :: Proxy "queue"
_comparePatch = Proxy :: Proxy "comparePatch"
_partialRules = Proxy :: Proxy "partialRules"

{-
TODO: When a partially instantiated rule is ruled, put it in the set of other
partially instantiated rules, and put an ApplyPath in the queue with this rule
so that it ends up using the best candidate to satisfy its next premise.

TODO:IDEA: use "stop points" in rules that indicate when to stop and put in the
queue, otherwise "keep going" and trying to satisfy the rest of the premises.
This can defined purely in terms of the ordering over hte quwue, so that these
rules are put at front of queue immediately.
-}

data Patch
  = ApplyPatch NormRule
  | ConclusionPatch G.ConcreteProposition

derive instance Generic Patch _
instance Show Patch where show x = genericShow x

instance Pretty Patch where
  pretty (ApplyPatch rule) = "apply:" <\> indent (pretty rule)
  pretty (ConclusionPatch prop) = "conclude: " <> pretty prop

getAllRules :: forall m. MonadEffect m => FixpointT m (Array NormRule)
getAllRules = do
  rules <- gets _.rules <#> Array.fromFoldable <<< Map.values
  partialRules <- gets _.partialRules
  pure $ rules <> partialRules

insertRule :: forall m. MonadEffect m => NormRule -> FixpointT m Unit
insertRule rule = modify_ $ R.modify _partialRules (rule Array.: _)

--------------------------------------------------------------------------------
-- loop
--------------------------------------------------------------------------------

loop :: forall m. MonadEffect m => FixpointT m Unit
loop = do
  -- do
  --   db <- gets _.database
  --   Debug.debugA $ "[loop] database:" <> pretty db

  -- do
  --   partialRules <- gets _.partialRules
  --   Debug.debugA $ "[loop] partialRules:" <> indent (bullets (Array.fromFoldable (pretty <$> partialRules)))

  queue <- gets _.queue
  Debug.debugA $ "[loop] queue:" <> indent (pretty queue)

  gas <- _.gas <$> modify \env -> env {gas = env.gas - 1}
  if gas <= 0 then bug "[loop] out of gas" else do
    pop >>= case _ of
      Nothing -> 
        -- no more patches; done
        pure unit
      Just patch -> do
        Debug.debugA $ "[loop] learn patch:" <\> indent (pretty patch)
        -- learn patch, yielding new patches
        patches <- learn patch
        Debug.debugA $ "[loop] queued patches:" <> indent (pretty patches)
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
  rules <- getAllRules
  join <$> for rules \rule -> applyRule rule prop
    
learn (ApplyPatch rule) = do
  -- TODO: here I add the rule to the database, which is necessary if there are
  -- premises for this apply patch that could be learned later. Alternatively,
  -- you could order the queue such that conclusions are processed before
  -- applications.
  insertRule rule

  -- For each candidate proposition in the database
  candidates <- getCandidates
  Array.concat <$> for candidates \candidate -> do
    applyRule rule candidate

applyRule :: forall m. MonadEffect m => NormRule -> G.ConcreteProposition -> FixpointT m (Array Patch)
applyRule (NormRule _rule) prop' = case _rule.rule of
  G.FilterRule _ _ -> bug $ "[applyRule] Cannot apply a rule that starts with a filter: " <> pretty (NormRule _rule)
  G.ConclusionRule _ -> bug $ "[applyRule] Cannot apply a rule that starts with a conclusion: " <> pretty (NormRule _rule)
  G.QuantificationRule quant rule ->
    applyRule 
      (NormRule _rule 
        { quantifications = Array.snoc _rule.quantifications quant
        , rule = rule }) 
      prop'
  G.LetRule name term rule -> do
    term' <- evaluateTerm $ assertI G.concreteTerm term
    applyRule 
      (NormRule _rule
        { rule = G.substituteRule (Map.singleton name term') rule })
      prop'
  G.PremiseRule prem rule ->
    -- does the premise unify with the candidate?
    liftFixpointT (unify (Left (prem /\ prop'))) >>= case _ of
      Left _err -> pure []
      Right sigma -> go (NormRule _rule {rule = G.substituteRule sigma rule})
        where
        go (NormRule _rule'@{rule: G.QuantificationRule quant rule'}) = 
          go (NormRule _rule'
            { quantifications = Array.snoc _rule.quantifications quant
            , rule = rule' }) 
        go (NormRule _rule'@{rule: G.LetRule name term rule'}) = do
          term' <- evaluateTerm $ assertI G.concreteTerm term
          go (NormRule _rule'
            { rule = G.substituteRule (Map.singleton name term') rule' })
        go (NormRule _rule'@{rule: G.FilterRule cond rule''}) =
          checkCondition (assertI G.concreteTerm $ G.substituteTerm sigma cond) >>= if _
            then pure [ApplyPatch (NormRule _rule' {rule = rule''})]
            else pure []
        go (NormRule _rule'@{rule: G.ConclusionRule conc}) = do
          let conc' = assertI G.concreteProposition conc
          pure [ConclusionPatch conc']
        go _rule' = pure [ApplyPatch _rule']

tryApplyRule :: forall m. MonadEffect m => NormRule -> G.ConcreteProposition -> FixpointT m (Maybe NormRule)
tryApplyRule (NormRule _rule) prop' = 
  -- does the premise unify with the candidate?
  liftFixpointT (unify (Left (_rule.premise /\ prop'))) >>= case _ of
    Left _err -> pure Nothing
    Right sigma -> hole "TODO"

checkCondition :: forall m. MonadEffect m => G.ConcreteTerm -> FixpointT m Boolean
checkCondition term = do
  term' <- evaluateTerm term
  let success = term' == G.trueTerm
  pure success

--------------------------------------------------------------------------------
-- Subsumption
--------------------------------------------------------------------------------

isSubsumed :: forall m. MonadEffect m => Patch -> FixpointT m Boolean
isSubsumed (ApplyPatch rule) = do
  -- Check if there is an equal rule already known.
  -- TODO: do more complicated subsumption check?
  rules <- getAllRules
  (\f -> Array.foldM f false rules) case _ of
    false -> \_ -> pure false
    true -> \rule' -> pure $ rule == rule'
isSubsumed (ConclusionPatch prop) = do
  -- This patch is subsumed if `prop` is subsumed by any of the propositions in
  -- the Database.
  props <- getCandidates
  (\f -> Array.foldM f false props) case _ of
    false -> \_ -> pure false 
    true -> \prop' -> subsumes prop' prop

-- | `prop1` subsumes `prop2` if `prop1 >= prop2`.
subsumes :: forall m. MonadEffect m => G.ConcreteProposition -> G.ConcreteProposition -> FixpointT m Boolean
subsumes prop1 prop2 =
  pure $ (prop1 ~? prop2) == Just GT
-- subsumes prop1 prop2 = do
--   let result = (prop1 ~? prop2) == Just GT
--   Debug.debugA $ "[subsumes] " <> pretty prop1 <> "  >=?  " <> pretty prop2 <> "  ==>  " <> pretty result
--   pure result

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

evaluateProposition :: forall m. MonadEffect m => G.ConcreteProposition -> FixpointT m G.ConcreteProposition
evaluateProposition (G.Proposition rel a) = do
  a' <- evaluateTerm a
  pure $ G.Proposition rel a'

evaluateTerm :: forall m. MonadEffect m => G.ConcreteTerm -> FixpointT m G.ConcreteTerm
evaluateTerm (G.QuantTerm x _) = absurd x
evaluateTerm (G.ApplicationTerm funName args _) = do
  moduleCtx <- lift getModuleCtx
  let G.FunctionSpec funSpec = assertI keyOfMap $ funName /\ (unwrap moduleCtx.module_).functionSpecs
  case funSpec.implementation of
    Nothing -> bug $ "[evaluateTerm]: function has no internal implementation: " <> ticks (pretty funName)
    Just impl -> do
      args' <- evaluateTerm `traverse` args
      let term' = impl args'
      evaluateTerm term'
evaluateTerm (G.ConstructorTerm prim args ty) = do
  args' <- evaluateTerm `traverse` args
  pure $ G.ConstructorTerm prim args' ty
