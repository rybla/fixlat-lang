-- - !TODO the metavariables in the queried prop are actually _existentially_
--   quantified, not universally. importantly, existentially quantified
--   metavariables can be specified when unifying with a candidate goal's
--   conclusion

module Language.Fixlat.Querying where

import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Grammar
import Prelude
import Prim hiding (Type)
import Utility

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Reader (class MonadReader, Reader, ReaderT, asks, local, runReader)
import Control.Monad.State (class MonadState, State, StateT, gets, modify_, runState)
import Control.MonadPlus (class MonadPlus)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Bitraversable (rtraverse)
import Data.Cast (class Cast)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldM, foldMap, foldl, foldr)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Newtype (unwrap)
import Data.Set.Lattice as Lattice
import Data.Traversable (class Traversable, for, traverse)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)

-- | MVar

newtype MVar = MVar (Maybe Var /\ UUID)
derive newtype instance Eq MVar
derive newtype instance Ord MVar

freshMVar :: Maybe Var -> MVar
freshMVar mb_var = MVar (mb_var /\ unsafePerformEffect UUID.genUUID)

type MX = MVar -- type of variables in a meta structure
type MRule = LRule MX -- meta
type MParam = Param MX
type MProp = LProp MX
type MTerm = LTerm MX
type MDeriv = Deriv MX

fromVarToMVar :: forall m. MonadReader (Map.Map Var MVar) m => Var -> m MVar
fromVarToMVar x = asks (Map.lookup x) >>= case _ of 
  Nothing -> pure $ freshMVar (Just x)
  Just mx -> pure mx

fromCRuleToMRule :: CRule -> MRule
fromCRuleToMRule = flip runReader Map.empty <<< rtraverse fromVarToMVar

-- | A derivation is a tree.
data Deriv xt = Deriv
  { label :: Label
  , params :: Array (Param xt)
  , hypsProven :: Array (Deriv xt)
  , hyps :: Array (Prop CLat xt)
  , con :: Prop CLat xt
  }

derive instance Functor Deriv 
derive instance Foldable Deriv 
derive instance Traversable Deriv

fromRuleToDeriv :: forall xt. LRule xt -> Deriv xt
fromRuleToDeriv (Rule rule) = Deriv
  { label: rule.label
  , params: rule.params
  , hypsProven: []
  , hyps: rule.hyps
  , con: rule.con
  }


-- | MonadQuery

class
  ( Monad m
  , MonadReader Ctx m
  , MonadState St m
  , MonadError Err m
  ) <= MonadQuery m

-- | Querying context:
-- |   - `preds`: map of predicate name to predicate declaration
-- |   - `rules`: map of predicate name to array of rules that each can produce
-- |     an instance of that predicate
type Ctx = 
  { preds :: Map.Map Var Pred
  , mvarQuants :: Map.Map MVar Quant
  , mvarSubst :: Map.Map MVar MTerm
  }

-- | Querying state:
-- |   - `predLatticeSets`: map of predicate name to lattice set of known rules that
-- |     can produce instances of that predicate
type St = 
  { predLatticeSets :: Map.Map Var (Lattice.Set MDeriv)
  }

newtype Err = Err String
instance Show Err where show (Err str) = "[querying error] " <> show str

-- | Substitution

class Subst :: (Prim.Type -> Prim.Type) -> Constraint
class Functor t <= Subst t where substJoin :: forall m. MonadQuery m => t (m (MVar \/ MTerm)) -> m (t MVar)

substMVar :: forall m. MonadQuery m => MVar -> m (Maybe MTerm)
substMVar x = asks (_.mvarSubst >>> Map.lookup x)

subst :: forall t m. Subst t => MonadQuery m => t MVar -> m (t MVar)
subst = substJoin <<< map \mv -> substMVar mv <#> maybe (Left mv) Right

instance Subst LRule where
  substJoin (Rule rule) = do
    params <- foldMap identity <$> for rule.params \p -> 
      p.bind <#> either (\x -> [p {bind = x}]) (const [])
    hyps <- substJoin `traverse` rule.hyps
    con <- substJoin rule.con
    pure $ Rule rule {params = params, hyps = hyps, con = con}

instance Subst Deriv where
  substJoin (Deriv deriv) = do
    params <- foldMap identity <$> for deriv.params \p -> 
      p.bind <#> either (\x -> [p {bind = x}]) (const [])
    hypsProven <- substJoin `traverse` deriv.hypsProven
    hyps <- substJoin `traverse` deriv.hyps
    con <- substJoin deriv.con
    pure $ Deriv deriv {params = params, hypsProven = hypsProven, hyps = hyps, con = con}

instance Subst LProp where
  substJoin (Prop prop) = do
    arg <- substJoin prop.arg
    pure $ Prop prop {arg = arg}

instance Subst LTerm where
  substJoin (AtomicTerm at y) = pure (AtomicTerm at y)
  substJoin (VarTerm mx y) = mx >>= case _ of
    Left x -> pure (VarTerm x y)
    Right t -> pure t
  substJoin (ProdTerm t1 t2 y) = ProdTerm <$> substJoin t1 <*> substJoin t2 <*> pure y
  substJoin (Proj1Term t y) = Proj1Term <$> substJoin t <*> pure y
  substJoin (Proj2Term t y) = Proj2Term <$> substJoin t <*> pure y
  substJoin (Inj1Term t y) = Inj1Term <$> substJoin t <*> pure y
  substJoin (Inj2Term t y) = Inj2Term <$> substJoin t <*> pure y
  substJoin (ElimTerm _ _ _ _ _ _) = unsafeThrow "TODO"

-- | Localize the parameters of a rule by introducing them into the query
-- | context.
localMDeriv :: forall m a. MonadQuery m => MDeriv -> m a -> m a
localMDeriv (Deriv deriv) = local \ctx ->
  ctx {mvarQuants = foldr (\qp -> Map.insert qp.bind qp.quant) ctx.mvarQuants deriv.params}

-- !TODO
learnDeriv :: forall m a. MonadQuery m => MDeriv -> m Unit
learnDeriv _ = 
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
localUnifyProps :: forall m a. MonadQuery m => MProp -> MProp -> m a -> m (Maybe a)
localUnifyProps _mpropExpected _mpropCandidate _k = 
  unsafeCrashWith "TODO"

-- | Attempt to match a derivation's conclusion with a proposition. On success,
-- | continue with the specialized derivation that has the goal as its conclusion.
matchDerivation :: forall m a. MonadQuery m => MDeriv -> MProp -> (MDeriv -> m a) -> m (Maybe a)
matchDerivation (Deriv deriv) goal onSuccess = do
  -- localize a deriv
  localMDeriv (Deriv deriv) do
    -- unify goal with conclusion of deriv
    let con = deriv.con
    localUnifyProps goal con do
      -- apply unifying substitution to deriv
      deriv' <- subst (Deriv deriv)
      -- continue
      onSuccess deriv'

queryProp :: forall m a. MonadQuery m => MProp -> m (Maybe MDeriv)
queryProp (Prop goal) = do
  gets (_.predLatticeSets >>> Map.lookup goal.pred) >>= case _ of
    -- there are no known derivations that can produce an instance of this
    -- proposition's predicate
    Nothing -> pure Nothing
    -- there are some derivations that can produce instances of this
    -- propisition's predicate
    Just derivs -> do
      -- for each derivation (break if done)
      (\f -> foldM f Nothing derivs) case _ of
        -- already proved goal, so just keep derivation
        Just deriv -> \_ -> pure (Just deriv)
        -- haven't proved goal yet, so try this derivation
        Nothing -> \deriv ->
          join <$> matchDerivation deriv (Prop goal) \(Deriv deriv') -> 
            case Array.uncons deriv'.hyps of
              -- this deriv has no hypotheses, so we're done!
              Nothing -> pure $ Just (Deriv deriv')
              -- this deriv still has hypotheses, so try query the first one
              Just {head: hyp, tail: hyps'} -> do
                queryProp hyp >>= case _ of
                  -- can't prove the first hypothesis, so just continue
                  Nothing -> pure Nothing
                  -- proved the first hypothesis, so learn the newly-proven
                  -- knowledge, then continue
                  Just deriv'' -> do
                    learnDeriv deriv''
                    pure Nothing


-- | Attempt to prove a rule. Continuation on successs.
queryRule :: forall m a. MonadQuery m => MRule -> m a -> m (Maybe a)
queryRule rule _onSuccess = do
  -- -- learn the hypotheses
  -- join $ foldM
  --   (\k hyp -> do
  --     -- learn hyp
  --     pure (\b -> k ?a)
  --   )
  --   (if_ 
  --     (goProp (unwrap rule).con) 
  --     (pure Nothing))
  --   (unwrap rule).hyps
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
  gets (_.predLatticeSets >>> Map.lookup prop.bind) >>= case _ of 
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
          -- let rule = injectCRuleIntoMRule rule
          -- -- specialize rule to expected conclusion
          -- case specializeRule prop rule of 
          --   Nothing -> pure false
          --   Just rule' -> do
          --     -- for each hyp
          --     isProven <- (\f -> foldM f false (unwrap rule').hyps) case _ of
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
matchRule :: forall m. MonadQuery m => CRule -> Lattice.Set CRule -> m (Lattice.Set CRule)
matchRule _ _ = unsafeCrashWith "TODO"

learnDeriv :: forall m. MonadQuery m => MRule -> m Unit
learnDeriv mr = modify_ \st -> st { predLatticeSets = Map. }
-}
