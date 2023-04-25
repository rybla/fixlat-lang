-- - !TODO the metavariables in the queried prop are actually _existentially_
--   quantified, not universally. importantly, existentially quantified
--   metavariables can be specified when unifying with a candidate goal's
--   conclusion

module Language.Fixlat.Querying where

import Language.Fixlat.Deriv
import Language.Fixlat.Grammar
import Language.Fixlat.MVar
import Prelude
import Prim hiding (Type)

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader, asks, local, runReader)
import Control.Monad.State (class MonadState, gets, modify_)
import Data.Array.NonEmpty as Array
import Data.Bitraversable (rtraverse)
import Data.Bug (bug)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldM, foldMap, foldr)
import Data.LatList (LatList, unwrap)
import Data.LatList as LatList
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Language.Fixlat.Deriv as Deriv
import Partial.Unsafe (unsafeCrashWith)

fromVarToMVar :: forall m. MonadReader (Map.Map Var MVar) m => Var -> m MVar
fromVarToMVar x = asks (Map.lookup x) >>= case _ of 
  Nothing -> pure $ freshMVar (Just x)
  Just mx -> pure mx

fromCRuleToMRule :: CRule -> MRule
fromCRuleToMRule = flip runReader Map.empty <<< rtraverse fromVarToMVar

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
-- |   - `predLatLists`: map of predicate name to lattice set of known rules that
-- |     can produce instances of that predicate
type St = 
  { predLatLists :: Map.Map Var (LatList MDerivs)
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
    derivsRev <- substJoin `traverse` deriv.derivsRev
    hyps <- substJoin `traverse` deriv.hyps
    con <- substJoin deriv.con
    pure $ Deriv deriv {params = params, derivsRev = derivsRev, hyps = hyps, con = con}

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
  substJoin (Inj1Term t y) = Inj1Term <$> substJoin t <*> pure y
  substJoin (Inj2Term t y) = Inj2Term <$> substJoin t <*> pure y

-- | Localize the parameters of a rule by introducing them into the query
-- | context.
localMDeriv :: forall m a. MonadQuery m => MDeriv -> m a -> m a
localMDeriv (Deriv deriv) = local \ctx ->
  ctx {mvarQuants = foldr (\qp -> Map.insert qp.bind qp.quant) ctx.mvarQuants deriv.params}

learnDeriv :: forall m a. MonadQuery m => MDeriv -> m Unit
learnDeriv (Deriv d) = do 
  let Prop con = d.con
  -- gets (_.predLatLists >>> Map.lookup goal.pred) >>= case _ of
  modify_ \st -> st 
    { predLatLists = Map.alter
        (case _ of
          Nothing -> bug $ "[learnDeriv] each predicate should have an entry in predLatLists"
          Just ll -> Just (LatList.insert (singleton (Deriv d)) ll)
        )
        con.pred st.predLatLists
    }

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
matchDerivation (Deriv d) goal onSuccess = do
  -- localize the derivation
  localMDeriv (Deriv d) do
    -- unify goal with conclusion of the derivation
    let con = d.con
    localUnifyProps goal con do
      -- apply unifying substitution to the derivation
      d' <- subst (Deriv d)
      -- continue
      onSuccess d'

queryProp :: forall m. MonadQuery m => MProp -> m (Maybe MDeriv)
queryProp (Prop goal) = do
  gets (_.predLatLists >>> Map.lookup goal.pred) >>= case _ of
    Nothing -> bug $ "[queryProp] each predicate should have an entry in predLatLists"
    -- there are some derivations that can produce instances of this
    -- propisition's predicate
    Just derivss -> do
      let derivs = Deriv.concat $ List.toUnfoldable $ unwrap derivss
      -- for each derivation (break if done)
      (\f -> foldM f Nothing derivs) case _ of
        -- already proved goal, so just keep derivation
        Just d -> \_ -> pure (Just d)
        -- haven't proved goal yet, so try this derivation
        Nothing -> \d -> do
          -- try to match goal with derivation's conclusion, then query matched
          -- derivation
          matchDerivation d (Prop goal) procDeriv >>= case _ of
              -- failed to match goal with derivation's conclusion
              -- !TODO put deriv in back
              Nothing -> do
                pure Nothing
              -- failed to prove next hypothesis of matched derivation
              -- !TODO put deriv in back
              (Just Nothing) -> do
                pure Nothing
              -- !TODO put deriv in front
              -- proved next hypothesis of matched derivation
              (Just (Just Nothing)) -> do
                pure Nothing
              -- derivation is done, so query is done
              (Just (Just (Just d'))) -> pure (Just d')

-- | Process a derivation by:
-- |  - if it has no hypotheses, then already have a fully-processed derivation
-- |  - if has some hypotheses, then query the next hypothesis and if it's
-- |    successfully proven then learn the resulting derivation that has that
-- |    hypothesis proven 
procDeriv :: forall m. MonadQuery m => MDeriv -> m (Maybe (Maybe MDeriv))
procDeriv deriv@(Deriv d) = case d.hyps of
  -- matched derivation has no hypotheses, so just yield it
  List.Nil -> pure (Just (Just deriv))
  -- matched derivation still has hypotheses, so query the next one
  List.Cons hyp hyps -> do
    queryProp hyp >>= case _ of
      -- can't prove hypothesis
      Nothing -> pure Nothing
      -- proved next hypothesis
      Just hypDeriv -> do
        -- new derivation with the appropriate hypothesis proven hypothesis
        let deriv' = Deriv d 
              { derivsRev = List.Cons hypDeriv d.derivsRev
              , hyps = hyps }
        learnDeriv deriv'
        pure (Just Nothing)
