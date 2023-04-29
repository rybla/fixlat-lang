-- - !TODO the metavariables in the queried prop are actually _existentially_
--   quantified, not universally. importantly, existentially quantified
--   metavariables can be specified when unifying with a candidate goal's
--   conclusion

module Language.Fixlat.Querying where

import Data.Tuple.Nested
import Language.Fixlat.Deriv
import Language.Fixlat.Grammar
import Language.Fixlat.MVar
import Prelude
import Prim hiding (Type)

import Control.Monad.Reader (class MonadReader, ReaderT, asks, local, runReader)
import Control.Monad.State (StateT, get, gets, modify_)
import Data.Bitraversable (rtraverse)
import Data.Bug (bug)
import Data.Either (Either(..))
import Data.Foldable (foldM, foldMap, foldr, intercalate)
import Data.LatList (LatList)
import Data.LatList as LatList
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Debug as Debug
import Language.Fixlat.Deriv as Deriv
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))

fromVarToMVar :: forall m. MonadReader (Map.Map Var MVar) m => Var -> m MVar
fromVarToMVar x = asks (Map.lookup x) >>= case _ of 
  Nothing -> pure $ freshMVar (Just x)
  Just mx -> pure mx

fromCRuleToMRule :: CRule -> MRule
fromCRuleToMRule = flip runReader Map.empty <<< rtraverse fromVarToMVar

-- | MonadQuery

-- class
--   ( Monad m
--   , MonadReader Ctx m
--   , MonadState St m
--   ) <= MonadQuery m

-- type MonadQuery m = (Monad m)

type QueryT m = StateT St (ReaderT Ctx m)

-- | Querying context:
-- |   - `preds`: map of predicate name to predicate declaration
-- |   - `rules`: map of predicate name to array of rules that each can produce
-- |     an instance of that predicate
type Ctx = 
  { preds :: Map.Map Var Pred
  , mvarQuants :: Map.Map MVar Quant
  , mvarSubst :: Map.Map MVar MTerm
  }
_mvarSubst = Proxy :: Proxy "mvarSubst"

-- | Querying state:
-- |   - `predLatLists`: map of predicate name to lattice set of known rules that
-- |     can produce instances of that predicate
type St = 
  { predLatLists :: Map.Map Var (LatList MDerivs)
  }

newtype Err = Err String
instance Show Err where show (Err str) = "[querying error] " <> show str

-- | Localize the parameters of a rule by introducing them into the query
-- | context.
localMDeriv :: forall m a. Monad m => MDeriv -> QueryT m a -> QueryT m a
localMDeriv (Deriv deriv) = local \ctx -> do
  let introParam (Param p) = case p.bind of
        Left _ -> identity
        Right x -> Map.insert x p.quant
  ctx {mvarQuants = foldr introParam ctx.mvarQuants deriv.params}

learnDeriv :: forall m. Monad m => MDeriv -> QueryT m Unit
learnDeriv (Deriv d) = do 
  Debug.traceM $ "[learnDeriv] deriv = " <> pretty (Deriv d)
  let Prop con = d.con
  modify_ \st -> st 
    { predLatLists = Map.alter
        (case _ of
          Nothing -> bug $ "[learnDeriv] each predicate should have an entry in predLatLists"
          Just ll -> Just (LatList.insert (singleton (Deriv d)) ll)
        )
        con.pred st.predLatLists
    }
  do
    st <- get
    Debug.traceM $ "[learnDeriv] predLatLists: " <>
      -- intercalate "\n" ((Map.toUnfoldable st.predLatLists :: Array _) <#> unwrap >>> map (unwrap >>> map pretty >>> intercalate ", ") >>> intercalate "\n")
      foldMap ("\n" <> _) ((Map.toUnfoldable st.predLatLists :: Array _) <#> \(x /\ latList) -> "pred " <> pretty x <> ":" <> foldMap (\ds -> "\n  - " <> intercalate ", " (pretty <$> unwrap ds)) latList)

-- | Find a unifying substitution where the expected prop is implied by the
-- | candidate prop (i.e. `expectedProp <= candidateProp`).
localUnifyProps :: forall m a. Monad m => MProp -> MProp -> QueryT m a -> QueryT m (Maybe a)
localUnifyProps expectedProp candidateProp m = do
  uCtx <- asks _.mvarQuants
  let uSt = {exiSigma: Map.empty, uniSigma: Map.empty}
  sequence $ runUnify uCtx uSt (unifyLeLProp expectedProp candidateProp) <#> \uSt' ->
    local (\qSt -> qSt{mvarSubst = Map.unions [qSt.mvarSubst, uSt'.exiSigma, uSt'.uniSigma]}) m

-- | Attempt to match a derivation's conclusion with a proposition. On success,
-- | continue with the specialized derivation that has the goal as its conclusion.
matchDerivation :: forall m a. Monad m => MDeriv -> MProp -> (MDeriv -> QueryT m a) -> QueryT m (Maybe a)
matchDerivation (Deriv d) goal onSuccess = do
  -- localize the derivation
  localMDeriv (Deriv d) do
    -- unify goal with conclusion of the derivation
    let con = d.con
    localUnifyProps goal con do
      -- apply unifying substitution to the derivation
      d' <- substDeriv (Deriv d) <$> asks _.mvarSubst
      -- continue
      onSuccess d'

queryProp :: forall m. Monad m => MProp -> QueryT m (Maybe MDeriv)
queryProp (Prop goal) = do
  Debug.traceM $ "[queryProp] goal = " <> pretty (Prop goal)
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
          Debug.traceM $ "[queryProp] trying to match goal with deriv's conclusion; deriv = " <> pretty d
          matchDerivation d (Prop goal) procDeriv >>= case _ of
              -- failed to match goal with derivation's conclusion
              -- !TODO move deriv to back
              Nothing -> do
                Debug.traceM $ "[queryProp] failed to match"
                pure Nothing
              -- failed to prove next hypothesis of matched derivation
              -- !TODO move deriv to back
              (Just Nothing) -> do
                Debug.traceM $ "[queryProp] matched; failed to prove next hyp of deriv"
                pure Nothing
              -- !TODO move deriv to front
              -- proved next hypothesis of matched derivation, so we're making
              -- progress, but not done yet
              (Just (Just Nothing)) -> do
                Debug.traceM $ "[queryProp] matched; proved next hyp of deriv"
                queryProp (Prop goal)
              -- !TODO move deriv to back
              -- derivation is done, so query is done
              (Just (Just (Just d'))) -> do
                Debug.traceM $ "[queryProp] matched; deriv has no hyps, so done"
                pure (Just d')

-- | Process a derivation by:
-- |  - if it has no hypotheses, then already have a fully-processed derivation
-- |  - if has some hypotheses, then query the next hypothesis and if it's
-- |    successfully proven then learn the resulting derivation that has that
-- |    hypothesis proven 
procDeriv :: forall m. Monad m => MDeriv -> QueryT m (Maybe (Maybe MDeriv))
procDeriv deriv@(Deriv d) = case d.hyps of
  -- matched derivation has no hypotheses, so just yield it
  List.Nil -> do
    Debug.traceM $ "[procDeriv] matched deriv has no more hypotheses, so just yield it; derive = " <> pretty deriv
    pure (Just (Just deriv))
  -- matched derivation still has hypotheses, so query the next one
  List.Cons hyp hyps -> do
    queryProp hyp >>= case _ of
      -- failed to prove hypothesis
      Nothing -> do
        Debug.traceM $ "[procDeriv] failed to prove hyp of deriv; hyp = " <> pretty hyp <> "; deriv = " <> pretty deriv
        pure Nothing
      -- proved next hypothesis
      Just hypDeriv -> do
        -- new derivation with the appropriate hypothesis proven hypothesis
        let deriv' = Deriv d 
              { derivsRev = List.Cons hypDeriv d.derivsRev
              , hyps = hyps }
        -- learned deriv is put at front
        Debug.traceM $ "[procDeriv] proved next hyp so learn its hypDeriv; hypDeriv = " <> pretty hypDeriv <> "; deriv' = " <> pretty deriv'
        learnDeriv deriv'
        pure (Just Nothing)
