-- - !TODO the metavariables in the queried prop are actually _existentially_
--   quantified, not universally. importantly, existentially quantified
--   metavariables can be specified when unifying with a candidate goal's
--   conclusion

module Language.Fixlat.Querying where

import Data.Tuple.Nested
import Language.Fixlat.Deriv
import Language.Fixlat.Grammar
import Prelude
import Prim hiding (Type)
import Text.Pretty

import Control.Monad.Reader (class MonadReader, ReaderT, ask, asks, local, runReader, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalState, get, gets, modify, modify_)
import Data.Array as Array
import Data.Bitraversable (rtraverse)
import Data.Bug (bug)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Foldable (foldM, foldMap, foldr, intercalate, traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.LatList (LatList)
import Data.LatList as LatList
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Debug as Debug
import Effect.File (FilePath(..))
import Effect.File as File
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Language.Fixlat.Deriv as Deriv
import Partial.Unsafe (unsafeCrashWith)
import Text.Hyper as H
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))
import Utility (map2)

fromVarToMVar :: forall m. MonadState (Map.Map Var MVar) m => MonadReader (Array Var) m => Var -> m MX
fromVarToMVar x = gets (Map.lookup x) >>= case _ of 
  Nothing -> do 
    let mx = freshMVar (Just x)
    modify_ $ Map.insert x mx
    pure $ Right $ mx
  -- already been substituted for metavar
  Just mx -> pure $ Right mx

-- Only turn parameterized variables into metavariables; leave others alone
fromCRuleToMRule :: CRule -> MRule
fromCRuleToMRule rule@(Rule r) = 
  flip evalState Map.empty 
  <<< flip runReaderT (r.params <#> \(Param p) -> p.bind)
  $ rtraverse fromVarToMVar rule

-- | MonadQuery

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
  { predLatLists :: Map.Map Var (LatList Derivs)
  , focusMaybeDeriv :: Maybe Deriv
  , gas :: Int
  }

newtype Err = Err String
instance Show Err where show (Err str) = "[querying error] " <> show str

-- | Localize the parameters of a rule by introducing them into the query
-- | context.
localDeriv :: forall m a. Monad m => Deriv -> QueryT m a -> QueryT m a
localDeriv (Deriv deriv) = local \ctx -> do
  let introParam (Param p) = case p.bind of
        -- Right x -> Map.insert x p.quant
        x /\ Nothing -> Map.insert x p.quant
        -- already substituted away
        _ /\ Just _ -> identity
  ctx {mvarQuants = foldr introParam ctx.mvarQuants deriv.params}

learnDeriv :: forall m. Monad m => Deriv -> QueryT m Boolean
learnDeriv (Deriv d) = do 
  Debug.traceM $ "[learnDeriv] deriv = " <> pretty (Deriv d)
  let Prop con = d.con
  predLatLists <- gets _.predLatLists
  progress <- case Map.lookup con.pred predLatLists of
    Nothing -> bug $ "[learnDeriv] each predicate should have an entry in predLatLists"
    Just ll -> case LatList.insert (singleton (Deriv d)) ll of
      Nothing -> pure false
      Just ll' -> do
        modify_ _{predLatLists = Map.alter (const $ Just ll') con.pred predLatLists}
        pure true

  -- debug
  do
    st <- get
    Debug.traceM $ "[learnDeriv] predLatLists: " <> foldMap ("\n" <> _) ((Map.toUnfoldable st.predLatLists :: Array _) <#> \(x /\ latList) -> "pred " <> pretty x <> ":" <> foldMap (\ds -> "\n  - " <> intercalate ", " (pretty <$> unwrap ds)) latList)
    Debug.traceM $ "[learnDeriv] progress: " <> show progress
  
  pure progress

-- | Find a unifying substitution where the expected prop is implied by the
-- | candidate prop (i.e. `expectedProp <= candidateProp`).
localUnifyLeProps :: forall m a. Monad m => MProp -> MProp -> QueryT m a -> QueryT m (UnifyResult a)
localUnifyLeProps expectedProp candidateProp m = do
  uCtx <- asks _.mvarQuants
  let uSt = {exiSigma: Map.empty, uniSigma: Map.empty}
  sequence $ runUnify uCtx uSt (unifyLeLProp expectedProp candidateProp) <#> \uSt' ->
    local (\qSt -> qSt{mvarSubst = extendMVarSubst qSt.mvarSubst (uSt'.exiSigma `unionMVarSubst` uSt'.uniSigma)}) m

-- | Attempt to unify a derivation's conclusion with a proposition. On success,
-- | continue with the specialized derivation that has the goal as its
-- | conclusion.
unifyDerivationConclusionWithGoal :: forall m a. Monad m => Deriv -> MProp -> ((Deriv /\ MProp) -> QueryT m a) -> QueryT m (UnifyResult a)
unifyDerivationConclusionWithGoal deriv goal onSuccess = do
  deriv'@(Deriv d) <- substDeriv deriv <$> asks _.mvarSubst
  goal' <- substLProp goal <$> asks _.mvarSubst
  -- localize the derivation
  localDeriv deriv' do
    -- unify goal with conclusion of the derivation
    Debug.traceM $ "[unifyDerivationConclusionWithGoal]\n  - goal'  = " <> pretty goal' <> "\n  - deriv' = " <> pretty d.con
    localUnifyLeProps goal' d.con do
      deriv'' <- substDeriv deriv' <$> asks _.mvarSubst
      goal'' <- substLProp goal' <$> asks _.mvarSubst
      -- continue
      onSuccess (deriv'' /\ goal'')

queryInitRule :: forall m. Monad m => MRule -> QueryT m (Maybe Deriv)
queryInitRule (Rule r) = do
  -- intro params
  let
    introParam (Param p) = case p.bind of
      Left _ -> identity
      Right mx -> Map.insert mx p.quant
    withParams = local \ctx -> do ctx {mvarQuants = foldr introParam ctx.mvarQuants r.params}
  withParams do
    -- assume hyps
    flip traverseWithIndex_ r.hyps \i hyp -> 
      learnDeriv $ initDeriv 
        { label: Label $ unwrap r.label <> "-hyp#" <> show i
          -- only include the params that actually appear in hyp
        , params: 
            r.params 
            # Array.filter (\(Param p) -> p.bind `List.elem` foldMap List.singleton hyp) 
            >>> map2 (either (\x -> unsafeCrashWith $ "concrete var should not appear in parameters of MRule: " <> pretty x) identity)
        , hyps: mempty
        , con: hyp
        }
    -- query con
    queryProp r.con \mb_deriv -> do
      snapshot {head: Right mb_deriv, status: "finished"}
      case mb_deriv of 
        Nothing -> do
          Debug.traceM $ "[queryInitRule] FAILURE"
        Just deriv ->
          Debug.traceM $ "[queryInitRule] SUCCESS\n  - deriv = " <> pretty deriv
      pure mb_deriv

queryProp :: forall m a. Monad m => MProp -> (Maybe Deriv -> QueryT m a) -> QueryT m a
queryProp goal@(Prop p) k = do
  modify_ _{focusMaybeDeriv = Nothing}
  snapshot {head: Left goal, status: "finding next derivation to focus"}
  st <- modify \st -> st {gas = st.gas - 1}
  if st.gas == 0 then do
    Debug.traceM $ "[queryProp] OUT OF GAS"
    snapshot {head: Left goal, status: "<b style=\"color: white; background-color: red\">OUT OF GAS</b>"}
    k Nothing
  else do
    Debug.traceM $ "\n\n[queryProp] goal = " <> pretty goal
    gets (_.predLatLists >>> Map.lookup p.pred) >>= case _ of
      Nothing -> bug $ "[queryProp] each predicate should have an entry in predLatLists"
      -- there are some derivations that can produce instances of this
      -- propisition's predicate
      Just derivss -> do
        
        let tryDeriv :: Deriv -> QueryT m (Maybe a)
            tryDeriv deriv0 = do
              Debug.traceM $ "\n[queryProp] deriv = " <> pretty deriv0
              let deriv@(Deriv d) = freshenDeriv deriv0
              Debug.traceM $ "[queryProp] freshenDeriv\n  - deriv = " <> pretty deriv
              -- try to match goal with derivation's conclusion, then query matched
              -- derivation
              Debug.traceM $ "[queryProp] trying to unify goal with deriv's conclusion\n  - deriv = " <> pretty deriv <> "\n  - con = " <> pretty d.con <> "\n  - goal = " <> pretty goal
              ( unifyDerivationConclusionWithGoal deriv goal \(deriv' /\ goal') -> do
                  -- unified with derivation's goal
                  Debug.traceM $ "[queryProp] unify SUCCESS\n  - deriv' = " <> pretty deriv'
                  -- !TODO set deriv' to focusly used deriv
                  modify_ _{focusMaybeDeriv = Just deriv'}
                  snapshot {head: Left goal', status: "unify <b style=\"color: white; background-color: blue\">SUCCESS</b>"}
                  procDeriv deriv' case _ of
                    Nothing -> do
                      Debug.traceM $ "[queryProp] query FAILURE to prove next hyp of deriv"
                      snapshot {head: Left goal', status: "query <b style=\"color: white; background-color: red\">FAILURE</b> on next hypothesis of focus derivation"}
                      k Nothing -- pure Nothing
                    -- !TODO move deriv to back
                    -- proved next hypothesis of unified derivation, but made no
                    -- progress (didn't learn anything), and not done with proof
                    -- yet
                    (Just (Left false)) -> do
                      Debug.traceM $ "[queryProp] query SUCCESS to prove next hyp of deriv; NO progress"
                      snapshot {head: Left goal', status: "query <b style=\"color: white; background-color: green\">SUCCESS</b> on next hypothesis of focus derivation, but <b style=\"color: white; background-color: red\">NO</b> progress"}
                      queryProp goal' k
                    -- !TODO move deriv to front
                    -- proved that hypothesis of unified derivation, and made
                    -- progress, but dont done with proof yet
                    (Just (Left true)) -> do
                      snapshot {head: Left goal', status: "query <b style=\"color: white; background-color: green\">SUCCESS</b> on that hypothesis of focus derivation, and <b style=\"color: white; background-color: green\">YES</b> progress"}
                      queryProp goal' k
                    -- !TODO move deriv to back
                    -- derivation is done, so query is done
                    (Just (Right d')) -> do
                      Debug.traceM $ "[queryProp] deriv has no hyps, so DONE"
                      snapshot {head: Left goal', status: "focus derivation has no hypotheses, so <b style=\"color: white; background-color: blue\">DONE</b>"}
                      k (Just d')
                ) >>= case _ of
                  Left err -> do
                    Debug.traceM $ "[queryProp] unify FAILURE:" <+> err
                    pure Nothing
                  Right a -> pure (Just a)
        
        let tryDerivs :: Array Deriv -> QueryT m a
            tryDerivs derivs = case Array.uncons derivs of
              Nothing -> k Nothing
              Just {head: deriv, tail: derivs'} ->
                tryDeriv deriv >>= case _ of
                  -- continue on falure
                  Nothing -> tryDerivs derivs'
                  -- break on success
                  Just a -> pure a

        tryDerivs $ Deriv.concat $ List.toUnfoldable $ unwrap derivss

-- | Process a derivation by:
-- |  - if it has no hypotheses, then already have a fully-processed derivation
-- |  - if has some hypotheses, then query the next hypothesis and if it's
-- |    successfully proven then learn the resulting derivation that has that
-- |    hypothesis proven
procDeriv :: forall m a. Monad m => Deriv -> (Maybe (Either Boolean Deriv) -> QueryT m a) -> QueryT m a
procDeriv deriv@(Deriv d) k = case d.hyps of
  -- unified derivation has no hypotheses
  List.Nil -> do
    -- if derivation is finished, then just yield it
    if isFinished deriv then do
      Debug.traceM $ "[procDeriv] unified deriv has no more hypotheses nor free metavars, so just yield it\n  - derive = " <> pretty deriv
      k (Just (Right deriv))
    else do
      -- otherwise, try querying again
      -- map Right <$> queryProp d.con
      -- !TODO detect when you can still unify away metavars
      k (Just (Right deriv))
  -- unified derivation still has hypotheses, so query the next one
  List.Cons hyp _ -> do
    queryProp hyp case _ of
      -- failed to prove hypothesis
      Nothing -> do
        Debug.traceM $ "[procDeriv] failed to prove hyp of deriv\n  - hyp = " <> pretty hyp <> "\n  - deriv = " <> pretty deriv
        k Nothing
      -- proved next hypothesis of deriv
      Just hypDeriv -> do
        hypDeriv' <- substDeriv hypDeriv <$> asks _.mvarSubst
        Deriv d' <- substDeriv deriv <$> asks _.mvarSubst
        let hyps' = List.drop 1 d'.hyps
        -- new derivation with the appropriate hypothesis proven hypothesis
        let deriv'' = Deriv d'
              { derivsRev = List.Cons hypDeriv' d'.derivsRev
              , hyps = hyps' }
        modify_ _{focusMaybeDeriv = Just deriv''}
        -- learned deriv is inserted at front
        Debug.traceM $ "[procDeriv] proved next hyp so learn its hypDeriv\n  - hypDeriv = " <> pretty hypDeriv' <> "\n  - deriv' = " <> pretty deriv''
        progress <- learnDeriv deriv''
        k (Just (Left progress))

snapshot :: forall m. Monad m => 
  { head :: Either MProp (Maybe Deriv)
  , status :: String
  } -> QueryT m Unit
snapshot args = do
  ctx <- ask
  st <- get
  let i = unsafePerformEffect $ Ref.read File.iRef
  let html = 
        H.div [H.Class "querying"] <<< Array.concat $
          [ [ H.div [H.Class "navigation"] $ Array.concat
              [ if i == 0 then [H.raw "<span class=\"navigation-button\">-</span>"] else [H.raw $ "<a class=\"navigation-button\" href=\"step" <> show (i - 1) <> ".html\">-</a>"]
              , [H.raw $ "<span>step " <> show i <> "</span>"]
              , if isRight args.head then [H.raw "<span class=\"navigation-button\">+</span>"] else [H.raw $ "<a class=\"navigation-button\" href=\"step" <> show (i + 1) <> ".html\">+</a>"]
              ]
            ]
          , case args.head of
              Left goal ->
                [ H.div [H.Class "section goal"]
                  [ H.div [H.Class "title"] [H.raw "goal"]
                  , H.raw $ pretty goal ]
                ]
              Right result -> 
                [ H.div [H.Class "section result"]
                  [ H.div [H.Class "title"] [H.raw "result"]
                  -- , H.raw $ maybe "FAILURE" pretty result 
                  , maybe (H.raw "FAILURE") (H.div_ <<< H.hyper) result
                  ]
                ]
          , [ H.div [H.Class "section status" ]
              [ H.div [H.Class "title"] [H.raw "status"] 
              , H.raw args.status ]            
            ]
          , case st.focusMaybeDeriv of
              Just deriv -> 
                [ H.div [H.Class "section focusDeriv"]
                  [ H.div [H.Class "title"] [H.raw "focusDeriv"] 
                  , H.div_ $ H.hyper deriv
                  ] 
                ]
              Nothing -> []
          , [ H.div [H.Class "section mvars"] $
                (H.div [H.Class "title"] [H.raw "mvars"] Array.: _) $
                Map.toUnfoldable ctx.mvarQuants <#> \(x /\ q) ->
                  case Map.lookup x ctx.mvarSubst of
                    Nothing -> H.div_ [H.raw $ pretty q <> " " <> pretty x]
                    Just t -> H.div_ [H.raw $ pretty q <> " " <> pretty x <> " := " <> pretty t]
            ]
          , [ H.div [H.Class "section preds"] $ 
                (H.div [H.Class "title"] [H.raw "preds"] Array.: _) $
                Map.toUnfoldable ctx.preds <#> \(x /\ p) ->
                  H.div_ [H.raw $ pretty p]
            ]
          , [ H.div [H.Class "section predLatLists"] <<<
                (H.div [H.Class "title"] [H.raw "predLatLists"] Array.: _) $
                Map.toUnfoldable st.predLatLists <#> \(predVar /\ latList) ->
                  H.div [H.Class "subsection predLatList"] $
                    [ H.div [H.Class "title"] [H.raw $ "pred " <> pretty predVar]
                    , H.div [H.Class "derivs"] $ latList #
                        unwrap >>>
                        Array.fromFoldable >>>
                        map (
                          unwrap >>>
                          map H.hyper >>>
                          Array.concat >>>
                          H.div_
                        )
                    ] 
            ]
          ]
  let _ = unsafePerformEffect $ File.writeFile 
            ( H.renderDocument 
                (H.defaultDocumentOptions "querying-snapshot") 
                  {linkHrefs = ["main.css"]}
                html
            ) 
            (FilePath {base: "snapshots/step", end: "html"}) 
  pure unit