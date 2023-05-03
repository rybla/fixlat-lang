module Language.Fixlat.Deriv where

import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Grammar
import Prelude
import Text.Pretty
import Utility

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (class MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (class MonadState, StateT, get, gets, modify_, put, runStateT)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap, rmap)
import Data.Bug (bug)
import Data.Either (Either(..), either, fromLeft', fromRight')
import Data.Foldable (class Foldable, foldM, foldMap, foldr, intercalate, sequence_, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Lattice (class JoinSemilattice, class Lattice, class MeetSemilattice, class PartialOrd, comparePartial)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (class Traversable)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Debug as Debug
import Effect.Unsafe (unsafePerformEffect)
import Record as R
import Text.Hyper (class Hyper)
import Text.Hyper as H
import Text.Pretty (class Pretty)
import Type.Proxy (Proxy(..))

-- | MVar

newtype MVar = MVar (Maybe Var /\ UUID)
derive instance Generic MVar _
derive instance Newtype MVar _
derive newtype instance Show MVar
instance Pretty MVar where 
  pretty (MVar (Nothing /\ uuid)) = "?" <> String.take 2 (UUID.toString uuid)
  -- !TODO it's so cluttering to have this, so, not sure if its necessary
  pretty (MVar (Just x /\ uuid)) = "?" <> pretty x <> "~" <> String.take 2 (UUID.toString uuid)
derive newtype instance Eq MVar
derive newtype instance Ord MVar

freshMVar :: Maybe Var -> MVar
freshMVar mb_var = MVar (mb_var /\ unsafePerformEffect UUID.genUUID)

getMVarMaybeVar :: MVar -> Maybe Var
getMVarMaybeVar (MVar (mb_var /\ _)) = mb_var

type MX = Var \/ MVar -- type of variables in a meta structure
type MRule = LRule MX -- meta
type MParam = Param MX
type MProp = LProp MX
type MTerm = LTerm MX

-- | Deriv

-- | A derivation is a tree.
data Deriv = Deriv
  { label :: Label
  , params :: Array (Param (MVar /\ Maybe MTerm))
  , derivsRev :: List Deriv
  , hyps :: List (Prop CLat MX)
  , con :: Prop CLat MX
  }

instance Pretty Deriv where
  pretty (Deriv d) =
    pretty d.label <+>
    "(" <> intercalate ", " (d.params <#> prettyParam) <> ")" <+> 
    "{" <>
      -- proven
      (if List.null d.derivsRev then "" else "[" <> intercalate ", " (pretty <$> List.reverse d.derivsRev) <> "]") <+>
      -- to prove
      (if List.null d.hyps then "" else intercalate ", " (pretty <$> d.hyps)) <+>
      "|-" <+>
      pretty d.con <>
    "}"
    where
    prettyParam (Param p) = case p.bind of
      x /\ Nothing -> pretty p.quant <+> pretty x <> ":" <+> pretty p.type_
      x /\ Just t -> pretty p.quant <+> pretty x <> "=" <> pretty t

instance Hyper Deriv where
  hyper (Deriv d) =
    [ H.div [H.Class "deriv"] $ Array.concat
        [ [H.div [H.Class "label"] [H.raw $ "rule" <+> pretty d.label]]
        , if Array.null d.params then [] else [H.div [H.Class "params"] [H.raw $ intercalate ", " (d.params <#> hyperParam)]]
        , if List.null d.derivsRev then [] else [H.div [H.Class "derivs"] <<< Array.concat <<< ([H.div [H.Class "title"] [H.raw "proven"]] Array.: _) $ H.hyper <$> Array.reverse (Array.fromFoldable d.derivsRev)]
        , if List.null d.hyps then [] else [H.div [H.Class "hyps"] $ H.raw <<< pretty <$> Array.fromFoldable d.hyps]
        , [H.div [H.Class "con"] $ [H.raw $ pretty d.con]]
        ]
    ]
    where
    hyperParam (Param p) = case p.bind of
      x /\ Nothing -> pretty p.quant <+> pretty x <> " : " <+> pretty p.type_
      x /\ Just t -> pretty p.quant <+> pretty x <> " := " <+> pretty t

derive instance Generic Deriv _
instance Show Deriv where show x = genericShow x

newtype Derivs = Derivs (Array Deriv)
derive instance Newtype Derivs _
derive newtype instance Show Derivs

singleton :: Deriv -> Derivs
singleton deriv = Derivs (Array.singleton deriv)

concat :: Array Derivs -> Array Deriv
concat = foldMap unwrap

-- initDeriv :: _ -> Deriv
initDerivs args = Derivs [initDeriv args]

initDeriv {label, params, hyps, con} = Deriv
  { label
  , params: (_ /\ Nothing) <$$> params
  , derivsRev: mempty 
  , hyps
  , con
  }


instance PartialOrd Deriv where
  -- d1 <= d2  <==>  d1 has more general hypotheses and more specific conclusion
  comparePartial d1 d2 =
    case runUnify' (unifyLeDeriv d1 d2) of
      Left _err -> case runUnify' (unifyLeDeriv d2 d1) of
        Left _err -> Nothing
        Right st -> 
          -- d1 is alpha-equivalent to d2
          if isRenaming st.exiSigma && isRenaming st.uniSigma then Just EQ
          -- d1 is more general than d2 via sigma
          else Just GT
      Right st -> 
        -- d1 is alpha-equivalent to d2
        if isRenaming st.exiSigma && isRenaming st.uniSigma then Just EQ
        -- d1 is less general than d2 via sigma
        else Just LT

isRenaming :: Map.Map MVar (LTerm MX) -> Boolean
isRenaming sigma = isJust $ foldr f (Just Map.empty) (Map.toUnfoldable sigma :: Array _)
  where 
  f (x /\ t) m_rho = do
    rho <- m_rho
    case substLTerm t sigma of
      VarTerm x' _ -> case Map.lookup x rho of
        -- not renamed yet, so rename here
        Nothing -> Just $ Map.insert x x' rho
        Just x'' -> if x' == x''
          then Just rho -- renaming match
          else Nothing -- mismatch
      _ -> Nothing

type UnifyM = ReaderT UnifyCtx (StateT UnifySt UnifyResult)
type UnifyResult = Either String
type MVarSubst = Map.Map MVar MTerm

type UnifyCtx = Map.Map MVar Quant
type UnifySt = {exiSigma :: MVarSubst, uniSigma :: MVarSubst}

_exiSigma = Proxy :: Proxy "exiSigma"
_uniSigma = Proxy :: Proxy "uniSigma"

extendMVarSubst :: MVarSubst -> MVarSubst -> MVarSubst
extendMVarSubst sigma1 sigma2 = do
  let sigma1' = flip substLTerm sigma2 <$> sigma1
  sigma1' `Map.union` sigma2

unionMVarSubst :: MVarSubst -> MVarSubst -> MVarSubst
unionMVarSubst sigma1 sigma2 = do
  let sigma = sigma1 `Map.union` sigma2
  (_ `substLTerm` sigma) <$> sigma

totalSigma :: UnifyM MVarSubst
totalSigma = do
  exiSigma <- gets _.exiSigma
  uniSigma <- gets _.uniSigma
  pure $ unionMVarSubst exiSigma uniSigma

runUnify :: forall a. UnifyCtx -> UnifySt -> UnifyM a -> UnifyResult UnifySt
runUnify ctx st m = snd <$> runStateT (runReaderT m ctx) st

runUnify' :: forall a. UnifyM a -> UnifyResult UnifySt
runUnify' = runUnify Map.empty {exiSigma: Map.empty, uniSigma: Map.empty}

tryUnify :: forall a. UnifyM a -> UnifyM (Maybe a)
tryUnify m = do
  st <- get
  ctx <- ask
  case flip runReaderT ctx >>> flip runStateT st $ m of
    Left _err -> pure Nothing
    Right (a /\ st) -> do
      put st
      pure (Just a)

anyUnify :: forall f a. Foldable f => f (UnifyM a) -> UnifyM a
anyUnify ms = foldM f Nothing ms >>= case _ of
  Nothing -> throwError $ "none of the collection of unifications worked"
  Just a -> pure a
  where 
  f (Just a) _ = pure (Just a) -- already found a passing unification
  f Nothing m = tryUnify m -- haven't succeeded yet, so try this one

-- constravariant in hypotheses
unifyLeDeriv :: Deriv -> Deriv -> UnifyM Unit
unifyLeDeriv deriv1 deriv2 = do
  Deriv d1 <- substDeriv deriv1 <$> totalSigma
  Deriv d2 <- substDeriv deriv2 <$> totalSigma
  -- intro (unsubstituted) quantifications
  let introParam (Param p) = case p.bind of
        x /\ Nothing -> Map.insert x p.quant
        _ -> identity
  local (flip (foldr introParam) (d1.params <> d2.params)) do
    -- check that each d1 hyp is MORE general than at least one d2 hyp
    flip traverse_ d1.hyps \hyp1 ->
      anyUnify (d2.hyps <#> \hyp2 -> unifyLeLProp hyp2 hyp1)
    -- check that d1 con is LESS general than d2 con 
    unifyLeLProp d1.con d2.con

unifyLeLProp :: MProp -> MProp -> UnifyM Unit
unifyLeLProp (Prop p1) (Prop p2) | p1.pred /= p2.pred = throwError $ "can't unify proposition" <+> pretty (Prop p1) <+> "with proposition" <+> pretty (Prop p2)
unifyLeLProp (Prop p1) (Prop p2) = unifyLeLTerm p1.arg p2.arg

-- | Check that there exists substitutions uniSigma, exiSigma such that
-- term1[exiSigma] = term2[uniSigma]
unifyLeLTerm :: MTerm -> MTerm -> UnifyM Unit
unifyLeLTerm term1 term2 = do
  -- Debug.traceM $ "[unifyLeLTerm]   (" <> pretty term1 <> ")   (" <> pretty term2 <> ")"
  term1' <- substLTerm term1 <$> totalSigma
  term2' <- substLTerm term2 <$> totalSigma
  case term1' /\ term2' of
    VarTerm (Right x1) y1 /\ VarTerm (Right x2) y2 -> do
      q1 <- asks (Map.lookup x1) >>= case _ of
        Nothing -> bug $ "[unifyLeLTerm] when looking up quantifier, unknown variable"
        Just q1 -> pure q1
      -- Debug.traceM $ "[unifyLeLTerm] term1' is var with quant " <> pretty q1
      q2 <- asks (Map.lookup x2) >>= case _ of
        Nothing -> bug $ "[unifyLeLTerm] when looking up quantifier, unknown variable"
        Just q2 -> pure q2
      -- Debug.traceM $ "[unifyLeLTerm] term2' is var with quant " <> pretty q2
      case q1 /\ q2 of
        -- substitute universal on the left for any quantification on the right
        ExistQuant /\ _ -> modify_ $ R.modify _exiSigma $ Map.insert x1 (VarTerm (Right x2) y2)
        -- substitute existential on the right for any quantification on the left
        _ /\ UnivQuant -> modify_ $ R.modify _exiSigma $ Map.insert x2 (VarTerm (Right x1) y1)
        _ -> throwError $ "can't unify variable" <+> pretty x1 <+> "of quantification" <+> pretty q1 <+> "with variable" <+> pretty x2 <+> "of quantification" <+> pretty q2
    VarTerm (Right x1) _ /\ _ -> do
      q1 <- asks (Map.lookup x1) >>= case _ of
        Nothing -> bug $ "[unifyLeLTerm] when looking up quantifier, unknown variable:" <+> pretty x1
        Just q1 -> pure q1
      -- Debug.traceM $ "[unifyLeLTerm] term1' is var with quant " <> pretty q1
      case q1 of
        -- substitute existentials on the left
        ExistQuant -> modify_ $ R.modify _exiSigma $ Map.insert x1 term2'
        UnivQuant -> throwError $ "can't unify variable" <+> pretty x1 <+> "of quantification" <+> pretty q1 <+> "with term" <+> pretty term2'
    _ /\ VarTerm (Right x2) _ -> do
      q2 <- asks (Map.lookup x2) >>= case _ of
        Nothing -> bug $ "[unifyLeLTerm] when looking up quantifier, unknown variable"
        Just q2 -> pure q2
      -- Debug.traceM $ "[unifyLeLTerm] term2' is var with quant " <> pretty q2
      case q2 of
        -- substitute universals on the right
        UnivQuant -> modify_ $ R.modify _uniSigma $ Map.insert x2 term1'
        ExistQuant -> throwError $ "can't unify term" <+> pretty term1' <+> "with variable" <+> pretty x2 <+> "of quantification" <+> pretty q2
    _ -> go term1' term2'
  where
  go :: MTerm -> MTerm -> UnifyM Unit
  go (AtomicTerm at1 _) (AtomicTerm at2 _) | at1 == at2 = pure unit
  go (ProdTerm ta1 tb1 _) (ProdTerm ta2 tb2 _) = sequence_ [unifyLeLTerm ta1 ta2, unifyLeLTerm tb1 tb2]
  go (Inj1Term t1 _) (Inj1Term t2 _) = unifyLeLTerm t1 t2
  go (Inj2Term t1 _) (Inj2Term t2 _) = unifyLeLTerm t1 t2
  go (VarTerm (Left x1) _) (VarTerm (Left x2) _) | x1 == x2 = pure unit
  -- invalid
  go (VarTerm (Right _) _) _ = bug $ "[unifyLeLTerm.go] impossible case, `VarTerm (Right _)` should have already been handled"
  go _ (VarTerm (Right _) _) = bug $ "[unifyLeLTerm.go] impossible case, `VarTerm (Right _)` should have already been handled"
  -- default
  go t1 t2 = throwError $ "can't unify term" <+> pretty t1 <+> "with term" <+> pretty t2

substDeriv :: Deriv -> MVarSubst -> Deriv
substDeriv (Deriv d) sigma | Map.isEmpty sigma = Debug.trace ("[substDeriv] sigma = " <> pretty sigma) \_ -> Deriv d
substDeriv (Deriv d) sigma = Debug.trace ("[substDeriv] sigma = " <> pretty sigma) \_ -> Deriv d
  { params = d.params <#> \(Param p) -> case p.bind of
      -- x /\ Nothing -> case Map.lookup x sigma of
      --   Nothing -> Param p
      --   Just t -> Param p {bind = x /\ Just t}
      -- x /\ Just t -> case Map.lookup x sigma of
      --   Nothing -> Param p
      --   Just t' -> bug $ "attempted to substitute a parameter in a deriv that's already been substituted:\n  - deriv = " <> pretty (Deriv d) <> "\n  - x = " <> pretty x <> "\n  - t = " <> pretty t <> "\n  - t' = " <> pretty t'
      x /\ Nothing -> Param p {bind = x /\ Map.lookup x sigma}
      x /\ Just t -> Param p {bind = x /\ Just (substLTerm t sigma)}
  , derivsRev = flip substDeriv sigma <$> d.derivsRev
  , hyps = flip substLProp sigma <$> d.hyps
  , con = substLProp d.con sigma
  }

substLProp :: MProp -> MVarSubst -> MProp
substLProp (Prop p) sigma = Prop p {arg = substLTerm p.arg sigma}

substLTerm :: MTerm -> MVarSubst -> MTerm
substLTerm t@(AtomicTerm _ _) __sigma = t
substLTerm t@(VarTerm (Left _x) _) _sigma = t
substLTerm t@(VarTerm (Right x) _) sigma = Map.lookup x sigma # maybe t (_ `substLTerm` sigma)
substLTerm (ProdTerm ta tb y) sigma = ProdTerm (substLTerm ta sigma) (substLTerm tb sigma) y
substLTerm (Proj1Term t y) sigma =
  case substLTerm t sigma of
    ProdTerm ta _tb y' -> ta -- normalize projection of product
    t' -> Proj1Term t' y
substLTerm (Proj2Term t y) sigma =
  case substLTerm t sigma of
    ProdTerm _ta tb y' -> tb -- normalize projection of product
    t' -> Proj2Term t' y
substLTerm (Inj1Term t y) sigma = Inj1Term (substLTerm t sigma) y
substLTerm (Inj2Term t y) sigma = Inj2Term (substLTerm t sigma) y

instance PartialOrd Derivs where 
  comparePartial (Derivs ds1) (Derivs ds2) = 
    (\f1 -> foldr f1 (Just EQ) ds1) \d1 mb_ord ->
      (\f2 -> foldr f2 mb_ord ds2) \d2 mb_ord' ->
        f d1 d2 =<< mb_ord' 
    where
    f :: Deriv -> Deriv -> Ordering -> Maybe Ordering
    f deriv1 deriv2 ord = case comparePartial deriv1 deriv2 of
      Just ord' | ord == ord' -> Just ord
      _ -> Nothing

instance MeetSemilattice Derivs where 
  meet (Derivs ds1) (Derivs ds2) = Derivs (ds1 <> ds2)

instance JoinSemilattice Derivs where 
  join (Derivs ds1) (Derivs ds2) = Derivs (ds1 <> ds2)

instance Lattice Derivs

fromRuleToDeriv :: MRule -> Deriv
fromRuleToDeriv (Rule r) = Deriv
  { label: r.label
  , params: r.params <#> map (either (freshMVar <<< Just) identity >>> (_ /\ Nothing))
  , derivsRev: mempty
  , hyps: List.fromFoldable r.hyps
  , con: r.con
  }

-- !TODO old debug: Debug.trace (bifoldMap (\l -> ["lat " <> pretty l]) (either (\x -> ["var " <> pretty x]) (\mx -> ["mvar " <> pretty mx])) d.con :: Array String) \_ -> 
renameDerivMVars :: Map.Map MVar MVar -> Deriv -> Deriv
renameDerivMVars rho (Deriv d) = do
  Deriv d
    { params = d.params # map (map (bimap ren (map (rmap (map ren)))))
    , derivsRev = d.derivsRev # map (renameDerivMVars rho)
    , hyps = d.hyps # map (rmap (map ren))
    , con = d.con # rmap (map ren)
    }
  where
  ren x = x `Map.lookup` rho # maybe x identity

-- | Freshen all unsubstituted params in derivation
freshenDeriv :: Deriv -> Deriv
freshenDeriv (Deriv d) = do
  let rho = Map.fromFoldable <<< Array.concat $ d.params <#> \(Param p) -> case p.bind of
        _ /\ Just _ -> []
        x /\ Nothing -> [x /\ freshMVar (getMVarMaybeVar x)]
  renameDerivMVars rho (Deriv d)

-- has neither hypotheses nor metavariables
isFinished :: Deriv -> Boolean
isFinished (Deriv d) = do
  if List.null d.hyps then do
    let xs = bifoldMap (const []) (either (const []) (Array.singleton)) d.con
    Array.null xs
  else false
