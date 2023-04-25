module Language.Fixlat.Deriv where

import Language.Fixlat.Grammar
import Prelude

import Control.Monad.Reader (class MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (class MonadState, StateT, get, gets, modify_, put, runStateT)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bug (bug)
import Data.Foldable (class Foldable, foldM, foldMap, foldr, sequence_, traverse_)
import Data.Lattice (class JoinSemilattice, class Lattice, class MeetSemilattice, class PartialOrd, comparePartial)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Record as Record
import Type.Proxy (Proxy(..))

-- | A derivation is a tree.
data Deriv xt = Deriv
  { label :: Label
  , params :: Array (Param xt)
  , derivsRev :: List (Deriv xt)
  , hyps :: List (Prop CLat xt)
  , con :: Prop CLat xt
  }

derive instance Functor Deriv
derive instance Foldable Deriv 
derive instance Traversable Deriv

newtype Derivs xt = Derivs (Array (Deriv xt))
derive instance Newtype (Derivs xt) _

singleton :: forall xt. Deriv xt -> Derivs xt
singleton deriv = Derivs (Array.singleton deriv)

-- fromSingleton :: forall xt. Derivs xt -> Deriv xt
-- fromSingleton deriv = Derivs (Array.singleton deriv)

-- asSingleton :: forall xt. (Derivs xt -> Derivs xt) -> Deriv xt -> Deriv xt
-- asSingleton f deriv = 

concat :: forall xt. Array (Derivs xt) -> Array (Deriv xt)
concat = foldMap unwrap

instance Ord xt => PartialOrd (Deriv xt) where
  -- d1 <= d2  <==>  d1 has more general hypotheses and more specific conclusion
  comparePartial d1 d2 =
    case runUnify (unifyLeDeriv d1 d2) of
      Just st -> 
        -- d1 is alpha-equivalent to d2
        if isRenaming st.exiSigma && isRenaming st.uniSigma then Just EQ
        -- d1 is less general than d2 via sigma
        else Just LT
      Nothing -> case runUnify (unifyLeDeriv d2 d1) of
        Just st -> 
          -- d1 is alpha-equivalent to d2
          if isRenaming st.exiSigma && isRenaming st.uniSigma then Just EQ
          -- d1 is more general than d2 via sigma
          else Just GT
        Nothing -> Nothing

isRenaming :: forall xt. Ord xt => Map.Map xt (LTerm xt) -> Boolean
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

type UnifyM xt = ReaderT (UnifyCtx xt) (StateT (UnifySt xt) Maybe)

type UnifyCtx xt = Map.Map xt Quant
type UnifySt xt = {exiSigma :: Map.Map xt (LTerm xt), uniSigma :: Map.Map xt (LTerm xt)}

_exiSigma = Proxy :: Proxy "exiSigma"
_uniSigma = Proxy :: Proxy "uniSigma"

runUnify :: forall xt a. UnifyM xt a -> Maybe (UnifySt xt)
runUnify = flip runReaderT Map.empty >>> flip runStateT {exiSigma: Map.empty, uniSigma: Map.empty} >>> map snd

tryUnify :: forall xt a. UnifyM xt a -> UnifyM xt (Maybe a)
tryUnify m = do
  st <- get
  ctx <- ask
  case flip runReaderT ctx >>> flip runStateT st $ m of
    Nothing -> pure Nothing
    Just (a /\ sigma) -> do
      put sigma
      pure (Just a)

anyUnify :: forall f xt a. Foldable f => f (UnifyM xt a) -> UnifyM xt a
anyUnify ms = foldM f Nothing ms >>= case _ of
  Nothing -> empty
  Just a -> pure a
  where 
  f (Just a) _ = pure (Just a) -- already found a passing unification
  f Nothing m = tryUnify m -- haven't succeeded yet, so try this one

-- constravariant in hypotheses
unifyLeDeriv :: forall xt. Ord xt => Deriv xt -> Deriv xt -> UnifyM xt Unit
unifyLeDeriv deriv1 deriv2 = do
  Deriv d1 <- substDeriv deriv1 <$> gets _.exiSigma
  Deriv d2 <- substDeriv deriv2 <$> gets _.uniSigma
  -- intro quantifications
  local (flip (foldr (\p -> Map.insert p.bind p.quant)) (d1.params <> d2.params)) do
    -- check that each d1 hyp is MORE general than at least one d2 hyp
    flip traverse_ d1.hyps \hyp1 ->
      anyUnify (d2.hyps <#> \hyp2 -> unifyLeLProp hyp2 hyp1)
    -- check that d1 con is LESS general than d2 con 
    unifyLeLProp d1.con d2.con

unifyLeLProp :: forall xt. Ord xt => LProp xt -> LProp xt -> UnifyM xt Unit
unifyLeLProp (Prop p1) (Prop p2) | p1.pred /= p2.pred = empty
unifyLeLProp (Prop p1) (Prop p2) = unifyLeLTerm p1.arg p2.arg

-- | Check that there exists substitutions sigmaUni, sigmaExi such that
-- term1[sigmaExi] = term2[sigmaUni]
unifyLeLTerm :: forall xt. Ord xt => LTerm xt -> LTerm xt -> UnifyM xt Unit
unifyLeLTerm term1 term2 = do
  term1' <- substLTerm term1 <$> gets _.exiSigma
  term2' <- substLTerm term2 <$> gets _.uniSigma
  case term1' /\ term2' of
    VarTerm x1 _ /\ _ -> asks (Map.lookup x1) >>= case _ of
      Nothing -> bug $ "[unifyLeLTerm] when looking up quantifier, unknown variable"
      -- substitute existentials on the left
      Just ExistQuant -> modify_ $ Record.modify _exiSigma $ Map.insert x1 term2'
      Just _ -> go term1' term2'
    _ /\ VarTerm x2 _ -> asks (Map.lookup x2) >>= case _ of
      Nothing -> bug $ "[unifyLeLTerm] when looking up quantifier, unknown variable"
      -- substitute universals on the right
      Just UnivQuant -> modify_ $ Record.modify _uniSigma $ Map.insert x2 term1'
      Just ExistQuant -> go term1' term2'
    _ -> go term1' term2'
  where
  go :: LTerm xt -> LTerm xt -> UnifyM xt Unit
  go (AtomicTerm at1 _) (AtomicTerm at2 _) = if at1 == at2 then pure unit else empty
  go (ProdTerm ta1 tb1 _) (ProdTerm ta2 tb2 _) = sequence_ [unifyLeLTerm ta1 ta2, unifyLeLTerm tb1 tb2]
  go (Inj1Term t1 _) (Inj1Term t2 _) = unifyLeLTerm t1 t2
  go (Inj2Term t1 _) (Inj2Term t2 _) = unifyLeLTerm t1 t2
  go (VarTerm _ _) _ = bug $ "[unifyLeLTerm.go] impossible case, should VarTerm should have already been handled"
  go _ (VarTerm _ _) = bug $ "[unifyLeLTerm.go] impossible case, should VarTerm should have already been handled"
  go _ _ = empty

substDeriv :: forall xt. Ord xt => Deriv xt -> Map.Map xt (LTerm xt) -> Deriv xt
substDeriv (Deriv d) sigma = Deriv d
  { params = Array.filter (_.bind >>> (flip Map.member sigma) >>> not) d.params
  , derivsRev = flip substDeriv sigma <$> d.derivsRev
  , hyps = flip substLProp sigma <$> d.hyps
  , con = substLProp d.con sigma
  }

substLProp :: forall xt. Ord xt => LProp xt -> Map.Map xt (LTerm xt) -> LProp xt
substLProp (Prop p) sigma = Prop p {arg = substLTerm p.arg sigma}

substLTerm :: forall xt. Ord xt => LTerm xt -> Map.Map xt (LTerm xt) -> LTerm xt
substLTerm t@(AtomicTerm _ _) _sigma = t
substLTerm t@(VarTerm x _) sigma = Map.lookup x sigma # fromMaybe t
substLTerm (ProdTerm ta tb y) sigma = ProdTerm (substLTerm ta sigma) (substLTerm tb sigma) y
substLTerm (Inj1Term t y) sigma = Inj1Term (substLTerm t sigma) y
substLTerm (Inj2Term t y) sigma = Inj2Term (substLTerm t sigma) y

instance Ord xt => PartialOrd (Derivs xt) where 
  comparePartial (Derivs ds1) (Derivs ds2) = 
    (\f1 -> foldr f1 (Just EQ) ds1) \d1 mb_ord ->
      (\f2 -> foldr f2 mb_ord ds2) \d2 mb_ord' ->
        f d1 d2 =<< mb_ord' 
    where
    f :: Deriv xt -> Deriv xt -> Ordering -> Maybe Ordering
    f deriv1 deriv2 ord = case comparePartial deriv1 deriv2 of
      Just ord' | ord == ord' -> Just ord
      _ -> Nothing

instance Ord xt => MeetSemilattice (Derivs xt) where 
  meet (Derivs ds1) (Derivs ds2) = Derivs (ds1 <> ds2)

instance Ord xt => JoinSemilattice (Derivs xt) where 
  join (Derivs ds1) (Derivs ds2) = Derivs (ds1 <> ds2)

instance Ord xt => Lattice (Derivs xt)

fromRuleToDeriv :: forall xt. LRule xt -> Deriv xt
fromRuleToDeriv (Rule rule) = Deriv
  { label: rule.label
  , params: rule.params
  , derivsRev: mempty
  , hyps: List.fromFoldable rule.hyps
  , con: rule.con
  }
