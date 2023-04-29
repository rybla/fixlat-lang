module Language.Fixlat.Deriv where

import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Grammar
import Prelude
import Text.Pretty

import Control.Monad.Reader (class MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (class MonadState, StateT, get, gets, modify_, put, runStateT)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Bug (bug)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM, foldMap, foldr, intercalate, sequence_, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Lattice (class JoinSemilattice, class Lattice, class MeetSemilattice, class PartialOrd, comparePartial)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)
import Record as R
import Text.Pretty (class Pretty)
import Type.Proxy (Proxy(..))

-- | A derivation is a tree.
data Deriv xt = Deriv
  { label :: Label
  , params :: Array (Param ((xt /\ LTerm xt) \/ xt))
  , derivsRev :: List (Deriv xt)
  , hyps :: List (Prop CLat xt)
  , con :: Prop CLat xt
  }

instance Pretty xt => Pretty (Deriv xt) where
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
      Left (x /\ t) -> pretty p.quant <+> pretty x <> "=" <> pretty t
      Right x -> pretty p.quant <+> pretty x <> ":" <+> pretty p.type_

derive instance Generic (Deriv xt) _
instance Show xt => Show (Deriv xt) where show x = genericShow x
derive instance Functor Deriv
derive instance Foldable Deriv 
derive instance Traversable Deriv

newtype Derivs xt = Derivs (Array (Deriv xt))
derive instance Newtype (Derivs xt) _
derive newtype instance Show a => Show (Derivs a)

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
    case runUnify' (unifyLeDeriv d1 d2) of
      Just st -> 
        -- d1 is alpha-equivalent to d2
        if isRenaming st.exiSigma && isRenaming st.uniSigma then Just EQ
        -- d1 is less general than d2 via sigma
        else Just LT
      Nothing -> case runUnify' (unifyLeDeriv d2 d1) of
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

totalSigma :: forall xt. Ord xt => UnifyM xt (Map.Map xt (LTerm xt))
totalSigma = do
  exiSigma <- gets _.exiSigma
  uniSigma <- gets _.uniSigma
  pure $ exiSigma `Map.union` uniSigma

runUnify :: forall xt a. UnifyCtx xt -> UnifySt xt -> UnifyM xt a -> Maybe (UnifySt xt)
runUnify ctx st m = snd <$> runStateT (runReaderT m ctx) st

runUnify' :: forall xt a. UnifyM xt a -> Maybe (UnifySt xt)
runUnify' = runUnify Map.empty {exiSigma: Map.empty, uniSigma: Map.empty}

tryUnify :: forall xt a. UnifyM xt a -> UnifyM xt (Maybe a)
tryUnify m = do
  st <- get
  ctx <- ask
  case flip runReaderT ctx >>> flip runStateT st $ m of
    Nothing -> pure Nothing
    Just (a /\ st) -> do
      put st
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
  Deriv d1 <- substDeriv deriv1 <$> totalSigma
  Deriv d2 <- substDeriv deriv2 <$> totalSigma
  -- intro quantifications
  let introParam (Param p) = case p.bind of
        Left _ -> identity
        Right x -> Map.insert x p.quant
  local (flip (foldr introParam) (d1.params <> d2.params)) do
    -- check that each d1 hyp is MORE general than at least one d2 hyp
    flip traverse_ d1.hyps \hyp1 ->
      anyUnify (d2.hyps <#> \hyp2 -> unifyLeLProp hyp2 hyp1)
    -- check that d1 con is LESS general than d2 con 
    unifyLeLProp d1.con d2.con

unifyLeLProp :: forall xt. Ord xt => LProp xt -> LProp xt -> UnifyM xt Unit
unifyLeLProp (Prop p1) (Prop p2) | p1.pred /= p2.pred = empty
unifyLeLProp (Prop p1) (Prop p2) = unifyLeLTerm p1.arg p2.arg

-- | Check that there exists substitutions uniSigma, exiSigma such that
-- term1[exiSigma] = term2[uniSigma]
unifyLeLTerm :: forall xt. Ord xt => LTerm xt -> LTerm xt -> UnifyM xt Unit
unifyLeLTerm term1 term2 = do
  term1' <- substLTerm term1 <$> totalSigma
  term2' <- substLTerm term2 <$> totalSigma
  case term1' /\ term2' of
    VarTerm x1 _ /\ _ -> asks (Map.lookup x1) >>= case _ of
      Nothing -> bug $ "[unifyLeLTerm] when looking up quantifier, unknown variable"
      -- substitute existentials on the left
      Just ExistQuant -> modify_ $ R.modify _exiSigma $ Map.insert x1 term2'
      Just _ -> go term1' term2'
    _ /\ VarTerm x2 _ -> asks (Map.lookup x2) >>= case _ of
      Nothing -> bug $ "[unifyLeLTerm] when looking up quantifier, unknown variable"
      -- substitute universals on the right
      Just UnivQuant -> modify_ $ R.modify _uniSigma $ Map.insert x2 term1'
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
  { params = d.params <#> \(Param p) -> case p.bind of
      Left (x /\ t) -> case Map.lookup x sigma of
        Nothing -> Param p
        Just t' -> unsafeCrashWith "attempted to substitute a parameter in a deriv that's already been substituted"
      Right x -> case Map.lookup x sigma of
        Nothing -> Param p
        Just t -> Param p {bind = Left (x /\ t)}
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
  , params: rule.params <#> map pure
  , derivsRev: mempty
  , hyps: List.fromFoldable rule.hyps
  , con: rule.con
  }
