module Language.Mfl.Generate.Internal.Unification where

import Data.Tuple.Nested
import Language.Mfl.Core.Ast
import Language.Mfl.Generate.Internal.Base
import Prelude

import Control.Monad.Computation (ComputationT, gets, modify, throwError, tryComputation)
import Data.Bifunctor (bimap)
import Data.Bot (elimBot)
import Data.Either (Either(..), isRight)
import Data.Foldable (and, or)
import Data.Foldable as Foldable
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Hole (hole)
import Language.Mfl.Core.ModuleT (ModuleT)
import Record as R
import Text.Pretty (pretty, ticks)
import Type.Proxy (Proxy(..))

type UnifyT m = ComputationT "Unify" UnifyCtx UnifyEnv UnifyErr (ModuleT m)

type UnifyCtx = {}

type UnifyEnv = 
  { sigma :: TermSub }

type UnifyErr = String

unifyTerm :: forall m. Monad m => SymbolicTerm -> EvaluatedTerm -> UnifyT m Unit

unifyTerm (Term (NeuTerm (Right x) _args) _sig1) term2 =
  gets (_.sigma >>> Foldable.lookup x) >>= case _ of
    Nothing -> throwError ("unifyTerm: Unknown term name: " <> ticks (pretty x))
    Just term1' -> unifyTerm (fromEvaluatedTerm term1') term2

unifyTerm (Term (QuantTerm x) _sig1) term2 = do
  void (modify (R.modify (Proxy :: Proxy "sigma") (List.Cons (x /\ term2))))

unifyTerm (Term (ConstrTerm (NatConstr ZeroConstr)) _sig1) (Term (ConstrTerm (NatConstr ZeroConstr)) _sig2) = pure unit
unifyTerm (Term (ConstrTerm (NatConstr InfinityConstr)) _sig1) (Term (ConstrTerm (NatConstr InfinityConstr)) _sig2) = pure unit
unifyTerm (Term (ConstrTerm (NatConstr (SucConstr t1))) _sig1) (Term (ConstrTerm (NatConstr (SucConstr t2))) _sig2) = unifyTerm t1 t2

unifyTerm (Term (ConstrTerm (BoolConstr b1)) _sig1) (Term (ConstrTerm (BoolConstr b2)) _sig2) | b1 == b2 = pure unit

unifyTerm (Term (ConstrTerm (StringConstr s1)) _sig1) (Term (ConstrTerm (StringConstr s2)) _sig2) | s1 == s2 = pure unit

-- This is very tricky, since there could be multiple ways to unify sets
unifyTerm term1@(Term (ConstrTerm (SetConstr (LiteralSetConstr ts1))) _sig1) term2@(Term (ConstrTerm (SetConstr (LiteralSetConstr ts2))) _sig2) = do
  -- need a 1-to-1 correspondence of elements
  -- result <- and <$> for ts1 \t1 -> or <$> for ts2 \t2 -> (tryComputation (unifyTerm t1 t2) <#> isRight)
  result <- hole "TODO"
  if result 
    then pure unit
    else throwError ("unifyTerm: Cannot unify " <> ticks (pretty term1) <> " with " <> ticks (pretty term2))
unifyTerm (Term (ConstrTerm (SetConstr DomainConstr)) _sig1) (Term (ConstrTerm (SetConstr DomainConstr)) _sig2) = pure unit

unifyTerm (Term (ConstrTerm (TupleConstr (MakeTupleConstr a1 b1))) _sig1) (Term (ConstrTerm (TupleConstr (MakeTupleConstr a2 b2))) _sig2) = do
  unifyTerm a1 a2
  unifyTerm b1 b2

unifyTerm term1 term2 = throwError ("unifyTerm: Cannot unify " <> ticks (pretty term1) <> " with " <> ticks (pretty term2))



