module Language.Mfl.Generate.Internal.Evaluation where

import Data.Either.Nested
import Data.Tuple.Nested
import Language.Mfl.Core.Ast
import Prelude
import Control.Monad.Computation
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Bot (elimBot)
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Language.Mfl.Core.ModuleT (ModuleT, getFunctionSpec)
import Text.Pretty (pretty, ticks)

--------------------------------------------------------------------------------
-- EvaluationT
--------------------------------------------------------------------------------

type EvaluationT m = ComputationT "Evaluation" EvaluationCtx EvaluationEnv EvaluationErr (ModuleT m)

type EvaluationCtx =
  { sigma :: TermSub }

type EvaluationEnv = {}

type EvaluationErr = String

runEvaluationT :: forall m a. Monad m => _ -> EvaluationT m a -> ModuleT m ((EvaluationErr \/ a) /\ EvaluationEnv)
runEvaluationT {sigma} = runComputationT 
  { sigma } 
  {}

--------------------------------------------------------------------------------
-- TermSub
--------------------------------------------------------------------------------

type TermSub = List (Tuple TermName EvaluatedTerm)

concretizeProp :: forall m. Monad m => SymbolicProp -> EvaluationT m ConcreteProp
concretizeProp (Prop rel arg) = Prop rel <$> concretizeTerm arg

concretizeTerm :: forall m. Monad m => SymbolicTerm -> EvaluationT m ConcreteTerm
concretizeTerm (Term (NeuTerm f args) sig) = Term <$> (NeuTerm f <$> (concretizeTerm `traverse` args)) <*> pure sig
concretizeTerm (Term (ConstrTerm (NatConstr n)) sig) = Term <$> (ConstrTerm <$> (NatConstr <$> (concretizeTerm `traverse` n))) <*> pure sig 
concretizeTerm (Term (ConstrTerm (StringConstr s)) sig) = pure (Term (ConstrTerm (StringConstr s)) sig)
concretizeTerm (Term (ConstrTerm (BoolConstr b)) sig) = pure (Term (ConstrTerm (BoolConstr b)) sig)
concretizeTerm (Term (ConstrTerm (SetConstr s)) sig) = Term <$> (ConstrTerm <$> (SetConstr <$> (concretizeTerm `traverse` s))) <*> pure sig 
concretizeTerm (Term (ConstrTerm (TupleConstr t)) sig) = Term <$> (ConstrTerm <$> (TupleConstr <$> (concretizeTerm `traverse` t))) <*> pure sig 
concretizeTerm (Term (QuantTerm x) _sig) = do
  -- `x` should be substituted by `sigma`
  asks (_.sigma >>> Foldable.lookup x) >>= case _ of
    Nothing -> throwError ("concretizeTerm: Found a free variable: " <> ticks (pretty x))
    Just t -> pure (rmap elimBot t)

evaluateProp :: forall m. Monad m => ConcreteProp -> EvaluationT m EvaluatedProp
evaluateProp (Prop rel arg) = Prop rel <$> evaluateTerm arg

evaluateTerm :: forall m. Monad m => ConcreteTerm -> EvaluationT m EvaluatedTerm
evaluateTerm (Term (NeuTerm (Left f) args) _sig) = do
  FunctionSpec funSpec <- liftComputationT (getFunctionSpec f)
  case funSpec.internalImplementation of
    Nothing -> throwError ("No internal implementation for function: " <> ticks (pretty f))
    Just eval -> do
      args' <- evaluateTerm `traverse` args
      pure (eval args')
evaluateTerm term@(Term (NeuTerm (Right x) args) _sig) =
  asks (_.sigma >>> Foldable.lookup x) >>= case _ of
    Nothing -> throwError ("evaluateTerm: Unknown name: " <> ticks (pretty x))
    Just t -> do
      when (Array.length args /= 0) (throwError ("evaluateTerm: A user-defined value cannot be applied to arguments: " <> ticks (pretty term)))
      pure t
evaluateTerm (Term (ConstrTerm (NatConstr n)) sig) = Term <$> (ConstrTerm <$> (NatConstr <$> (evaluateTerm `traverse` n))) <*> pure sig 
evaluateTerm (Term (ConstrTerm (StringConstr s)) sig) = pure (Term (ConstrTerm (StringConstr s)) sig)
evaluateTerm (Term (ConstrTerm (BoolConstr b)) sig) = pure (Term (ConstrTerm (BoolConstr b)) sig)
evaluateTerm (Term (ConstrTerm (SetConstr s)) sig) = Term <$> (ConstrTerm <$> (SetConstr <$> (evaluateTerm `traverse` s))) <*> pure sig 
evaluateTerm (Term (ConstrTerm (TupleConstr t)) sig) = Term <$> (ConstrTerm <$> (TupleConstr <$> (evaluateTerm `traverse` t))) <*> pure sig 
evaluateTerm (Term (QuantTerm x) _sig) = elimBot x
