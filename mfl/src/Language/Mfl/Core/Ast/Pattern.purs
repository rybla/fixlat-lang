module Language.Mfl.Core.Ast.Pattern where

import Language.Mfl.Core.Ast
import Data.Tuple.Nested
import Prelude
import Bug (bug)
import Data.Bot (Bot, elimBot)
import Text.Pretty (ticks)

matchEvaluatedTerms :: forall a.
  String -> 
  { op :: a -> a
  , bool :: BoolConstr /\ BoolConstr -> a 
  , string :: StringConstr /\ StringConstr -> a
  , set :: SetConstr EvaluatedTerm  /\ SetConstr EvaluatedTerm -> a
  , tuple :: TupleConstr EvaluatedTerm /\ TupleConstr EvaluatedTerm -> a
  } ->
  EvaluatedTerm -> EvaluatedTerm -> a

matchEvaluatedTerms _ _ (Term (NeuTerm f _) _) _ = elimBot f
matchEvaluatedTerms _ _ _ (Term (NeuTerm f _) _) = elimBot f

matchEvaluatedTerms source _ term1@(Term _ ty1) term2@(Term _ ty2) | ty1 /= ty2 = 
  bug $ "[matchEvaluatedTerms/" <> source <> "] Type mismatch between matched terms: " <> ticks (show term1) <> ", " <> ticks (show term2) <> "."

matchEvaluatedTerms source f@{op} (Term pt1 (OpType _ ty)) (Term pt2 (OpType _ _)) = op (matchEvaluatedTerms source f (Term pt1 ty) (Term pt2 ty))
matchEvaluatedTerms _ {bool} (Term (ConstrTerm (BoolConstr b1)) BoolType) (Term (ConstrTerm (BoolConstr b2)) BoolType) = bool (b1 /\ b2)
matchEvaluatedTerms _ {string} (Term (ConstrTerm (StringConstr s1)) StringType) (Term (ConstrTerm (StringConstr s2)) StringType) = string (s1 /\ s2)
matchEvaluatedTerms _ {set} (Term (ConstrTerm (SetConstr s1)) (SetType _)) (Term (ConstrTerm (SetConstr s2)) (SetType _)) = set (s1 /\ s2)
matchEvaluatedTerms _ {tuple} (Term (ConstrTerm (TupleConstr t1)) (TupleType _ _)) (Term (ConstrTerm (TupleConstr t2)) (TupleType _ _)) = tuple (t1 /\ t2)

matchEvaluatedTerms source _ term1 term2 =
  bug $ "[matchEvaluatedTerms/" <> source <> "] Unexpected case: " <> ticks (show term1) <> ", " <> ticks (show term2) <> "."

