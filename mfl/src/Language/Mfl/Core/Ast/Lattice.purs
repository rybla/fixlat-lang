-- | Lattice over AST.
module Language.Mfl.Core.Ast.Lattice where

import Data.Tuple.Nested
import Language.Mfl.Core.Ast
import Prelude
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (uncurry)
import Language.Mfl.Core.Ast.Pattern (matchEvaluatedTerms)

compareProp :: EvaluatedProp -> EvaluatedProp -> Maybe Ordering
compareProp (Prop r1 t1) (Prop r2 t2) = 
  if r1 == r2 
    then compareTerm t1 t2
    else Nothing

compareTerm :: EvaluatedTerm -> EvaluatedTerm -> Maybe Ordering
compareTerm = matchEvaluatedTerms "compareTerm"
  { op: map case _ of
      LT -> GT 
      EQ -> EQ
      GT -> LT
  , bool: Just <<< uncurry compare
  , string: Just <<< case _ of
      LiteralStringConstr s1 /\ LiteralStringConstr s2 -> compare s1 s2
      LiteralStringConstr _ /\ ZetaConstr -> LT
      ZetaConstr /\ LiteralStringConstr _ -> GT
      ZetaConstr /\ ZetaConstr -> EQ
  , set: case _ of
      LiteralSetConstr s1 /\ LiteralSetConstr s2 -> 
        let set1 = Set.fromFoldable s1 in
        let set2 = Set.fromFoldable s2 in
        if set1 == set2 then Just EQ else
        if Set.subset set1 set2 then Just LT else
        if Set.subset set2 set1 then Just GT else
        Nothing
      LiteralSetConstr _ /\ DomainConstr -> Just LT
      DomainConstr /\ LiteralSetConstr _ -> Just GT
      DomainConstr /\ DomainConstr -> Just EQ
  , tuple: \((MakeTupleConstr x1 y1) /\ (MakeTupleConstr x2 y2)) ->
      compareTerm x1 x2 >>= case _ of
        LT -> Just LT
        EQ -> compareTerm y1 y2
        GT -> Just GT
  }
