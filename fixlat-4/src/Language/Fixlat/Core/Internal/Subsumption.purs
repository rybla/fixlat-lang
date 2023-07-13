module Language.Fixlat.Core.Internal.Subsumption where

import Control.Monad.Trans.Class
import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Assert (assert, assertI)
import Control.Assert.Assertions (equal)
import Control.Bug (bug)
import Control.Debug (debugA)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, gets)
import Data.Array as Array
import Data.Foldable as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Evaluation (evaluate, evaluate', shallowEvaluate)
import Language.Fixlat.Core.ModuleT (ModuleT)
import Text.Pretty (pretty, ticks)
import Utility (allArrayM, anyArrayM, anyListM)

class Subsumable a where
  subsumes :: forall m. MonadEffect m => a -> a -> GenerateT m Boolean

instance Subsumable Patch where
  subsumes (ApplyPatch rule1) (ApplyPatch rule2) = rule1 `subsumes` rule2
  subsumes (ConclusionPatch prop1) (ConclusionPatch prop2) = prop1 `subsumes` prop2
  subsumes _ _ = pure false

isSubsumedPatch :: forall m. MonadEffect m => Patch -> GenerateT m Boolean  
isSubsumedPatch (ApplyPatch rule) = do
  rules <- gets _.rules
  rules # anyListM \rule' -> subsumes rule rule'

isSubsumedPatch (ConclusionPatch prop) = do
  Database props <- gets _.database
  -- TODO: this is simpler now, since only one prop per relation
  props # anyListM \prop' -> subsumes prop prop'

instance Subsumable NormInstRule where
  subsumes rule1 rule2 = pure $ rule1 == rule2

instance Subsumable G.ConcreteProposition where
  subsumes (G.Proposition rel1 arg1) (G.Proposition rel2 arg2)
    | rel1 /= rel2 = pure false
    | otherwise = arg1 `subsumes` arg2

-- | Assumes that the terms have been shallowly evaluated.
subsumesConcreteTerm' :: forall m. MonadEffect m => 
  G.ConcreteTerm -> G.ConcreteTerm ->
  GenerateT m Boolean

subsumesConcreteTerm' term1 term2 
  | lat <- G.typeOfTerm term1
  , lat' <- G.typeOfTerm term2
  , lat /= lat' 
  = bug $ "[subsumes] Expected terms to be of the same lattice: " <> ticks (pretty term1 <> " : " <> pretty lat) <> ", " <> ticks (pretty term2 <> " : " <> pretty lat')

subsumesConcreteTerm' term1@(G.ApplicationTerm _ _ _) term2 = bug $ "[subsumes] Expected term to be normalized before checking subsumption between " <> ticks (pretty term1) <> " and " <> ticks (pretty term2) <> "."
subsumesConcreteTerm' term1 term2@(G.ApplicationTerm _ _ _) = bug $ "[subsumes] Expected term to be normalized before checking subsumption between " <> ticks (pretty term1) <> " and " <> ticks (pretty term2) <> "."

-- valid cases, which assume:
--   • neither term is a `G.ApplicationTerm`
--   • terms are of the same `G.LatticeType`

subsumesConcreteTerm' term1 term2 
  | G.OpLatticeType lat <- G.typeOfTerm term1
  = subsumes term2 term1

subsumesConcreteTerm' 
  (G.ConstructorTerm G.ZeroConstructor [_] G.NatLatticeType)
  (G.ConstructorTerm G.ZeroConstructor [] G.NatLatticeType) = 
  pure false

subsumesConcreteTerm' 
  (G.ConstructorTerm G.ZeroConstructor [_] G.NatLatticeType)
  (G.ConstructorTerm G.SucConstructor [_] G.NatLatticeType) = 
  pure false

subsumesConcreteTerm' 
  (G.ConstructorTerm G.SucConstructor [_] G.NatLatticeType)
  (G.ConstructorTerm G.ZeroConstructor [] G.NatLatticeType) = 
  pure true

subsumesConcreteTerm'
  (G.ConstructorTerm G.SucConstructor [n1] G.NatLatticeType)
  (G.ConstructorTerm G.SucConstructor [n2] G.NatLatticeType) = 
  subsumes n1 n2

subsumesConcreteTerm'
  (G.ConstructorTerm (G.StringConstructor s1) [] G.StringLatticeType)
  (G.ConstructorTerm (G.StringConstructor s2) [] G.StringLatticeType) = 
  pure $ s1 <= s2

subsumesConcreteTerm'
  (G.ConstructorTerm (G.BoolConstructor b1) [] G.BoolLatticeType)
  (G.ConstructorTerm (G.BoolConstructor b2) [] G.BoolLatticeType) =
  pure $ b1 <= b2

subsumesConcreteTerm'
  (G.ConstructorTerm G.TupleConstructor [a1, b1] (G.TupleLatticeType G.LexicographicTupleOrdering  _aLat  _bLat))
  (G.ConstructorTerm G.TupleConstructor [a2, b2] (G.TupleLatticeType G.LexicographicTupleOrdering __aLat __bLat)) =
  a1 `subsumes` a2 >>= if _
    then a2 `subsumes` a1 >>= if _
      -- a1 == a2, so need to check b1 vs b2
      then b1 `subsumes` b2
      -- a1 > a2
      else pure true
    -- a1 < a2
    else pure false

subsumesConcreteTerm'
  (G.ConstructorTerm G.TupleConstructor [a1, b1] (G.TupleLatticeType G.ParallelTupleOrdering  _aLat  _bLat))
  (G.ConstructorTerm G.TupleConstructor [a2, b2] (G.TupleLatticeType G.ParallelTupleOrdering __aLat __bLat)) = do
  c1 <- a1 `subsumes` a2
  c2 <- b1 `subsumes` b2
  pure (c1 && c2)

-- In the power set lattice, `xs1` subsumes `xs2` if every element of `xs2`
-- is equal to some element in `xs1`.
subsumesConcreteTerm' 
  (G.ConstructorTerm G.SetConstructor xs1 (G.PowerLatticeType _))
  (G.ConstructorTerm G.SetConstructor xs2 (G.PowerLatticeType _)) = do
  -- xs1' <- evaluate `traverse` xs1
  -- xs2' <- evaluate `traverse` xs2
  -- pure $ Array.all (\x2 -> Array.any (\x1 -> x1 == x2) xs1') xs2'

  debugA $ "subsumption check: " <> pretty xs1 <> " ⊑ " <> pretty xs2
  xs1' <- evaluate `traverse` xs1
  xs2' <- evaluate `traverse` xs2
  
  result <- pure $ Array.all (\x2 -> Array.any (\x1 -> x1 == x2) xs1') xs2'

  debugA $ "===> " <> show result

  pure result

subsumesConcreteTerm' (G.BoundTerm name _) term2 = do
  term1 <- (List.lookup name <$> asks _.sigma) >>= case _ of
    Nothing -> bug $ "[subsumes] Expected bound variable to be in local substitution: " <> ticks (pretty name)
    Just term1 -> pure term1
  subsumes term1 term2

subsumesConcreteTerm' (G.QuantTerm name _) _ = absurd name
subsumesConcreteTerm' _ (G.QuantTerm name _) = absurd name

subsumesConcreteTerm' term1 term2 = bug $ "[subsumes] Unhandled subsumption checking pair: " <> ticks (pretty term1) <> ", " <> ticks (pretty term2)

instance Subsumable G.ConcreteTerm where

  -- TODO: first shallow evaluate, then use subsumesConcreteTerm'
  subsumes term1 term2 = do
    -- term1' <- shallowEvaluate term1
    -- term2' <- shallowEvaluate term2
    -- subsumesConcreteTerm' term1' term2'

    debugA $ "subsumes: " <> pretty term1 <> " <= " <> pretty term2
    term1' <- shallowEvaluate term1
    term2' <- shallowEvaluate term2
    result <- subsumesConcreteTerm' term1' term2'
    debugA $ "===> " <> show result
    pure result
