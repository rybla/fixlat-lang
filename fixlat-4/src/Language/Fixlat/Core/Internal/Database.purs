module Language.Fixlat.Core.Internal.Database where

import Data.Lattice
import Data.Tuple.Nested
import Language.Fixlat.Core.Internal.Base
import Prelude hiding (join)

import Control.Bug (bug)
import Control.Debug (debugA)
import Control.Debug as Debug
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..), either, isRight)
import Data.Either.Nested (type (\/))
import Data.List (List(..), (:))
import Data.List as List
import Data.Make (make)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Traversable (for, traverse)
import Effect.Class (class MonadEffect)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Evaluation (evaluate, shallowEvaluate)
import Language.Fixlat.Core.Internal.Normalization (normalize)
import Language.Fixlat.Core.Internal.Substitution (substitute)
import Language.Fixlat.Core.Internal.Unification (unify)
import Record as R
import Text.Pretty (braces, pretty, ticks)
import Type.Proxy (Proxy(..))
import Utility (anyListM)
import Utility (anyListM, (<$$>), (<##>))

-- | Get the highest known proposition of a relation in a database.
getHighestDb :: forall m. Monad m => G.RelationName -> Database -> m G.ConcreteProposition
getHighestDb rel (Database props) = do
  case Map.lookup rel props of
    Nothing -> bug $ "No entry in current database for relation: " <> ticks (pretty rel)
    Just prop -> pure prop

-- | Insert a proposition into a database.
insertPropositionDb :: forall m. Monad m => G.ConcreteProposition -> Database -> m (Proxy "inserted proposition was subsumed" \/ Database)
insertPropositionDb prop db = do
  let rel = G.propositionRelationName prop
  prop' <- getHighestDb rel db
  let prop'' = join prop prop' 
  if prop' == prop'' 
    then pure $ Left Proxy
    else pure $ Right (over Database (Map.insert rel prop'') db)

-- | Insert a proposition into the current database.
insertProposition :: forall m. MonadEffect m => 
  G.ConcreteProposition ->
  GenerateT m (Proxy "inserted proposition was subsumed" \/ Unit)
insertProposition prop = do
  db <- gets _.database
  liftGenerateT (insertPropositionDb prop db) >>= case _ of
    Left pxy -> pure $ Left pxy
    Right db' -> Right <$> modify_ (R.set _database db')
  
-- | Get the highest known proposition of a relation in the current database.
getHighest :: forall m. MonadEffect m => G.RelationName -> GenerateT m G.ConcreteProposition
getHighest rel = do
  db <- gets _.database
  liftGenerateT $ getHighestDb rel db

-- | Learn a patch to the current database.
learnPatch :: forall m. MonadEffect m =>
  Patch ->
  GenerateT m (Proxy "learned patch was subsumed" \/ List Patch)
learnPatch (ConclusionPatch prop) = do
  insertProposition prop >>= case _ of
    Left _ -> pure (Left Proxy)
    Right _ -> do
      -- yield all the rules that could apply to this prop
      rules <- gets _.rules
      Right <<< List.concat <$> 
        for rules \rule -> canApplyNormInstRuleToProposition rule prop >>= if _
          then pure (List.singleton (ApplyPatch rule))
          else pure Nil
learnPatch (ApplyPatch rule) = do
  modify_ $ R.modify _rules (rule : _)
  Right <$> applyNormInstRule rule

-- | Apply a rule to the current database, yielding the resulting patches.
applyNormInstRule :: forall m. MonadEffect m =>
  NormInstRule ->
  GenerateT m (List Patch)
applyNormInstRule rule = do
  Database props <- gets _.database
  List.foldMap (either (const Nil) List.singleton) <$> 
    for props \prop -> applyNormInstRuleToProposition rule prop

-- | Apply a rule to a proposition, yielding the resulting patch if applicable.
applyNormInstRuleToProposition :: forall m. MonadEffect m =>
  NormInstRule ->
  G.ConcreteProposition ->
  GenerateT m (String \/ Patch)
applyNormInstRuleToProposition (NormInstRule rule) prop = do
  unify rule.premise prop >>= case _ of
    Left err -> pure (Left err)
    Right sigma -> do
      let rule' = substitute sigma $ normInstRuleConclusion (NormInstRule rule)
      rule'_norm <- normalize rule'
      pure $ rule'_norm <#> case _ of
        Left rule'' -> ApplyPatch rule''
        Right prop' -> ConclusionPatch prop'

-- | Check if a rule can be applied to a proposition.
canApplyNormInstRuleToProposition :: forall m. MonadEffect m =>
  NormInstRule ->
  G.ConcreteProposition ->
  GenerateT m Boolean
canApplyNormInstRuleToProposition rule prop =
  applyNormInstRuleToProposition rule prop <#>
    isRight


class Subsumable a where
  subsumes :: forall m. MonadEffect m => a -> a -> GenerateT m Boolean

instance Subsumable Patch where
  subsumes (ApplyPatch rule1) (ApplyPatch rule2) = rule1 `subsumes` rule2
  subsumes (ConclusionPatch prop1) (ConclusionPatch prop2) = prop1 `subsumes` prop2
  subsumes _ _ = pure false

isSubsumedPatch :: forall m. MonadEffect m => Patch -> GenerateT m Boolean  
isSubsumedPatch (ApplyPatch _) = pure false
isSubsumedPatch (ConclusionPatch prop@(G.Proposition rel _)) = do
  prop' <- getHighest rel
  pure $ (prop <=? prop') == Just true

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
  -- debugA $ "subsumption check: " <> pretty xs1 <> " ⊑ " <> pretty xs2
  xs1' <- evaluate `traverse` xs1
  xs2' <- evaluate `traverse` xs2
  result <- pure $ Array.all (\x2 -> Array.any (\x1 -> x1 == x2) xs1') xs2'
  -- debugA $ "===> " <> show result
  pure result

subsumesConcreteTerm' (G.BoundTerm name _) term2 = do
  term1 <- (List.findMap (\(name' /\ term) -> if name == name' then Just term else Nothing) <$> asks _.sigma) >>= case _ of
    Nothing -> bug $ "[subsumes] Expected bound variable to be in local substitution: " <> ticks (pretty name)
    Just term1 -> pure term1
  subsumes term1 term2

subsumesConcreteTerm' (G.QuantTerm name _) _ = absurd name
subsumesConcreteTerm' _ (G.QuantTerm name _) = absurd name

subsumesConcreteTerm' term1 term2 = bug $ "[subsumes] Unhandled subsumption checking pair: " <> ticks (pretty term1) <> ", " <> ticks (pretty term2)

instance Subsumable G.ConcreteTerm where

  -- TODO: first shallow evaluate, then use subsumesConcreteTerm'
  subsumes term1 term2 = do
    debugA $ "subsumes: " <> pretty term1 <> " <= " <> pretty term2
    term1' <- shallowEvaluate term1
    term2' <- shallowEvaluate term2
    result <- subsumesConcreteTerm' term1' term2'
    debugA $ "===> " <> show result
    pure result
