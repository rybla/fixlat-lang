module Language.Fixlat.Core.Internal.Database where

import Data.Either.Nested (type (\/))
import Control.Monad.Trans.Class (lift)
import Language.Fixlat.Core.Internal.Base
import Prelude
import Utility (anyListM, (<$$>))
import Control.Debug as Debug
import Control.Monad.State (gets, modify_)
import Data.Either (Either(..), either, isRight)
import Data.List (List(..), (:))
import Data.List as List
import Data.Make (make)
import Data.Traversable (for)
import Effect.Class (class MonadEffect)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Normalization (normalize)
import Language.Fixlat.Core.Internal.Substitution (substitute)
import Language.Fixlat.Core.Internal.Subsumption (subsumes)
import Language.Fixlat.Core.Internal.Unification (unify)
import Record as R
import Type.Proxy (Proxy(..))
import Utility (anyListM)

-- | Insert a proposition into the current database.
insertProposition :: forall m. MonadEffect m => 
  G.ConcreteProposition ->
  GenerateT m (Proxy "inserted proposition was subsumed" \/ Unit)
insertProposition prop = do
  Database props <- gets _.database 
  go Nil props
  where
  go props' Nil = do
    modify_ $ R.set _database $ Database (prop : List.reverse props')
    pure (Right unit)
  go props' (Cons prop' props) =
    subsumes prop' prop >>= if _ 
      then do
        Debug.debugA "[insertProposition] inserted prop is subsumed"
        pure (Left Proxy)
      else subsumes prop prop' >>= if _
        then do
          Debug.debugA "[insertProposition] removing subsumed prop"
          go props' props
        else go (prop' : props') props

-- | Learn a patch to the current database.
learnPatch :: forall m. MonadEffect m =>
  Patch ->
  GenerateT m (Proxy "learned patch was subsumed" \/ List Patch)
learnPatch (ConclusionPatch prop) =
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
  rules <- gets _.rules
  isOld <- anyListM 
    (\rule' -> rule' `subsumes` rule)
    rules
  if isOld 
    then do
      Debug.debugA "[learnPatch] learned apply-patch is subsumed"
      pure (Left Proxy)
    else do
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
      let NormInstRule rule' = substitute sigma (NormInstRule rule)
      ApplyPatch <$$> lift (normalize (make rule'.rule :: InstRule))
      -- ?a $ normalize (make rule'.rule :: InstRule)

-- | Check if a rule can be applied to a proposition.
canApplyNormInstRuleToProposition :: forall m. MonadEffect m =>
  NormInstRule ->
  G.ConcreteProposition ->
  GenerateT m Boolean
canApplyNormInstRuleToProposition rule prop =
  applyNormInstRuleToProposition rule prop <#>
    isRight
