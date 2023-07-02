module Language.Fixlat.Core.Internal.Database where

import Data.Either.Nested
import Data.Tuple.Nested
import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Debug as Debug
import Control.Monad.Reader (ask, asks, lift, runReaderT)
import Control.Monad.State (gets, modify, modify_)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Unfoldable as Unfoldable
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Subsumption (subsumes)
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
        for rules \rule -> canApplyNormRuleToProposition rule prop >>= if _
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
      Right <$> applyNormRule rule

-- | Apply a rule to the current database, yielding the resulting patches.
applyNormRule :: forall m. MonadEffect m =>
  NormRule ->
  GenerateT m (List Patch)
applyNormRule rule = do
  Database props <- gets _.database
  List.foldMap Unfoldable.fromMaybe <$> 
    for props \prop -> hole "TODO"

canApplyNormRuleToProposition :: forall m. MonadEffect m =>
  NormRule ->
  G.ConcreteProposition ->
  GenerateT m Boolean
canApplyNormRuleToProposition = hole "canApplyNormRuleToProposition"