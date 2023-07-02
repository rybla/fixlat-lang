module Language.Fixlat.Core.Internal.Database where

import Data.Either.Nested
import Data.Tuple.Nested
import Prelude

import Control.Debug as Debug
import Control.Monad.Reader (ask, asks, lift, runReaderT)
import Control.Monad.State (gets, modify_)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Base (Database(..), GenerateT, NormRule(..), Patch, _database)
import Language.Fixlat.Core.Internal.Subsumption (subsumes)
import Record as R
import Type.Proxy (Proxy(..))

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
learnPatch = hole "learnPatch"

-- | Enqueue a patch.
enqueuePatch :: forall m. MonadEffect m =>
  Patch ->
  GenerateT m Unit
enqueuePatch = hole "queuePatch"

-- | Apply a rule to the current database.
applyNormRule :: forall m. MonadEffect m =>
  NormRule ->
  GenerateT m (Maybe NormRule)
applyNormRule = hole "applyNormRule"

