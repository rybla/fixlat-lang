module Language.Fixlat.Core.Internal.Database where

import Data.List (List)
import Prelude
import Data.Tuple.Nested
import Data.Either.Nested
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Base (GenerateT, NormRule(..), Patch)
import Type.Proxy (Proxy(..))

-- | Insert a proposition into the current database.
insertProposition :: forall m. MonadEffect m => 
  G.ConcreteProposition ->
  GenerateT m (Proxy "inserted proposition was subsumed" \/ Unit)
insertProposition = hole "insertProposition"

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

