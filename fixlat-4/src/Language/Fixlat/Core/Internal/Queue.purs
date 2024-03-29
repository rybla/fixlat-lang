module Language.Fixlat.Core.Internal.Queue where

import Data.Either.Nested
import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Debug as Debug
import Control.Monad.State (gets, modify_)
import Data.Either (Either(..))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.NonEmpty as NonemptyList
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, under)
import Data.Traversable (for, for_)
import Effect.Class (class MonadEffect)
import Hole (hole)
import Language.Fixlat.Core.Internal.Database (isSubsumedPatch)
import Record as R
import Text.Pretty (indent, indentN, pretty)
import Type.Proxy (Proxy(..))

pop :: forall m. MonadEffect m => 
  GenerateT m (Proxy "no more patches" \/ NonEmptyList Patch)
pop = do
  Queue queue <- gets _.queue
  case List.uncons queue of
    Nothing -> do
      Debug.debugA "[pop] no more patches" 
      pure $ Left Proxy
    Just {head: patches, tail: queue'} -> do
      modify_ $ R.set _queue (Queue queue')
      Debug.debugA $ "[pop] patches:"
      for_ patches \patch -> Debug.debugA (indentN 4 (pretty patch))
      patches # 
        ( -- ignore subsumed patches
          NonemptyList.filterM ((pure <<< not) <=< isSubsumedPatch) >=> 
          NonEmptyList.fromList >>> case _ of
            Nothing -> do
              Debug.debugA "[pop] all new patches are subsumed"
              pop
            Just patches' -> pure (Right patches')
        )

-- TODO: actually use `comparePatch`
insert :: forall m. MonadEffect m => 
  Patch -> 
  GenerateT m Unit
insert patch = do
  modify_ $ R.modify _queue 
    (over Queue (_ `List.snoc` NonEmptyList.singleton patch))

