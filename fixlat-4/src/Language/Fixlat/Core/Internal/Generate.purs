module Language.Fixlat.Core.Internal.Generate (generate) where

import Language.Fixlat.Core.Internal.Base
import Prelude

import Control.Assert (assertI)
import Control.Assert.Assertions (keyOfMap)
import Control.Bug (bug)
import Control.Debug as Debug
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.NonEmpty as NonemptyList
import Data.Make (make)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.Internal.Database as Database
import Language.Fixlat.Core.Internal.Normalization (normalize)
import Language.Fixlat.Core.Internal.Queue as Queue
import Language.Fixlat.Core.ModuleT (ModuleT, getModuleCtx)
import Text.Pretty (pretty, ticks)

--------------------------------------------------------------------------------
-- Generate
--------------------------------------------------------------------------------

generate :: forall m. MonadEffect m => 
  Database -> 
  G.FixpointSpecName -> 
  ModuleT m Database
generate (Database initialProps) fixpointSpecName = do
  Debug.debugA $ "[generate] BEGIN"
  Debug.debugA $ "[generate] fixpoint: " <> show fixpointSpecName

  moduleCtx <- getModuleCtx
  let fixpointSpec = assertI keyOfMap $ fixpointSpecName /\ (unwrap moduleCtx.module_).fixpoints

  let comparePatch = const <<< const $ GT

  let queue = do
        let axioms = (unwrap moduleCtx.module_).axioms #
              Map.filterWithKey (\axiomName _ -> axiomName `Array.elem` ((unwrap fixpointSpec).axiomNames)) >>>
              Map.values >>>
              Array.fromFoldable
        let props = (snd <$> Map.toUnfoldable initialProps) <> List.fromFoldable (axioms <#> \(G.Axiom prop) -> prop)
        Queue (NonEmptyList.singleton <$> (ConclusionPatch <$> props))

  relations <- getModuleCtx <#> _.module_ >>> unwrap >>> _.relations

  rules <- (unwrap moduleCtx.module_).rules #
    Map.filterWithKey (\ruleName _ -> ruleName `Array.elem` ((unwrap fixpointSpec).ruleNames)) >>>
    map (make :: _ -> InstRule) >>>
    let
      ctx :: GenerateCtx
      ctx = makeGenerateCtx {}

      env :: GenerateEnv
      env = makeGenerateEnv
          { gas: moduleCtx.initialGas
          , relations
          , rules: mempty
          , queue
          , comparePatch }
    in
    traverseWithIndex (\ruleName rule -> runGenerateT ctx env (normalize rule) >>= case _ of
      Left err /\ _ -> bug $ "Failed to normalize module rule " <> ticks (pretty ruleName) <> ": " <> err
      Right (Left rule') /\ _ -> pure rule'
      Right (Right prop') /\ _ -> bug $ "A user-defined rule should not normalize to a conclusion: " <> pretty prop') >>>
    map Map.values

  _ /\ env' <- do
    let
      ctx :: GenerateCtx
      ctx = makeGenerateCtx {}

      env :: GenerateEnv
      env = makeGenerateEnv
          { gas: moduleCtx.initialGas 
          , relations
          , rules
          , queue
          , comparePatch }

    runGenerateT ctx env loop

  Debug.debugA $ "[generate] END"
  pure env'.database

--------------------------------------------------------------------------------
-- Loop
--------------------------------------------------------------------------------

loop :: forall m. MonadEffect m => GenerateT m Unit
loop = do
  Debug.debugA $ "[loop] BEGIN"
  decrementGasNonpositive >>= if _
    then do
      Debug.debugA "[loop] no more gas"
      end
    else do
      Queue.pop >>= case _ of
        Left _ -> do
          Debug.debugA "[loop] no more patches"
          end
        Right patches -> do
          Debug.debugA $ "[loop] new patches count: " <> show (NonemptyList.length patches)
          let merge = map (either (const Nil) identity) >>> NonemptyList.toList >>> List.concat
          patches #
            ( traverse Database.learnPatch >=>
              merge >>> traverse_ Queue.insert ) 
          loop
  where
  end = do
    Debug.debugA $ "[loop] END"
    pure unit