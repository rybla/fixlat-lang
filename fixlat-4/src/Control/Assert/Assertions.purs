module Control.Assert.Assertions where

import Prelude

import Control.Assert (Assertion)
import Control.Monad.Error.Class (throwError)
import Data.Array (all)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

just :: forall a. Assertion (Maybe a) a
just = 
  { label: "just"
  , check: case _ of
      Nothing -> throwError "Expected `Just _`, got `Nothing`"
      Just a -> pure a
  }

equal :: forall a. Show a => Eq a => Assertion (a /\ a) a
equal =
  { label: "equal"
  , check: \(a /\ a') -> if a == a' then pure a else 
      throwError $ 
        "Given values are not equal:\n" <>
        "- " <> show a <> "\n" <>
        "- " <> show a' <> "\n"
  }

permutedArrays :: forall a. Show a => Eq a => Assertion (Array a /\ Array a) Unit
permutedArrays =
  { label: "permutedArrays"
  , check: \(as /\ as') -> 
      if (_ `Array.elem` as') `all` as && 
         (_ `Array.elem` as) `all` as' 
        then pure unit else
        throwError $ 
          "Given arrays are not permutations of each other:\n" <>
          "  - " <> show as <> "\n" <>
          "  - " <> show as' <> "\n"
  }

anyInArray :: forall a. Show a => String -> (a -> Boolean) -> Assertion (Array a) a
anyInArray condLabel cond =
  { label: "anyInArray"
  , check: \as -> case cond `Array.find` as of
      Nothing -> throwError $ 
        "Could not find element of array that satisfies condition '" <> condLabel <> "':\n" <>
        "  - " <> show as
      Just a -> pure a
  }

memberOfArray :: forall a. Show a => Eq a => Assertion (a /\ Array a) Int
memberOfArray =
  { label: "memberOfArray"
  , check: \(a /\ as) -> case a `Array.elemIndex` as of
      Nothing -> throwError $
        "Could not find the term '" <> show a <> "' in array:\n" <>
        "  - " <> show as
      Just i -> pure i

  }

exactLength :: forall a f. Foldable f => Show (f a) => Int -> Assertion (f a) Unit
exactLength l = 
  { label: "exactLength"
  , check: \as -> if Foldable.length as == l then pure unit else
      throwError $ 
        "Expected a foldable of length " <> show l <> ", but got a foldable of length " <> show (Foldable.length as :: Int) <> ":\n" <>
        "  - " <> show as
  }

keyOfMap :: forall k v. Show k => Ord k => Show v => Assertion (k /\ Map.Map k v) v
keyOfMap =
  { label: "keyOfMap"
  , check: \(k /\ m) -> case Map.lookup k m of
      Nothing -> throwError $
        "Could not find the key '" <> show k <> "' in map:\n" <>
        "  - " <> show m
      Just v -> pure v
  }
