module Utility where

import Prelude
import Data.Array as Array
import Data.Foldable (foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

lookup' k m msg = case Map.lookup k m of
  Nothing -> unsafeCrashWith $ "lookup': " <> msg
  Just v -> v

fromJust' mb msg = case mb of
  Nothing -> unsafeCrashWith $ "fromJust': " <> msg
  Just a -> a

showMap :: forall k v. Show k => Show v => Map.Map k v -> String
showMap m =
  foldr append ""
    [ "["
    , Array.intercalate ", "
        $ map (\(Tuple k v) -> show k <> ": " <> show v)
        $ (Map.toUnfoldable m :: Array (Tuple k v))
    , "]"
    ]

showMap' :: forall k v. (k -> String) -> (v -> String) -> Map.Map k v -> String
showMap' show_k show_v m =
  foldr append ""
    [ "["
    , Array.intercalate ", "
        $ map (\(Tuple k v) -> show_k k <> ": " <> show_v v)
        $ (Map.toUnfoldable m :: Array (Tuple k v))
    , "]"
    ]
