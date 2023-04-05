module Utility where

import Prelude
import Control.Apply (lift2)
import Data.Array as Array
import Data.Foldable (foldr)
import Data.List (List(..), (:))
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

-- > fan ((1 : 2 : Nil) : (3 : 4 : Nil) : (5 : Nil) : Nil)
-- ((1 : 3 : 5 : Nil) : (1 : 4 : 5 : Nil) : (2 : 3 : 5 : Nil) : (2 : 4 : 5 : Nil) : Nil)
fan :: forall a. List (List a) -> List (List a)
fan Nil = Nil : Nil

fan (xs : xss) = do
  lift2 Cons xs (fan xss)

-- prefers first if compare EQ
maxBy :: forall a b. Ord b => (a -> b) -> a -> a -> a
maxBy f a1 a2 = if f a1 >= f a2 then a1 else a2
