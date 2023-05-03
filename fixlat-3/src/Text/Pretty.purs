module Text.Pretty where

import Data.Either.Nested
import Data.Tuple.Nested
import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.String (fromCodePointArray, toCodePointArray)

class Pretty a where pretty :: a -> String

-- show instance for String that doesn't have quotes around it
newtype ShowString = ShowString String
instance Show ShowString where show (ShowString str) = str
derive newtype instance Eq ShowString
derive newtype instance Ord ShowString

instance (Pretty a, Pretty b) => Pretty (a \/ b) where
  pretty (Left a) = pretty a
  pretty (Right b) = pretty b

instance (Pretty a, Pretty b) => Pretty (a /\ b) where
  pretty (a /\ b) = pretty a <> ", " <> pretty b

instance Pretty a => Pretty (List a) where
  pretty xs = show $ ShowString <<< pretty <$> xs

instance Pretty a => Pretty (Array a) where
  pretty xs = show $ ShowString <<< pretty <$> xs

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  -- pretty m = show <<< Map.fromFoldable <<< map (\(k /\ v) -> (ShowString (pretty k) /\ ShowString (pretty v))) $ (Map.toUnfoldable m :: Array _)
  pretty = (\str -> "{" <> str <> "}") <<< Array.intercalate ", " <<< map (\(k /\ v) -> pretty k <> ": " <> pretty v) <<< (Map.toUnfoldable :: _ -> Array _)

appendSpaced :: String -> String -> String
appendSpaced s1 s2 = 
  fromCodePointArray $ Array.concat
    [ Array.reverse $ Array.dropWhile (\cp -> " " == fromCodePointArray [cp]) $ Array.reverse $ toCodePointArray s1
    , toCodePointArray " "
    , Array.dropWhile (\cp -> " " == fromCodePointArray [cp]) $ toCodePointArray s2
    ]

infixr 6 appendSpaced as <+>
