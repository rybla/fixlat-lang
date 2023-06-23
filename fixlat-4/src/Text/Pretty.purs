module Text.Pretty where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))

class Pretty a where pretty :: a -> String

instance Pretty String where pretty str = str
instance Pretty Int where pretty = show
instance Pretty Char where pretty = show
instance Pretty Boolean where pretty = show
instance Pretty Number where pretty = show

instance Pretty a => Pretty (Maybe a) where pretty = maybe "Nothing" pretty
instance Pretty a => Pretty (Array a) where pretty xs = String.joinWith ", " (pretty <$> xs)
instance Pretty a => Pretty (List a) where pretty x = pretty (Array.fromFoldable x)
instance (Pretty a, Pretty b) => Pretty (Tuple a b) where pretty (Tuple a b) = pretty a <> "," <+> pretty b
instance (Pretty a, Pretty b) => Pretty (Either a b) where 
  pretty (Left a) = pretty a
  pretty (Right b) = pretty b
instance (Pretty k, Pretty v) => Pretty (Map k v) where pretty m = pretty ((Map.toUnfoldable m :: Array _) <#> \(Tuple k v) -> pretty k <> " := " <> pretty v)
instance Pretty a => Pretty (Set a) where pretty s = pretty (Set.toUnfoldable s :: Array _)

surround pre post str = pre <> str <> post

surround' prepost = surround prepost prepost

ticks = surround' "`"
brackets = surround "[" "]"
braces = surround "{" "}"
parens = surround "(" ")"

lines = String.joinWith "\n"

indent str = String.joinWith "\n" $ ("  " <> _) <$> String.split (String.Pattern "\n") str

bullets strs = String.joinWith "\n" $ ("  • " <> _) <$> strs

appendPlus str1 str2 = str1 <> " " <> str2

infixr 6 appendPlus as <+>
