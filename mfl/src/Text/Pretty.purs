module Text.Pretty where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))

class Pretty a where pretty :: a -> String

instance Pretty String where pretty str = str
instance Pretty Int where pretty = show
instance Pretty Char where pretty = CodeUnits.singleton
instance Pretty Boolean where pretty = show
instance Pretty Number where pretty = show

instance Pretty a => Pretty (Maybe a) where pretty = maybe "Nothing" pretty
instance Pretty a => Pretty (Array a) where pretty xs = String.joinWith ", " (pretty <$> xs)
instance Pretty a => Pretty (List a) where pretty x = pretty (Array.fromFoldable x)
instance Pretty a => Pretty (NonEmptyList a) where pretty x = pretty (Array.fromFoldable x)
instance (Pretty a, Pretty b) => Pretty (Tuple a b) where pretty (Tuple a b) = pretty a <> "," <+> pretty b
instance (Pretty a, Pretty b) => Pretty (Either a b) where 
  pretty (Left a) = pretty a
  pretty (Right b) = pretty b
instance (Pretty k, Pretty v) => Pretty (Map k v) where pretty m = bullets ((Map.toUnfoldable m :: Array _) <#> \(Tuple k v) -> pretty k <> " := " <> pretty v)
instance Pretty a => Pretty (Set a) where pretty s = pretty (Set.toUnfoldable s :: Array _)

surround pre post str = pre <> str <> post

surround' prepost = surround prepost prepost

ticks = surround' "`"
brackets = surround "[" "]"
braces = surround "{" "}"
parens = surround "(" ")"

lines = String.joinWith "\n"

indent = indentN 1
indentN i =
  let ind = String.joinWith "" $ Array.replicate i "  " in
  \str -> String.joinWith "\n" $ (ind <> _) <$> String.split (String.Pattern "\n") str

bullets strs = "\n" <> (String.joinWith "\n" $ ("• " <> _) <$> strs)

appendPlus str1 str2 = str1 <> " " <> str2
infixr 4 appendPlus as <+>

appendNewline str1 str2 = str1 <> "\n" <> str2
infixr 3 appendNewline as <\>
