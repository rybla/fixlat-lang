-- | Super simple pretty-printing.
module Pretty where

import Data.Array
import Data.Maybe
import Partial.Unsafe
import Prelude
import Data.Tuple

-- | The class of pretty types.
class Pretty a where
  pretty :: a -> Grid

instance prettyString :: Pretty String where
  pretty str = toGrid str

instance prettyInt :: Pretty Int where
  pretty int = pretty (show int)

instance prettyArray :: Pretty a => Pretty (Array a) where
  pretty = commas <<< map pretty

instance prettyTuple :: (Pretty a, Pretty b) => Pretty (Tuple a b) where
  pretty (Tuple a b) = pretty a ~ ", " ~ pretty b

-- | Grid of strings.
type Grid
  = Array (Array String)

class IsGrid a where
  toGrid :: a -> Grid

instance isGridGrid :: IsGrid Grid where
  toGrid = identity

instance isGridString :: IsGrid String where
  toGrid str = [ [ str ] ]

render :: Grid -> String
render grid = intercalate "\n" $ map (go "") grid
  where
  go :: String -> Array String -> String
  go str strs = case uncons strs of
    Nothing -> str
    -- !TODO check for spacing between str and head (relevant to punctuation
    -- like commas)
    Just { head, tail } -> go (str <> head) tail

indent :: forall a. IsGrid a => a -> Grid
indent = map ([ "  " ] <> _) <<< toGrid

happ :: forall a b. IsGrid a => IsGrid b => a -> b -> Grid
happ _g1 _g2 = case uncons g1 of
  Nothing -> g2
  Just { head: head1, tail: tail1 } -> case uncons g2 of
    Nothing -> g1
    Just { head: head2, tail: tail2 } ->
      (head1 <> head2)
        : (indent tail1 <> tail2)
  where
  g1 = toGrid _g1

  g2 = toGrid _g2

infixr 5 happ as ~

hcat :: forall a. IsGrid a => Array a -> Grid
hcat = foldr happ mempty

vapp :: forall a b. IsGrid a => IsGrid b => a -> b -> Grid
vapp g1 g2 = toGrid g1 <> toGrid g2

infixr 6 vapp as \~

vcat :: forall a. IsGrid a => Array a -> Grid
vcat = foldr vapp mempty

vsep :: forall a. IsGrid a => Array a -> Grid
vsep = intercalate [ [] ] <<< map toGrid

parens :: forall a. IsGrid a => a -> Grid
parens a = "(" ~ a ~ ")"

braces :: forall a. IsGrid a => a -> Grid
braces a = "{" ~ a ~ "}"

brackets :: forall a. IsGrid a => a -> Grid
brackets a = "[" ~ a ~ "]"

commas :: forall a. IsGrid a => Array a -> Grid
commas = intercalate (toGrid ", ") <<< map toGrid
