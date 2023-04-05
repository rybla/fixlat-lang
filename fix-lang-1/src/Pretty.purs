-- | Super simple pretty-printing.
module Pretty where

import Data.Array
import Data.Maybe
import Data.Tuple
import Partial.Unsafe
import Prelude
import Data.Newtype

-- | The class of pretty types.
class Pretty a where
  pretty :: a -> Grid

instance prettyString :: Pretty String where
  pretty str = Grid [ [ str ] ]

instance prettyInt :: Pretty Int where
  pretty int = pretty (show int)

instance prettyArray :: Pretty a => Pretty (Array a) where
  pretty = commas

instance prettyTuple :: (Pretty a, Pretty b) => Pretty (Tuple a b) where
  pretty (Tuple a b) = pretty a ~ ", " ~ pretty b

instance prettyGrid :: Pretty Grid where
  pretty = identity

-- | Grid of strings.
newtype Grid
  = Grid (Array (Array String))

instance showGrid :: Show Grid where
  show = render

derive instance newtypeGrid :: Newtype Grid _

instance semigroupGrid :: Semigroup Grid where
  append = happ

instance monoidGrid :: Monoid Grid where
  mempty = Grid mempty

-- class Pretty a where
--   pretty :: a -> Grid
-- instance isGridGrid :: Pretty Grid where
--   pretty = identity
-- instance isGridString :: Pretty String where
--   pretty str = [ [ str ] ]
render :: Grid -> String
render (Grid grid) = intercalate "\n" $ map (go "") grid
  where
  go :: String -> Array String -> String
  go str strs = case uncons strs of
    Nothing -> str
    -- !TODO check for spacing between str and head (relevant to punctuation
    -- like commas)
    Just { head, tail } -> go (str <> " " <> head) tail

indent :: forall a. Pretty a => a -> Grid
indent = over Grid indent' <<< pretty

indent' :: Array (Array String) -> Array (Array String)
indent' = (map ([ indentation ] <> _))
  where
  indentation = "  "

happ :: forall a b. Pretty a => Pretty b => a -> b -> Grid
happ a b = case unsnoc ls1 of
  Nothing -> g2
  Just { init: ls1', last: l1 } -> case uncons ls2 of
    Nothing -> g1
    Just { head: l2, tail: ls2' } -> wrap (ls1' <> [ l1 <> l2 ] <> ls2')
  where
  g1@(Grid ls1) = pretty a

  g2@(Grid ls2) = pretty b

infixr 5 happ as ~

hcat :: forall a. Pretty a => Array a -> Grid
hcat = foldr happ mempty

vapp :: forall a b. Pretty a => Pretty b => a -> b -> Grid
vapp g1 g2 = wrap (unwrap (pretty g1) <> unwrap (pretty g2))

infixr 6 vapp as \~

vcat :: forall a. Pretty a => Array a -> Grid
vcat = foldr vapp mempty <<< map pretty

parens :: forall a. Pretty a => a -> Grid
parens a = "(" ~ a ~ ")"

braces :: forall a. Pretty a => a -> Grid
braces a = "{" ~ a ~ "}"

brackets :: forall a. Pretty a => a -> Grid
brackets a = "[" ~ a ~ "]"

commas :: forall a. Pretty a => Array a -> Grid
commas = intercalate (pretty ", ") <<< map pretty
