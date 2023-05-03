module Text.Hyper where

import Prelude

import Data.Array (foldMap, intercalate)

data Html = Tag String (Array Prop) (Array Html) | Raw String
data Prop = Class String | Id String

type DocumentOptions =
  { title :: String
  , linkHrefs :: Array String
  }

defaultDocumentOptions :: String -> DocumentOptions
defaultDocumentOptions title = 
  { title 
  , linkHrefs: []
  }

renderDocument :: DocumentOptions -> Html -> String
renderDocument opts html =
  intercalate "\n"
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "  <meta charset=\"UTF-8\">"
    , "  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">"
    , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
    , "  <title>" <> opts.title <> "</title>"
    , intercalate "\n" $ opts.linkHrefs <#> \href -> 
        "  <link rel=\"stylesheet\" href=\"" <> href <> "\">"
    , "</head>"
    , "<body>"
    , render html
    , "</body>"
    , "</html>"
    ]

render :: Html -> String
render (Raw str) = str
render (Tag tag props kids) = 
  foldMap identity 
    [ "<" <> tag <> foldMap ((" " <> _) <<< goProp) props <> ">"
    , intercalate " " (render <$> kids)
    , "</" <> tag <> ">"
    ]
  where
  goProp (Class str) = "class=\"" <> str <> "\""
  goProp (Id str) = "id=\"" <> str <> "\""

class Hyper a where
  hyper :: a -> Array Html

raw = Raw
tag = Tag

span_ = tag "span" []
span = tag "span"

div_ = tag "div" []
div = tag "div"

