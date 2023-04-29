module Text.Pretty where

import Prelude

import Data.Array as Array
import Data.String (fromCodePointArray, toCodePointArray)

class Pretty a where pretty :: a -> String

appendSpaced :: String -> String -> String
appendSpaced s1 s2 = 
  fromCodePointArray $ Array.concat
    [ Array.reverse $ Array.dropWhile (\cp -> " " == fromCodePointArray [cp]) $ Array.reverse $ toCodePointArray s1
    , toCodePointArray " "
    , Array.dropWhile (\cp -> " " == fromCodePointArray [cp]) $ toCodePointArray s2
    ]

infixr 6 appendSpaced as <+>
