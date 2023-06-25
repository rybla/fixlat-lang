module Data.Make where

import Prelude

-- | An instance of `Make` has a canonical constructor that differs from its
-- | usual datatype constructor.
class Make a arg | a -> arg where 
  make :: arg -> a