module Data.Bug where

import Prelude
import Data.Array (intercalate)
import Partial.Unsafe (unsafeCrashWith)

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ intercalate "\n"
  [ "",
    "==[ BEGIN bug ]========================================================",
    msg,
    "==[ END bug ]========================================================",
    ""
  ]
