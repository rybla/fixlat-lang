module Utility where

import Prelude
import Data.Foldable (intercalate)

if_ :: forall a. a -> a -> Boolean -> a
if_ t _f true = t

if_ _t f false = f
