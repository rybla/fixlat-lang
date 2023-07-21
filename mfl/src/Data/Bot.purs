module Data.Bot where

import Prelude

import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty)

data Bot

elimBot :: forall a. Bot -> a
elimBot _ = unsafeCrashWith "Should not have term of type `Bot`"

-- derive instance Generic Bot _
instance Show Bot where show = elimBot
instance Eq Bot where eq _ = elimBot
instance Ord Bot where compare _ = elimBot
instance Pretty Bot where pretty = elimBot
