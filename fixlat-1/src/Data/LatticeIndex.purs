module Data.LatticeIndex where

import Data.Lattice
import Prelude
import Data.List.Lazy
import Partial.Unsafe (unsafeCrashWith)

data LatticeIndex a

insert :: forall a. Lattice a => a -> LatticeIndex a -> LatticeIndex a
insert = unsafeCrashWith "TODO"

-- | Looks up a (lazy) list of elements that ... TODO
lookup :: forall a. Lattice a => a -> LatticeIndex a -> List a
lookup = unsafeCrashWith "TODO"
