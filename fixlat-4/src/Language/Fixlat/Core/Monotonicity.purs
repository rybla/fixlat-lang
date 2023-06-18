module Language.Fixlat.Core.Monotonicity where

import Language.Fixlat.Core.Grammar
import Prelude

import Data.List (List)
import Hole (hole)

data MonotonicityObligation

-- | Generates the MonotonicityObligations that are sufficient to show that,
-- | each Relation's derivations, using only the given Rules, are monotonic in
-- | the lattice of its argument.
obligateMonotonicity :: Array Relation -> Array Rule -> List MonotonicityObligation
obligateMonotonicity = hole "obligateMonotonicity"
