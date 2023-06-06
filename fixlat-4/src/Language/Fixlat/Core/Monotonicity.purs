module Language.Fixlat.Core.Monotonicity where

import Language.Fixlat.Core.Grammar
import Prelude

import Data.List (List)
import Hole (hole)

type MonotonicityObligation =
  { scope :: List Quantifier
  , inputs :: List (Term LatticeType)
  , conclusionTerm :: Term LatticeType  
  , conclusionLattice :: LatticeType
  }

checkMonotonicity :: Rule -> List MonotonicityObligation
checkMonotonicity = hole "checkMonotonicity"
