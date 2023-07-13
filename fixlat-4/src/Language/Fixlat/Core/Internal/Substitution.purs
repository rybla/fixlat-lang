module Language.Fixlat.Core.Internal.Substitution where

import Language.Fixlat.Core.Internal.Base
import Prelude

import Data.Map as Map
import Language.Fixlat.Core.Grammar (Term)

class Substitutable a where
  substitute :: TermSub -> a -> a

instance Substitutable InstRule where
  substitute sigma (InstRule rule) = InstRule rule
    { sigma = sigma <> rule.sigma }

instance Substitutable NormInstRule where
  substitute sigma (NormInstRule rule) = NormInstRule rule
    { sigma = sigma <> rule.sigma }
