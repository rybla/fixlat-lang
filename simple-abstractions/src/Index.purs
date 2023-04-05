module Index where

import Data.Tuple.Nested
import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.List (List)

newtype IndexF query value info carry
  = Index
  { carry :: carry
  , insert :: value -> carry -> carry
  , lookup :: query -> carry -> List (info /\ value)
  }

type Index query value info
  = Exists (IndexF query value info)

insert :: forall query value info. value -> Index query value info -> Index query value info
insert r = runExists \(Index ix) -> mkExists (Index ix { carry = ix.insert r ix.carry })

lookup :: forall query value info. query -> Index query value info -> List (info /\ value)
lookup re = runExists \(Index ix) -> ix.lookup re ix.carry
