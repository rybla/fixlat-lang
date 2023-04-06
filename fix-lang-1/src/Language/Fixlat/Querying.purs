module Language.Fixlat.Querying where

import Language.Fixlat.Grammar
import Prelude
import Control.Monad.Reader
import Data.Map as Map
import Partial.Unsafe (unsafeCrashWith)
import Language.Fixlat.M as M

-- | The querying monad.
type QueryingM
  = ReaderT QueryingCtx M.M

-- | The querying context.
-- | - `inferences` maps a precicate name to all of the rules that can produce
-- |   an instance of that predicate.
type QueryingCtx
  = { predicates :: Map.Map Name Predicate -- pred name => pred
    , rules :: Map.Map Name (Rule Sort) -- rule name => rule
    , inferences :: Map.Map Name (Rule Sort) -- pred name => rule
    , queries :: Array (Query Sort)
    }

initQueryingCtx :: Module Sort -> QueryingCtx
initQueryingCtx _mdl = unsafeCrashWith "initQueryingCtx"

-- !TODO come up with better word than "submit"
submitQuery :: Query Sort -> QueryingM Unit
submitQuery = unsafeCrashWith "submitQuery"
