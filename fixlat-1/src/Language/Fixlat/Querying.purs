-- | This module defines how to process a single `Query`.
module Language.Fixlat.Querying where

import Control.Monad.Reader
import Control.Monad.State
import Language.Fixlat.Grammar
import Prelude
import Data.Map as Map
import Language.Fixlat.M as M
import Partial.Unsafe (unsafeCrashWith)

-- | The querying monad.
type QueryingM
  = ReaderT QueryingCtx (StateT QueryingEnv M.M)

-- | The querying context.
-- | - `producers` maps a precicate name to an array of all of the rules that
-- |   can produce an instance of it.
type QueryingCtx
  = { predicates :: Map.Map Name Predicate -- pred name => pred
    , rules :: Map.Map Name (Rule Sort) -- rule name => rule
    , producers :: Map.Map Name (Array (Rule Sort)) -- pred name => rules that can produce that pred
    , inferences :: Map.Map Name Knowledge -- pred name => knowledge index of the rules that can produce that pred
    }

-- | Knowledge index over rules that can produce a particular predicate. The
-- | index is aware of the lattice structure over rules, which ensures that:
-- | - when a new rule is inserted, overwrites rule rules that are less general
-- | - when a predicate instance is queried, yields rules with matching
-- |   conclusions in order of decreasing [!TODO what ordering here?]
data Knowledge

-- | The querying environment. Keeps track of information learned so far.
type QueryingEnv
  = {}

initQueryingCtx :: Module Sort -> QueryingCtx
initQueryingCtx _mdl = unsafeCrashWith "initQueryingCtx"

-- !TODO come up with better word than "submit"
submitQuery :: Query Sort -> QueryingM Unit
submitQuery = unsafeCrashWith "submitQuery"

-- !TODO write version of this that produces a proof tree of rule applications
-- rather than just `Boolean`
{-

how to query a term:
- 

-}
queryTerm :: Term Sort -> QueryingM Boolean
queryTerm = unsafeCrashWith "queryTerm"
