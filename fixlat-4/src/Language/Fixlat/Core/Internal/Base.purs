module Language.Fixlat.Core.Internal.Base where

import Prelude
import Control.Monad.State (StateT, modify)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List.Types (NonEmptyList)
import Data.List.Types as NonEmptyList
import Data.Make (class Make)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import Language.Fixlat.Core.Grammar as G
import Language.Fixlat.Core.ModuleT (ModuleT)
import Record as R
import Text.Pretty (class Pretty, bullets, indent, pretty, (<\>))
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- GenerateT
--------------------------------------------------------------------------------

type GenerateT m = StateT FixpointEnv (ModuleT m)

liftGenerateT :: forall m a. MonadEffect m => ModuleT m a -> GenerateT m a
liftGenerateT = lift

type FixpointEnv =
  { gas :: Int
  , database :: Database
  , rules :: List NormRule
  , queue :: Queue
  , comparePatch :: Patch -> Patch -> Ordering
  }

_gas = Proxy :: Proxy "gas"
_database = Proxy :: Proxy "database"
_rules = Proxy :: Proxy "rules"
_queue = Proxy :: Proxy "queue"
_comparePatch = Proxy :: Proxy "comparePatch"

-- | Decrements gas, and returns true if gas is nonpositive.
decrementGasNonpositive :: forall m. MonadEffect m => GenerateT m Boolean
decrementGasNonpositive = do
  env <- modify (R.modify _gas (_ - 1))
  pure $ env.gas <= 0

--------------------------------------------------------------------------------
-- Database
--------------------------------------------------------------------------------

-- | A Database stores all current rules, which includes 
data Database = Database (List G.ConcreteProposition)

derive instance Generic Database _
instance Show Database where show x = genericShow x

instance Pretty Database where
  pretty (Database props) = bullets (Array.fromFoldable (pretty <$> props))

emptyDatabase :: Database
emptyDatabase = Database Nil

--------------------------------------------------------------------------------
-- Queue
--------------------------------------------------------------------------------

-- | The Queue is ordered by the contextual `comparePatch` function. Each item
-- | of the queue is a NonEmptyList of Patches that are considered equivalent by
-- | `comparePatch`.
newtype Queue = Queue (List (NonEmptyList Patch))

derive instance Newtype Queue _
derive newtype instance Show Queue

instance Pretty Queue where
  pretty (Queue queue) = bullets (Array.fromFoldable (pretty <$> NonEmptyList.toList <$> queue))

--------------------------------------------------------------------------------
-- Patch
--------------------------------------------------------------------------------

data Patch
  = ApplyPatch NormRule
  | ConclusionPatch G.ConcreteProposition

derive instance Generic Patch _
instance Show Patch where show x = genericShow x

instance Pretty Patch where
  pretty (ApplyPatch rule) = "apply:" <\> indent (pretty rule)
  pretty (ConclusionPatch prop) = "conclude: " <> pretty prop

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

newtype Rule = Rule
  { originalRule :: G.Rule
  , gamma :: G.QuantCtx
  , sigma :: G.TermSub
  , rule :: G.Rule }

instance Make Rule G.Rule where
  make originalRule = Rule
    { originalRule
    , gamma: []
    , sigma: Map.empty
    , rule: originalRule }

--------------------------------------------------------------------------------
-- NormRule
--------------------------------------------------------------------------------

newtype NormRule = NormRule
  { originalRule :: G.Rule
  , gamma :: Array G.Quantification
  , sigma :: G.TermSub
  , premise :: G.SymbolicProposition
  , rule :: G.Rule }

derive instance Newtype NormRule _
derive newtype instance Show NormRule
derive newtype instance Eq NormRule

instance Pretty NormRule where
  pretty (NormRule rule) = pretty rule.rule

