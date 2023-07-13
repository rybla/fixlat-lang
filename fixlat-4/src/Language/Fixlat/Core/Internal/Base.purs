module Language.Fixlat.Core.Internal.Base where

import Data.Tuple.Nested
import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, modify, runStateT)
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

type GenerateT m = ReaderT GenerateCtx (StateT GenerateEnv (ModuleT m))

liftGenerateT :: forall m a. MonadEffect m => ModuleT m a -> GenerateT m a
liftGenerateT = lift <<< lift

runGenerateT :: forall m a. MonadEffect m => GenerateCtx -> GenerateEnv -> GenerateT m a -> ModuleT m (a /\ GenerateEnv)
runGenerateT ctx env = flip runReaderT ctx >>> flip runStateT env

type GenerateEnv =
  { gas :: Int
  , database :: Database
  , rules :: List NormInstRule
  , queue :: Queue
  , comparePatch :: Patch -> Patch -> Ordering
  }

makeGenerateEnv :: _ -> GenerateEnv
makeGenerateEnv {gas, rules, queue, comparePatch} =
  { gas
  , database: emptyDatabase
  , rules
  , queue
  , comparePatch
  }

type GenerateCtx =
  { sigma :: TermSub
  }

makeGenerateCtx :: _ -> GenerateCtx
makeGenerateCtx {} =
  { sigma: mempty }

_gas = Proxy :: Proxy "gas"
_database = Proxy :: Proxy "database"
_rules = Proxy :: Proxy "rules"
_queue = Proxy :: Proxy "queue"
_comparePatch = Proxy :: Proxy "comparePatch"
_sigma = Proxy :: Proxy "sigma"

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
  = ApplyPatch NormInstRule
  | ConclusionPatch G.ConcreteProposition

derive instance Generic Patch _
instance Show Patch where show x = genericShow x

instance Pretty Patch where
  pretty (ApplyPatch rule) = "apply:" <\> indent (pretty rule)
  pretty (ConclusionPatch prop) = "conclude: " <> pretty prop

--------------------------------------------------------------------------------
-- InstRule, NormInstRule
--------------------------------------------------------------------------------

-- | An instantiated rule is a rule that has been instantiated with a
-- | quantification context and a substitution. It only appears during
-- | generation.
-- |
-- | Note that `gamma` and `sigma` are stored in reverse order of introductions,
-- | since we want the most inner introductions to be found before outer ones.
newtype InstRule = InstRule
  { originalRule :: G.Rule
  , gamma :: QuantCtx
  , sigma :: TermSub
  , rule :: G.Rule }

instance Make InstRule G.Rule where
  make originalRule = InstRule
    { originalRule
    , gamma: mempty
    , sigma: mempty
    , rule: originalRule }

derive instance Newtype InstRule _
derive newtype instance Show InstRule
derive newtype instance Eq InstRule

-- | A normalized instantiated rule has a premise at its head.
-- |
-- | Note that `gamma` and `sigma` are stored in reverse order of introductions,
-- | since we want the most inner introductions to be found before outer ones.
newtype NormInstRule = NormInstRule
  { originalRule :: G.Rule
  , gamma :: QuantCtx
  , sigma :: TermSub
  , premise :: G.SymbolicProposition
  , rule :: G.Rule }

derive instance Newtype NormInstRule _
derive newtype instance Show NormInstRule
derive newtype instance Eq NormInstRule

instance Pretty NormInstRule where
  pretty (NormInstRule rule) = pretty rule.rule

normInstRuleConclusion :: NormInstRule -> InstRule
normInstRuleConclusion (NormInstRule rule) = InstRule
  { originalRule: rule.originalRule
  , gamma: rule.gamma
  , sigma: rule.sigma
  , rule: rule.rule }

type TermSub = List (G.TermName /\ G.ConcreteTerm)

emptyTermSub :: TermSub
emptyTermSub = mempty

type QuantCtx = List G.Quantification

emptyQuantCtx :: QuantCtx
emptyQuantCtx = mempty