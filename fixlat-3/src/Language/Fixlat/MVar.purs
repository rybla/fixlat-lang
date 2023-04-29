module Language.Fixlat.MVar where

import Language.Fixlat.Grammar
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Language.Fixlat.Deriv (Deriv, Derivs)
import Text.Pretty (class Pretty, pretty)

-- | MVar

newtype MVar = MVar (Maybe Var /\ UUID)
derive instance Generic MVar _
derive instance Newtype MVar _
derive newtype instance Show MVar
instance Pretty MVar where 
  pretty (MVar (Nothing /\ uuid)) = "?" <> String.take 3 (UUID.toString uuid)
  -- !TODO it's so cluttering to have this, so, not sure if its necessary
  pretty (MVar (Just x /\ uuid)) = "?" <> pretty x -- <> "#" <> String.take 3 (UUID.toString uuid)
derive newtype instance Eq MVar
derive newtype instance Ord MVar

freshMVar :: Maybe Var -> MVar
freshMVar mb_var = MVar (mb_var /\ unsafePerformEffect UUID.genUUID)

type MX = MVar -- type of variables in a meta structure
type MRule = LRule MX -- meta
type MParam = Param MX
type MProp = LProp MX
type MTerm = LTerm MX
type MDeriv = Deriv MX
type MDerivs = Derivs MX

