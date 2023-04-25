module Language.Fixlat.MVar where

import Data.Tuple.Nested
import Language.Fixlat.Grammar
import Prelude

import Data.Maybe (Maybe)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Language.Fixlat.Deriv (Deriv, Derivs)

-- | MVar

newtype MVar = MVar (Maybe Var /\ UUID)
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

