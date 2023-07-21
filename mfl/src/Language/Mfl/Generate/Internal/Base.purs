module Language.Mfl.Generate.Internal.Base where

import Language.Mfl.Core.Ast
import Prelude
import Data.Tuple.Nested
import Control.Monad.Computation (ComputationT)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

--------------------------------------------------------------------------------
-- GenerateT
--------------------------------------------------------------------------------

type GenerateT = ComputationT "Generate" GenerateCtx GenerateEnv GenerateErr

type GenerateCtx = 
  { sigma :: TermSub }

type GenerateEnv = 
  { gas :: Int
  , rules :: List NormRule
  , queue :: Queue
  , database :: Database }

type GenerateErr = String

type TermSub = List (TermName /\ EvaluatedTerm)

--------------------------------------------------------------------------------
-- NormRule
--------------------------------------------------------------------------------

newtype NormRule = NormRule
  { ruleName :: RuleName
  , gamma :: QuantCtx
  , sigma :: TermSub
  , premise :: EvaluatedProp
  , rule :: Rule }

derive instance Newtype NormRule _
derive newtype instance Show NormRule

type QuantCtx = List Quant

--------------------------------------------------------------------------------
-- Queue
--------------------------------------------------------------------------------

newtype Queue = Queue (List NormRule)

derive instance Newtype Queue _
derive newtype instance Show Queue

data Patch
  = ApplyPatch NormRule
  | ConclusionPath ConcreteProp

derive instance Generic Patch _
instance Show Patch where show x = genericShow x

--------------------------------------------------------------------------------
-- Database
--------------------------------------------------------------------------------

newtype Database = Database (Map.Map RelationName EvaluatedProp)

derive instance Newtype Database _
derive newtype instance Show Database

emptyDatabase :: Database
emptyDatabase = Database Map.empty




