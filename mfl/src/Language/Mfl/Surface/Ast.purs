module Language.Mfl.Surface.Ast where

import Prelude
import Prim hiding (Type)

import Data.Bot (Bot)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Top (Top)
import Data.Tuple.Nested

--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------

data Module = Module (Array Declaration)

derive instance Generic Module _
instance Show Module where show x = genericShow x

data Declaration
  = DataTypeDeclaration Name DataType
  | LatTypeDeclaration Name LatType
  | RelationDeclaration Name LatType
  | RuleDeclaration Name Rule
  | AxiomDeclaration Name Prop
  | FunctionDeclaration 
      { name :: Name
      , params :: Array (Name /\ DataType) }
  | FixpointDeclaration 
      { name :: Name
      , ruleOrAxiomNames :: Array Name }
  | QueryDeclaration
      { name :: Name
      , params :: Array (Name /\ LatType)
      , queryProp :: Prop }
  | InsertionDeclaration 
      { name :: Name
      , params :: Array (Name /\ LatType)
      , insertionProp :: Prop }

derive instance Generic Declaration _
instance Show Declaration where show x = genericShow x

--------------------------------------------------------------------------------
-- Type, DataType, LatType
--------------------------------------------------------------------------------

type LatType = Type Top
type DataType = Type Bot

data Type isLat
  = BoolType
  | NatType
  | StringType
  | SetType (Type isLat)
  | -- | Always use lexicographical ordering.
    TupleType (Array (Type isLat))
  | PowerSetType isLat (Type Bot)
  | OpType isLat (Type isLat)

derive instance Generic (Type isLat) _
instance Show isLat => Show (Type isLat) where show x = genericShow x

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

data Term
  = NeuTerm Name (Array Term)
  | ConstrTerm Constr
  | VarTerm Name

derive instance Generic Term _
instance Show Term where show x = genericShow x

data Constr
  = NatConstr NatConstr
  | BoolConstr BoolConstr
  | StringConstr StringConstr
  | SetConstr SetConstr
  | TupleConstr TupleConstr

derive instance Generic Constr _
instance Show Constr where show x = genericShow x

data NatConstr = ZeroConstr | SucConstr Term | InfinityConstr

derive instance Generic NatConstr _
instance Show NatConstr where show x = genericShow x

type BoolConstr = Boolean

data StringConstr = LiteralStringConstr String | ZetaConstr

derive instance Generic StringConstr _
instance Show StringConstr where show x = genericShow x

data SetConstr = LiteralSetConstr (Array Term) | SigmaConstr

derive instance Generic SetConstr _
instance Show SetConstr where show x = genericShow x

type TupleConstr = Array Term

--------------------------------------------------------------------------------
-- Prop
--------------------------------------------------------------------------------

data Prop = Prop Name Term

derive instance Generic Prop _
instance Show Prop where show x = genericShow x

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

type Rule = Array RuleItem

data RuleItem
  = FilterRuleItem Term 
  | QuantRuleItem Name LatType
  | LetRuleItem Name Term 
  | -- | If this appears before the end of a rule, then is a premise. If this 
    -- appears at the end of a rule, then is a conclusion.
    PropRuleItem Prop

derive instance Generic RuleItem _
instance Show RuleItem where show x = genericShow x

--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

data Name = Name String

derive instance Generic Name _
instance Show Name where show x = genericShow x
