module Language.Mfl.Surface.Ast where

import Data.Tuple.Nested
import Prelude
import Prim hiding (Type)

import Data.Array as Array
import Data.Bot (Bot)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Top (Top)
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, pretty)

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
instance Eq Declaration where eq x y = genericEq x y

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
instance Eq isLat => Eq (Type isLat) where eq x y = genericEq x y

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

data Term = Term TermLabel (Array Term)

derive instance Generic Term _
instance Show Term where show x = genericShow x
instance Eq Term where eq x y = genericEq x y

data TermLabel
  -- Nat
  = Zero
  | Suc 
  | Infinity
  -- Bool
  | Boolean Boolean
  -- String
  | String String
  | Zeta
  -- Set
  | Set
  | Sigma
  -- Tuple
  | Tuple
  -- Neu
  | Neu Name
  -- Var
  | Var Name

derive instance Generic TermLabel _
instance Show TermLabel where show x = genericShow x
instance Eq TermLabel where eq x y = genericEq x y

instance Pretty Term where
  pretty (Term Zero []) = "Zero"
  pretty (Term Suc [t]) = "Suc(" <> pretty t <> ")"
  pretty (Term Infinity []) = "∞"
  pretty (Term (Boolean b) []) = if b then "True" else "False"
  pretty (Term (String s) []) = show s
  pretty (Term Zeta []) = "Zeta"
  pretty (Term Set ts) = "{" <> seps "," (pretty <$> ts) <> "}"
  pretty (Term Sigma []) = "Sigma"
  pretty (Term Tuple ts) = "[" <> seps "," (pretty <$> ts) <> "]"
  pretty (Term (Neu f) ts) = "$" <> pretty f <> "(" <> seps "," (pretty <$> ts) <> ")"
  pretty (Term (Var x) []) = "@" <> pretty x
  pretty t = unsafeCrashWith "Invalid term: " <> show t

seps :: String -> Array String -> String
seps sep xs = Array.intercalate " " $ (\str -> str <> sep) <$> xs

--------------------------------------------------------------------------------
-- Prop
--------------------------------------------------------------------------------

data Prop = Prop Name Term

derive instance Generic Prop _
instance Show Prop where show x = genericShow x
instance Eq Prop where eq x y = genericEq x y

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
instance Eq RuleItem where eq x y = genericEq x y

--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

data Name = Name String

derive instance Generic Name _
instance Show Name where show x = genericShow x
instance Eq Name where eq x y = genericEq x y

instance Pretty Name where
  pretty (Name s) = s
