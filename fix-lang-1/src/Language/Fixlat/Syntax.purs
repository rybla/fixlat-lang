-- | Syntax types for language.
module Language.Fixlat.Syntax where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | A top-level __statement__.
data Statement
  = StatementSyntax Syntax
  | StatementRule Rule

-- | A __syntax__ declaration.
type Syntax
  = { name :: Name
    , argumentsCount :: Int
    }

-- | An inference __rule__ declaration.
type Rule
  = { parameters :: Array Name
    , hypotheses :: Array Expr
    , conclusions :: Array Expr
    }

-- | A __proposition__.
data Prop
  = PropExpr Expr
  | PropForall Name Prop
  | PropExists Name Prop

derive instance genericProp :: Generic Prop _

instance showProp :: Show Prop where
  show x = genericShow x

-- | An __expression__.
data Expr
  = ExprNeu { name :: Name, arguments :: Array Expr }

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

-- | A __name__.
newtype Name
  = Name String

derive instance genericName :: Generic Name _

derive instance newtypeName :: Newtype Name _

instance showName :: Show Name where
  show x = genericShow x
