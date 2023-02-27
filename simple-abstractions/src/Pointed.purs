module Pointed where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)

data Pointed a
  = Finite a
  | Infinity

derive instance genericPointed :: Generic (Pointed a) _

instance eqPointed :: Eq a => Eq (Pointed a) where
  eq x y = genericEq x y

instance ordPointed :: Ord a => Ord (Pointed a) where
  compare x y
    | x == y = EQ
  compare (Finite x) (Finite y) = compare x y
  compare Infinity _ = GT
  compare _ Infinity = LT

instance semigroupPointed :: Semigroup a => Semigroup (Pointed a) where
  append (Finite x) (Finite y) = Finite (x <> y)
  append Infinity _ = Infinity
  append _ Infinity = Infinity

instance monoidPointed :: Monoid a => Monoid (Pointed a) where
  mempty = Finite mempty
