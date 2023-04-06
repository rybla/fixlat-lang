module Data.Lattice where

import Prelude hiding (join)
import Data.Maybe

class
  Eq a <= PartialOrd a where
  comparePartial :: a -> a -> Maybe Ordering

-- | Test whether one value is _strictly less than_ another.
lessThan :: forall a. PartialOrd a => a -> a -> Maybe Boolean
lessThan a1 a2 =
  a1 `comparePartial` a2
    <#> case _ of
        LT -> true
        _ -> false

-- | Test whether one value is _strictly greater than_ another.
greaterThan :: forall a. PartialOrd a => a -> a -> Maybe Boolean
greaterThan a1 a2 =
  a1 `comparePartial` a2
    <#> case _ of
        GT -> true
        _ -> false

-- | Test whether one value is _non-strictly less than_ another.
lessThanOrEq :: forall a. PartialOrd a => a -> a -> Maybe Boolean
lessThanOrEq a1 a2 =
  a1 `comparePartial` a2
    <#> case _ of
        GT -> false
        _ -> true

-- | Test whether one value is _non-strictly greater than_ another.
greaterThanOrEq :: forall a. PartialOrd a => a -> a -> Maybe Boolean
greaterThanOrEq a1 a2 =
  a1 `comparePartial` a2
    <#> case _ of
        LT -> false
        _ -> true

infixl 4 lessThan as <?

infixl 4 lessThanOrEq as <=?

infixl 4 greaterThan as >?

infixl 4 greaterThanOrEq as >=?

-- | A  _lattice_ is a partially-ordered set that, for any finite subset, has a
-- | join (least upper bound) and a meet (greatest lower bound). A lattice has
-- | the following properties:
-- | 
-- | ```
-- | commutativity:
-- |   a ∨ b = b ∨ a
-- |   a ∧ b = b ∧ a
-- | 
-- | associativity:
-- |   a ∨ (b ∨ c) = (a ∨ b) ∨ c
-- |   a ∧ (b ∧ c) = (a ∧ b) ∧ c
-- | 
-- | absorbtion:
-- |   a ∨ (a ∧ b) = a
-- |   a ∧ (a ∨ b) = a
-- | 
-- | idempotentcy:
-- |   a ∨ a = a
-- |   a ∧ a = a
-- |
-- | a <= b  <==>  a = a ∧ b
-- | a <= b  <==>  b = a ∨ b
-- | a = a ∧ b  ==>  b ∧ (b ∨ a) = (a ∨ b) ∧ b = a ∧ b
-- | ```
class
  PartialOrd a <= Lattice a where
  join :: a -> a -> a
  meet :: a -> a -> a

infixr 3 join as ∧

infixr 3 meet as ∨
