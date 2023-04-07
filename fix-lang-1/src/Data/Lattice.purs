module Data.Lattice where

import Data.Maybe
import Prelude hiding (join)
import Utility
import Data.Newtype

-- | A partial ordering satisfies the following properties:
-- | ```
-- | reflexivity: 
-- |   a <= a
-- | 
-- | antisymmetry:
-- |   a <= b  and  b <= a  ==>  a = b
-- | 
-- | transitivity:
-- |   a <= b  and  b <= c  ==>  a <= c
-- | ```
class PartialOrd a where
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
-- | ```
class Lattice a where
  join :: a -> a -> Maybe a
  meet :: a -> a -> Maybe a

infixr 3 join as ∧

infixr 3 meet as ∨

-- | A partial ordering gives rise to a lattice if the following properties are
-- | satisfied:
-- | ```
-- | a <= b  <==>  a ∧ b = a
-- | a <= b  <==>  a ∨ b = b
-- |
-- | a = a ∧ b  ==>  b ∧ (b ∨ a) = (a ∨ b) ∧ b = a ∧ b
-- | ````
newtype PartialOrdLattice a
  = PartialOrdLattice a

instance partialOrdLattice :: PartialOrd a => Lattice (PartialOrdLattice a) where
  -- a <= b  ==>  a ∧ b = a
  -- b <= a  ==>  a ∧ b = b
  join (PartialOrdLattice a) (PartialOrdLattice b) = PartialOrdLattice <$> if_ a b <$> (a <=? b)
  -- a <= b  ==>  a ∨ b = b
  -- b <= a  ==>  a ∨ b = a
  meet (PartialOrdLattice a) (PartialOrdLattice b) = PartialOrdLattice <<< if_ b a <$> (a <=? b)
