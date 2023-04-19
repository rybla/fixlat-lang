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
-- |   a <= b  &&  b <= a  ==>  a = b
-- | 
-- | transitivity:
-- |   a <= b  &&  b <= c  ==>  a <= c
-- | ```
class PartialOrd a where comparePartial :: a -> a -> Maybe Ordering

-- | Test whether one value is _strictly less than_ another.
lessThanPartial :: forall a. PartialOrd a => a -> a -> Maybe Boolean
lessThanPartial a1 a2 = a1 `comparePartial` a2 <#> case _ of
  LT -> true
  _ -> false

-- | Test whether one value is _strictly greater than_ another.
greaterThanPartial :: forall a. PartialOrd a => a -> a -> Maybe Boolean
greaterThanPartial a1 a2 = a1 `comparePartial` a2 <#> case _ of
  GT -> true
  _ -> false

-- | Test whether one value is _non-strictly less than_ another.
lessThanOrEqPartial :: forall a. PartialOrd a => a -> a -> Maybe Boolean
lessThanOrEqPartial a1 a2 = a1 `comparePartial` a2 <#> case _ of
  GT -> false
  _ -> true

-- | Test whether one value is _non-strictly greater than_ another.
greaterThanOrEqPartial :: forall a. PartialOrd a => a -> a -> Maybe Boolean
greaterThanOrEqPartial a1 a2 = a1 `comparePartial` a2 <#> case _ of
  LT -> false
  _ -> true

infixl 4 lessThanPartial as <?

infixl 4 lessThanOrEqPartial as <=?

infixl 4 greaterThanPartial as >?

infixl 4 greaterThanOrEqPartial as >=?

minPartial :: forall a. PartialOrd a => a -> a -> Maybe a
minPartial a1 a2 = if_ a1 a2 <$> (a1 <? a2)

maxPartial :: forall a. PartialOrd a => a -> a -> Maybe a
maxPartial a1 a2 = if_ a1 a2 <$> (a1 >? a2)

-- | Join semilattice properties:
-- | ```
-- | join is least upper bound:
-- |   a ∧ b = c  <==>  a <= c && b <= c && (∀ c'. a <= c' && b <= c'  ==>  c <= c')
-- | ```
class PartialOrd a <= JoinSemilattice a where join :: a -> a -> a
infixr 3 join as ∧

-- | Meet semilattice properties:
-- | ```
-- | meet is greatest lower bound:
-- |   a ∨ b = c  <==>  c <= a  &&  c <= b  &&  
-- |                    (∀ c'. c' <= a  &&  c' <= b  ==>  c' <= c)
-- | ```
class PartialOrd a <= MeetSemilattice a where meet :: a -> a -> a
infixr 3 meet as ∨

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
class (MeetSemilattice a, JoinSemilattice a) <= Lattice a
