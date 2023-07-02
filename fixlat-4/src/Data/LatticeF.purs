module Data.LatticeF where

import Data.Maybe
import Data.Newtype
import Prelude hiding (join)
import Utility

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
class Functor f <= PartialMOrdF f a where comparePartialF :: a -> a -> f (Maybe Ordering)

-- | Test whether one value is _strictly less than_ another.
lessThanPartialF :: forall f a. PartialMOrdF f a => a -> a -> f (Maybe Boolean)
lessThanPartialF a1 a2 = a1 `comparePartialF` a2 <##> case _ of
  LT -> true
  _ -> false

-- | Test whether one value is _strictly greater than_ another.
greaterThanPartialF :: forall f a. PartialMOrdF f a => a -> a -> f (Maybe Boolean)
greaterThanPartialF a1 a2 = a1 `comparePartialF` a2 <##> case _ of
  GT -> true
  _ -> false

-- | Test whether one value is _non-strictly less than_ another.
lessThanOrEqPartialF :: forall f a. PartialMOrdF f a => a -> a -> f (Maybe Boolean)
lessThanOrEqPartialF a1 a2 = a1 `comparePartialF` a2 <##> case _ of
  GT -> false
  _ -> true

-- | Test whether one value is _non-strictly greater than_ another.
greaterThanOrEqPartialF :: forall f a. PartialMOrdF f a => a -> a -> f (Maybe Boolean)
greaterThanOrEqPartialF a1 a2 = a1 `comparePartialF` a2 <##> case _ of
  LT -> false
  _ -> true

incomparableF :: forall f a. PartialMOrdF f a => a -> a -> f Boolean
incomparableF a1 a2 = isNothing <$> a1 `comparePartialF` a2

infixl 4 comparePartialF as ~?

infixl 4 lessThanPartialF as <?

infixl 4 lessThanOrEqPartialF as <=?

infixl 4 greaterThanPartialF as >?

infixl 4 greaterThanOrEqPartialF as >=?

infixl 4 incomparableF as ><?

minPartialF :: forall f a. PartialMOrdF f a => a -> a -> f (Maybe a)
minPartialF a1 a2 = churchIf a1 a2 <$$> (a1 <? a2)

maxPartialF :: forall f a. PartialMOrdF f a => a -> a -> f (Maybe a)
maxPartialF a1 a2 = churchIf a1 a2 <$$> (a1 >? a2)

-- | Join semilattice properties:
-- | ```
-- | join is least upper bound:
-- |   a ∧ b = c  <==>  a <= c && b <= c && (∀ c'. a <= c' && b <= c'  ==>  c <= c')
-- | ```
class PartialMOrdF f a <= JoinSemilatticeF f a where join :: a -> a -> f a
infixr 3 join as ∧

-- | Bounded join semilattice properties:
-- | ```
-- |   a <= top
-- | ```
class JoinSemilatticeF f a <= BoundedJoinSemilatticeF f a where top :: f a

-- | Meet semilattice properties:
-- | ```
-- | meet is greatest lower bound:
-- |   a ∨ b = c  <==>  c <= a  &&  c <= b  &&  
-- |                    (∀ c'. c' <= a  &&  c' <= b  ==>  c' <= c)
-- | ```
class PartialMOrdF f a <= MeetSemilatticeF f a where meet :: a -> a -> f a
infixr 3 meet as ∨

-- | Bounded join semilattice properties:
-- | ```
-- |   bottom <= a
-- | ```
class MeetSemilatticeF f a <= BoundedMeetSemilatticeF f a where bottom :: f a

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
class (MeetSemilatticeF f a, JoinSemilatticeF f a) <= Lattice f a

class (BoundedMeetSemilatticeF f a, BoundedJoinSemilatticeF f a) <= BoundedLattice f a