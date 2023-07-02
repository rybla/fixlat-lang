module Data.LatticeM where

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
class Monad m <= PartialMOrdM m a where comparePartialM :: a -> a -> m (Maybe Ordering)

-- | Test whether one value is _strictly less than_ another.
lessThanPartialM :: forall a m. PartialMOrdM m a => a -> a -> m (Maybe Boolean)
lessThanPartialM a1 a2 = a1 `comparePartialM` a2 <##> case _ of
  LT -> true
  _ -> false

-- | Test whether one value is _strictly greater than_ another.
greaterThanPartialM :: forall m a. PartialMOrdM m a => a -> a -> m (Maybe Boolean)
greaterThanPartialM a1 a2 = a1 `comparePartialM` a2 <##> case _ of
  GT -> true
  _ -> false

-- | Test whether one value is _non-strictly less than_ another.
lessThanOrEqPartialM :: forall m a. PartialMOrdM m a => a -> a -> m (Maybe Boolean)
lessThanOrEqPartialM a1 a2 = a1 `comparePartialM` a2 <##> case _ of
  GT -> false
  _ -> true

-- | Test whether one value is _non-strictly greater than_ another.
greaterThanOrEqPartialM :: forall m a. PartialMOrdM m a => a -> a -> m (Maybe Boolean)
greaterThanOrEqPartialM a1 a2 = a1 `comparePartialM` a2 <##> case _ of
  LT -> false
  _ -> true

incomparableM :: forall m a. PartialMOrdM m a => a -> a -> m Boolean
incomparableM a1 a2 = isNothing <$> a1 `comparePartialM` a2

infixl 4 comparePartialM as ~?

infixl 4 lessThanPartialM as <?

infixl 4 lessThanOrEqPartialM as <=?

infixl 4 greaterThanPartialM as >?

infixl 4 greaterThanOrEqPartialM as >=?

infixl 4 incomparableM as ><?

minPartialM :: forall m a. PartialMOrdM m a => a -> a -> m (Maybe a)
minPartialM a1 a2 = churchIf a1 a2 <$$> (a1 <? a2)

maxPartialM :: forall m a. PartialMOrdM m a => a -> a -> m (Maybe a)
maxPartialM a1 a2 = churchIf a1 a2 <$$> (a1 >? a2)

-- | Join semilattice properties:
-- | ```
-- | join is least upper bound:
-- |   a ∧ b = c  <==>  a <= c && b <= c && (∀ c'. a <= c' && b <= c'  ==>  c <= c')
-- | ```
class PartialMOrdM m a <= JoinSemilatticeM m a where join :: a -> a -> m a
infixr 3 join as ∧

-- | Bounded join semilattice properties:
-- | ```
-- |   a <= top
-- | ```
class JoinSemilatticeM m a <= BoundedJoinSemilatticeM m a where top :: m a

-- | Meet semilattice properties:
-- | ```
-- | meet is greatest lower bound:
-- |   a ∨ b = c  <==>  c <= a  &&  c <= b  &&  
-- |                    (∀ c'. c' <= a  &&  c' <= b  ==>  c' <= c)
-- | ```
class PartialMOrdM m a <= MeetSemilatticeM m a where meet :: a -> a -> m a
infixr 3 meet as ∨

-- | Bounded join semilattice properties:
-- | ```
-- |   bottom <= a
-- | ```
class MeetSemilatticeM m a <= BoundedMeetSemilatticeM m a where bottom :: m a

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
class (MeetSemilatticeM m a, JoinSemilatticeM m a) <= Lattice m a

class (BoundedMeetSemilatticeM m a, BoundedJoinSemilatticeM m a) <= BoundedLattice m a