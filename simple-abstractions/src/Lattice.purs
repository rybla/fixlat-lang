module Lattice where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

class OrdPartial a where
  comparePartial :: a -> a -> Maybe Ordering

-- instance ordOrdPartial :: Ord a => OrdPartial a where
--   comparePartial x y = pure $ compare x y
maxPartial :: forall a. OrdPartial a => a -> a -> Maybe a
maxPartial x y =
  comparePartial x y
    >>= case _ of
        LT -> pure y
        EQ -> pure y
        GT -> pure x

class JoinLattice a where
  -- x \/ y; supremum; least upper bound
  -- properties: commutative, associative, idemptotent
  join :: a -> a -> Maybe a
  -- ⊤
  -- property: forall x, x \/ top = top
  top :: a

class MeetLattice a where
  -- x /\ y; infimum; greatest lower bound
  -- properties: commutative, associative, idemptotent
  meet :: a -> a -> Maybe a
  -- ⊥
  -- property: forall x, x 
  bottom :: a

class (JoinLattice a, MeetLattice a) <= Lattice a
