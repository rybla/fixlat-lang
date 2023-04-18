{-# LANGUAGE LambdaCase #-}

module Data.Lattice where

import Data.Functor

-- In this project, we are typically _lattice-climbing_, which implies:
-- - "higher" points implies "lower" points
-- - the "join" of two points is the "lowest" point that implies both
-- - the "meet" of two points is the "highest" point that is implied by both
-- - the "top" of the lattice is logical bottom (implies everything)
-- - the "bottom" of the lattice is logical top (only implies tautologies)

-- | A lattice is a partially-ordered set that, for any finite subset, has a
-- join (least upper bound), `(\/)`, and a meet (greatest lower bound), `(/\)`,
-- that satisfy the following properties.
-- - commutativity: `x /\/ y = y /\/ x`
-- - associativity: `a /\/ (b /\/ c) = (a /\/ b) /\/ c`
-- - absorbtion: `a /\/ (a \/\ b) = a`
-- - idempotentcy: `a /\/ a = a`
class Lattice a where
  (/\) :: a -> a -> a
  (\/) :: a -> a -> a

infixr 5 /\, \/

-- | A _partial ordering_ satisfies the following properties:
-- - reflexivity: `x <= x`
-- - antisymmetry: `(a <= b && b <= a) ==> (a == b)`
-- - transitivity: `(a <= b && b <= c) ==> (a <= c)`
class PartialOrd a where
  comparePartial :: a -> a -> Maybe Ordering

(<?) :: PartialOrd a => a -> a -> Maybe Bool
x <? y =
  comparePartial x y <&> \case
    LT -> True
    _ -> False

(<=?) :: PartialOrd a => a -> a -> Maybe Bool
x <=? y =
  comparePartial x y <&> \case
    LT -> True
    EQ -> True
    _ -> False

(>?) :: PartialOrd a => a -> a -> Maybe Bool
x >? y = not <$> (x <=? y)

(>=?) :: PartialOrd a => a -> a -> Maybe Bool
x >=? y = not <$> (x >? y)

infix 4 <?, <=?, >?, >=?
