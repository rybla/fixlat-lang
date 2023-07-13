module Control.Assert where

import Data.Either.Nested (type (\/))
import Prelude
import Control.Bug (bug)
import Data.Either (Either(..))
import Partial.Unsafe (unsafePartial)

type Assertion a b =
  { label :: String
  , check :: a -> String \/ b
  }

pureAssertion :: forall a. Assertion a a
pureAssertion = {label: "pure", check: Right}

emptyAssertion :: forall a. String -> Assertion a Void
emptyAssertion msg = {label: "(empty) " <> msg, check: const (Left "empty")}

assert :: forall a b c. Assertion a b -> a -> (Partial => b -> c) -> c
assert ass a k = case ass.check a of
  Left msg -> bug $ "Failed assertion " <> ass.label <> ": " <> msg
  Right b -> unsafePartial (k b)

assertI :: forall a b. Assertion a b -> a -> b
assertI ass a = assert ass a identity

contract :: forall a b c d. Assertion a b -> a -> (Partial => b -> c) -> Assertion c d -> d
contract inAss a k outAss = assert inAss a \b ->  assert outAss (k b) identity

assertA :: forall f a b. Applicative f => Assertion a b -> a -> f b
assertA ass a = assert ass a pure

