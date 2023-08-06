module Test.Parsing.Term where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Language.Mfl.Surface.Parsing (parseTerm, runParser)
import Text.Pretty (pretty)

test :: Effect Unit
test = do
  test1

test1 :: Effect Unit
test1 = do
  makeTest "Zero"
  makeTest "Suc Zero"
  makeTest "Suc Suc Zero"
  makeTest "True"
  makeTest "False"
  makeTest "\"a literal string\""
  makeTest "Zeta"
  makeTest "{True, False,}"
  makeTest "Sigma"
  makeTest "Tup Zero True"
  makeTest "$f(True, False, Zero, Suc Zero,)"
  makeTest "@x"

makeTest :: String -> Effect Unit
makeTest str = do
  runParser parseTerm str >>= case _ of
    Left err -> Console.log $ "Parsing error: " <> err
    Right a -> Console.log $ show a
