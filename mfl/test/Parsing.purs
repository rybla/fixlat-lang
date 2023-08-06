module Test.Parsing where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Test.Parsing.Term as Term

test :: Effect Unit
test = do
  Console.log "begin parsing tests"
  Term.test
  Console.log "end parsing tests"
