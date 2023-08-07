module Test.Testing where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect as Effect
import Effect.Class.Console as Console

data Test a
  = CrashTest 
      { mx :: Effect a
      , failString :: String -> String
      , passString :: a -> String
      }
  | PredTest 
      { mx :: Effect a
      , pred :: a -> Either String String }

foreign import try_ :: forall a. 
  {- makeErr -} (String -> Either String a) ->
  {- makeOk -} (a -> Either String a) ->
  {- m -} Effect a ->
  Effect (Either String a)

runTest :: forall a.
  Eq a =>
  Test a ->
  Effect Unit

runTest (CrashTest {failString, passString, mx}) = do
  try_ Left Right mx >>= case _ of
    Left err -> do logFail $ failString err
    Right x -> logPass $ passString x

runTest (PredTest {mx, pred}) = do
  x <- mx
  case pred x of
    Left failString -> logFail failString
    Right passString -> logPass passString

logPass :: String -> Effect Unit
logPass failString = Console.log $ "[.] " <> failString

logFail :: String -> Effect Unit
logFail failString = Console.log $ "[X] " <> failString
