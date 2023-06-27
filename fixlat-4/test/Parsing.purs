module Test.Parsing where

import Language.Fixlat.Core.Grammar
import Prelude

import Control.Monad.Reader (runReaderT)
import Effect (Effect)
import Effect.Class.Console as Console
import Hole (hole)
import Language.Fixlat.Core.InternalFixpoint (emptyDatabase, fixpoint)
import Language.Fixlat.Core.ModuleT (ModuleCtx, runModuleT)
import Text.Pretty (pretty)

-- relation: parsed

_parsed = Name "parsed" :: RelationName

parsed_domain :: LatticeType
parsed_domain = hole "parsed_domain"

parsed :: forall x. Term LatticeType x -> Term LatticeType x -> Term LatticeType x -> Proposition LatticeType x
parsed i1 i2 c = Proposition _parsed $
  PrimitiveTerm TuplePrimitive
    [ PrimitiveTerm TuplePrimitive [i1, i2] 
        (TupleLatticeType LexicographicTupleOrdering 
          (typeOfTerm i1)
          (typeOfTerm i2) )
    , c ]
    parsed_domain

-- database: db

_db = Name "db" :: DatabaseSpecName
_db_fix = Name "db_fix" :: FixpointSpecName

data Grammar -- = TODO

-- module

makeModule :: Grammar -> Module
makeModule = hole "makeModule"

-- main

main :: Effect Unit
main = do
  Console.log "[Parsing.main] Start"
  let
    grammar = hole "grammar" :: Grammar

    ctx :: ModuleCtx
    ctx = 
      { module_: makeModule grammar
      , initial_gas: 100 }

  let db = emptyDatabase
  Console.log $ "[Parsing.main] Input database:\n" <> pretty db <> "\n"
  db' <- runReaderT (runModuleT (fixpoint db _db _db_fix)) ctx
  Console.log $ "[Parsing.main] Output database:\n" <> pretty db' <> "\n"

  pure unit





