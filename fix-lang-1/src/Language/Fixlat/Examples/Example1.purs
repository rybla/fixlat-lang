module Language.Fixlat.Examples.Example1 where

import Language.Fixlat.Grammar
import Prelude
import Data.Maybe

{-
module1 :: Module
module1 =
  let
    equiv = Name "equiv"

    unit = Name "()"
  in
    Module
      { label: Label "module1"
      , statements:
          [ SyntaxStatement
              $ Syntax
                  { label: Label "equivalance"
                  , name: equiv
                  , argsCount: 2
                  }
          , SyntaxStatement
              $ Syntax
                  { label: Label "unit"
                  , name: unit
                  , argsCount: 0
                  }
          , let
              a = Name "a"
            in
              RuleStatement
                $ Rule
                    { label: Label "reflection"
                    , params: [ a ]
                    , hyps: []
                    , cons: [ neuExpr equiv [ varExpr a, varExpr a ] ]
                    }
          , QueryStatement
              $ Query
                  { params: []
                  , hyps: []
                  , cons:
                      [ neuExpr equiv [ varExpr unit, varExpr unit ]
                      ]
                  }
          ]
      }
-}
