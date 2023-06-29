module Test.Parsing where

import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude

import Control.Bug (bug)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalState, get, modify, modify_, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Make (make)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Class.Console as Console
import Hole (hole)
import Language.Fixlat.Core.InternalFixpoint (emptyDatabase, fixpoint)
import Language.Fixlat.Core.ModuleT (ModuleCtx, runModuleT)
import Text.Pretty (pretty)

forAll = map (Left <<< uncurry UniversalQuantification)
exists = map (Right <<< uncurry ExistentialQuantification)

tupleLex = TupleLatticeType LexicographicTupleOrdering

lty_index = IntLatticeType
dty_index = IntDataType
lit_index i = ConstructorTerm (IntConstructor i) [] lty_index
var_index x = VarTerm x lty_index

lty_symbol = StringLatticeType
dty_symbol = StringDataType
lit_symbol c = ConstructorTerm (StringConstructor (CodeUnits.singleton c)) [] lty_symbol
var_symbol x = VarTerm x lty_symbol

{-
Set : DataType -> DataType
PowerSetLattice : DataType -> DataType
PowerSetLattice(D) = Set(D) with subset ordering
-}

{-
actually its over 

  subset_lattice(power_set(nat x nat x sym))
-}

-- relation: parsed

_parsed = Name "parsed" :: RelationName

lty_parsed :: LatticeType
lty_parsed = DiscreteLatticeType dty_parsed

dty_parsed :: DataType
dty_parsed = (dty_index `TupleDataType` dty_index) `TupleDataType` dty_symbol

parsed :: forall x. Term LatticeType x -> Term LatticeType x -> Term LatticeType x -> Proposition LatticeType x
parsed i1 i2 c = Proposition _parsed $
  ConstructorTerm TupleConstructor
    [ ConstructorTerm TupleConstructor [i1, i2] 
        (lty_index `tupleLex` lty_index)
    , c ]
    lty_parsed

-- database: db

_db = Name "db" :: DatabaseSpecName
_db_fix = Name "db_fix" :: FixpointSpecName

data Grammar = Grammar
  { nonterminals :: Map.Map Char (Array String) }

makeGrammarRulesAndAxioms :: Grammar -> {axioms :: Map.Map AxiomName Axiom, rules :: Map.Map RuleName Rule}
makeGrammarRulesAndAxioms (Grammar grammar) =
  {
    axioms: Map.empty
  ,
    rules: Map.fromFoldable $ Array.concat $ Map.toUnfoldable grammar.nonterminals <#> \(nt /\ forms) ->
      flip Array.mapWithIndex forms \formIx _form ->
        Name ("nonterminal_" <> pretty nt <> "_" <> show formIx) /\ 
        let
          makeVarIndex i = Name ("i" <> show i) :: TermName
          prevIndex = get >>= \i -> pure (makeVarIndex i)
          nextIndex = modify (_ + 1) >>= \i -> pure (makeVarIndex i)

          go isFirst form = case Array.uncons form of
            Nothing -> bug "[makeGrammarRulesAndAxioms] empty form"
            Just {head: sym, tail: form'} -> do
              i0 <- prevIndex
              i1 <- nextIndex

              (if isFirst then QuantificationRule (Left (UniversalQuantification i0 lty_index)) else identity) <<<
                QuantificationRule (Left (UniversalQuantification i1 lty_index)) <<<
                PremiseRule (parsed (var_index i0) (var_index i1) (lit_symbol sym)) <$>
                if Array.null form' then 
                  pure (ConclusionRule (parsed (var_index (makeVarIndex 0)) (var_index i1) (lit_symbol nt)))
                else
                  go false form'

              -- HypothesisRule
              --   { quantifications: make (forAll ((if isFirst then [i0 /\ lty_index] else []) <> [i1 /\ lty_index]))
              --   , proposition: parsed (var_index i0) (var_index i1) (lit_symbol sym)
              --   , filter: Nothing } <$>
              --   if Array.null form' then 
              --     pure $ Right $ parsed (var_index (makeVarIndex 0)) (var_index i1) (lit_symbol nt)
              --   else
              --     Left <$> go false form'
        in
        evalState (go true (CodeUnits.toCharArray _form)) 0
  }

makeInputAxioms :: String -> Map.Map AxiomName Axiom
makeInputAxioms str =
  Map.fromFoldable $ flip Array.mapWithIndex (CodeUnits.toCharArray str) \i c ->
    Name ("input_" <> show i) /\
    Axiom (parsed (lit_index i) (lit_index (i + 1)) (lit_symbol c))

-- module

makeModule :: Grammar -> String -> Module
makeModule grammar input = do
  let
    {axioms: grammarAxioms, rules: grammarRules} = makeGrammarRulesAndAxioms grammar
    inputAxioms = makeInputAxioms input

    axioms = grammarAxioms `Map.union` inputAxioms
    rules = grammarRules

  emptyModule # Newtype.over Module _
    {
      relations = Map.fromFoldable
        [
          Tuple _parsed $ Relation lty_parsed
        ]
    ,
      axioms = axioms
    , 
      rules = rules
    ,
      databaseSpecs = Map.fromFoldable
        [
          Tuple _db $ emptyDatabaseSpec # Newtype.over DatabaseSpec _
            { 
              fixpoints = Map.fromFoldable
                [
                  Tuple _db_fix $ FixpointSpec
                    {
                      axiomNames: Array.fromFoldable $ Map.keys axioms
                    ,
                      ruleNames: Array.fromFoldable $ Map.keys rules
                    }
                ]
            }
        ]
    }

-- main

main :: Effect Unit
main = do
  Console.log "[Parsing.main] Start"
  let
    -- grammar = Grammar
    --   { nonterminals: Map.fromFoldable
    --       [ 'X' /\ [ "Y", "XX" ]
    --       , 'Y' /\ [ "ab", "ba" ] ] }
    -- input = "ababa"

    grammar = Grammar
      { nonterminals: Map.fromFoldable
          [ 'S' /\ [ "a", "(S)", "SS" ] ] }
    input = 
      -- "(a)" -- works
      -- "(a)(a)" -- works
      "((a))" -- TODO: doesn't work
      -- "(a)(((aa)))" -- TODO: doesn't work

    ctx :: ModuleCtx
    ctx = 
      { module_: makeModule grammar input
      , initial_gas: 1000 }

  let db = emptyDatabase
  Console.log $ "[Parsing.main] Input database:" <> pretty db <> "\n"
  db' <- runReaderT (runModuleT (fixpoint db _db _db_fix)) ctx
  Console.log $ "[Parsing.main] Output database:" <> pretty db' <> "\n"

  pure unit





