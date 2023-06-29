module Test.DfaMinimization where

import Data.Tuple
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Newtype as Newtype
import Data.Set as Set
import Data.String.CodeUnits as CodeUnits
import Effect.Class.Console as Console
import Hole (hole)
import Language.Fixlat.Core.InternalFixpoint (emptyDatabase, fixpoint)
import Language.Fixlat.Core.ModuleT (ModuleCtx, runModuleT)
import Text.Pretty (pretty)

--------------------------------------------------------------------------------
-- Internal representation of Dfa
--------------------------------------------------------------------------------

data Dfa = Dfa
  { states :: Array State
  , startState :: State
  , transitions :: Array Transition
  , acceptingStates :: Set.Set State }

type State = Int

type Transition = {start :: State, end :: State, label :: Char}

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

state :: _
state = do
  let lattice = DiscreteLatticeType IntDataType
  { lattice
  , data: toDataType lattice
  , lit: \i -> ConstructorTerm (IntConstructor i) [] lattice
  , var: \x -> VarTerm x lattice
  }

label :: _
label = do
  let lattice = DiscreteLatticeType StringDataType
  { lattice
  , data: toDataType lattice
  , lit: \c -> ConstructorTerm (StringConstructor (CodeUnits.fromCharArray [c])) [] lattice
  , var: \x -> VarTerm x lattice
  }

--------------------------------------------------------------------------------
-- relations
--------------------------------------------------------------------------------

-- relation transition[state, state, label]
transition :: _
transition = do
  let name = Name "transition" :: RelationName
  let lattice = DiscreteLatticeType ((state.data `TupleDataType` state.data) `TupleDataType` label.data)
  { name
  , lattice
  , data: toDataType lattice
  , make: \s1 s2 l -> Proposition name $
      ConstructorTerm TupleConstructor 
        [ ConstructorTerm TupleConstructor [s1, s2] (TupleLatticeType LexicographicTupleOrdering state.lattice state.lattice)
        , l ]
        lattice
  }

-- relation accepting[state]
accepting :: _
accepting = do
  let name = Name "accepting" :: RelationName
  let lattice = state.lattice
  { name
  , lattice
  , data: toDataType lattice
  , make: \s -> Proposition name s
  }

-- relation starting[state]
starting :: _
starting = do
  let name = Name "starting" :: RelationName
  let lattice = state.lattice
  { name
  , lattice
  , data: toDataType lattice
  , make: \s -> Proposition name s
  }

-- relation reachable[state]
reachable :: _
reachable = do
  let name = Name "reachable" :: RelationName
  let lattice = state.lattice
  { name
  , lattice
  , data: toDataType lattice
  , make: \s -> Proposition name s
  }

--------------------------------------------------------------------------------
-- databases
--------------------------------------------------------------------------------
_db = Name "db" :: DatabaseSpecName
_db_fix = Name "db_fix" :: FixpointSpecName

--------------------------------------------------------------------------------
-- makeModule
--------------------------------------------------------------------------------

makeModule :: Dfa -> Module
makeModule (Dfa dfa) = 
  emptyModule # Newtype.over Module _
    { 
      relations = Map.fromFoldable
        [ 
          transition.name /\ Relation transition.lattice
        ,
          accepting.name /\ Relation accepting.lattice
        ,
          starting.name /\ Relation starting.lattice
        ,
          reachable.name /\ Relation reachable.lattice
        ]
    , 
      axioms = Map.unions
        [
          -- transition axioms
          Map.fromFoldable $
            flip Array.mapWithIndex dfa.transitions \i t ->
              Name ("transition #" <> show i) /\
              Axiom (transition.make (state.lit t.start) (state.lit t.end) (label.lit t.label))
        ,
          -- accepting axioms
          Map.fromFoldable $
            flip Array.mapWithIndex (Set.toUnfoldable dfa.acceptingStates) \i s ->
              Name ("accepting #" <> show i) /\
              Axiom (accepting.make (state.lit s)) 
        ]
    ,
      rules = Map.unions
        [
          Map.fromFoldable
            [ 
              Tuple (Name "if a state is starting, then it is reachable")
              -- forall s: state.
              -- starting[s]
              -- |---
              -- reachable[s]
              let s = Name "s" :: TermName in
              QuantificationRule (Left (UniversalQuantification s state.lattice)) $
              PremiseRule (starting.make (state.var s)) $
              ConclusionRule (reachable.make (state.var s))
            ,
              Tuple (Name "if a state has a transition from a reachable state, then it is also reachable")
              -- forall s1: state
              -- reachable[s1]
              -- forall s2: state
              -- exists l: label
              -- transition[s1, s2, l]
              -- |---
              -- reachable[s2]
              let s1 = Name "s1" :: TermName in
              let s2 = Name "s2" :: TermName in
              let l = Name "l" :: TermName in
              QuantificationRule (Left (UniversalQuantification s1 state.lattice)) $
              PremiseRule (reachable.make (state.var s1)) $
              QuantificationRule (Left (UniversalQuantification s2 state.lattice)) $
              QuantificationRule (Right (ExistentialQuantification l label.lattice)) $
              PremiseRule (transition.make (state.var s1) (state.var s2) (label.var l)) $
              ConclusionRule (reachable.make (state.var s2))
            ]
        ]
    }

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main = do
  Console.log "[DfaMinimization.main] Start"

  let 
    ctx :: ModuleCtx
    ctx = hole "ctx"

  let db = emptyDatabase
  Console.log $ "[Parsing.main] Input database:" <> pretty db <> "\n"
  db' <- runReaderT (runModuleT (fixpoint db _db _db_fix)) ctx
  Console.log $ "[Parsing.main] Output database:" <> pretty db' <> "\n"

  Console.log "[DfaMinimization.main] Finish"
  pure unit