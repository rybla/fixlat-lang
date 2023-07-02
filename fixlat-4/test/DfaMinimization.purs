module Test.DfaMinimization where

import Data.Tuple
import Data.Tuple.Nested
import Language.Fixlat.Core.Grammar
import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Set as Set
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Effect.Class.Console as Console
import Hole (hole)
import Language.Fixlat.Core.InternalFixpoint (emptyDatabase, fixpoint)
import Language.Fixlat.Core.ModuleT (ModuleCtx, runModuleT)
import Text.Pretty (pretty)

--------------------------------------------------------------------------------
-- Internal representation of Dfa
--------------------------------------------------------------------------------

data Dfa = Dfa
  { startState :: State
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

-- relation live[state]
live :: _
live = do
  let name = Name "live" :: RelationName
  let lattice = state.lattice
  { name
  , lattice
  , data: toDataType lattice
  , make: \s -> Proposition name s
  }

--------------------------------------------------------------------------------
-- databases
--------------------------------------------------------------------------------
_fix_reachability = Name "fix_reachability" :: FixpointSpecName
_fix_live = Name "fix_live" :: FixpointSpecName

--------------------------------------------------------------------------------
-- rules
--------------------------------------------------------------------------------

_starting_reachable = Name "if a state is starting, then it is reachable" :: RuleName
_transition_reachable = Name "if a state has a transition from a reachable state, then it is also reachable" :: RuleName
_accepting_live = Name "if is a state is accepting, then it is live" :: RuleName
_transition_live = Name "if a state has a transition to a live state, then it is also live" :: RuleName

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
        ,
          live.name /\ Relation live.lattice
        ]
    , 
      axioms = Map.unions
        [
          -- starting state
          Map.singleton
            (Name ("starting state"))
            (Axiom (starting.make (state.lit dfa.startState)))
        ,
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
              Tuple _starting_reachable
              -- forall s: state.
              -- starting[s]
              -- |---
              -- reachable[s]
              let s = Name "s" :: TermName in
              QuantificationRule (Left (UniversalQuantification s state.lattice)) $
              PremiseRule (starting.make (state.var s)) $
              ConclusionRule (reachable.make (state.var s))
            ,
              Tuple _transition_reachable
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
              -- TODO: make sure that exists is handled properly
              QuantificationRule (Right (ExistentialQuantification l label.lattice)) $
              PremiseRule (transition.make (state.var s1) (state.var s2) (label.var l)) $
              ConclusionRule (reachable.make (state.var s2))
            ,
              -- forall s: state
              -- accepting[s]
              -- |---
              -- live[s]
              Tuple _accepting_live
              let s = Name "s" :: TermName in
              QuantificationRule (Left (UniversalQuantification s state.lattice)) $
              PremiseRule (accepting.make (state.var s)) $
              ConclusionRule (live.make (state.var s))
            ,
              Tuple _transition_live
              -- forall s2: state
              -- live[s2]
              -- forall s1: state
              -- exists l: label
              -- transition[s1, s2, l]
              -- |---
              -- live[s1]
              let s1 = Name "s1" :: TermName in
              let s2 = Name "s2" :: TermName in
              let l = Name "l" :: TermName in
              QuantificationRule (Left (UniversalQuantification s2 state.lattice)) $ 
              PremiseRule (live.make (state.var s2)) $
              QuantificationRule (Left (UniversalQuantification s1 state.lattice)) $
              QuantificationRule (Right (ExistentialQuantification l label.lattice)) $
              PremiseRule (transition.make (state.var s1) (state.var s2) (label.var l)) $
              ConclusionRule (live.make (state.var s1))
            ]
        ]
    , 
      fixpoints = Map.fromFoldable 
        [ _fix_reachability /\ FixpointSpec
            { axiomNames: Nothing
            , ruleNames: Just [_starting_reachable, _transition_reachable] }
        , _fix_live /\ FixpointSpec
            { axiomNames: Nothing
            , ruleNames: Just [_accepting_live, _transition_live] }
        ]
    }

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main ∷ Effect Unit
main = do
  Console.log "[DfaMinimization.main] Start"

  let 
    dfa = Dfa 
      { startState: 0
      , acceptingStates: Set.singleton 4
      , transitions: 
          [ {start: 0, end: 1, label: 'a'} 
          , {start: 1, end: 2, label: 'a'} 
          , {start: 2, end: 3, label: 'a'} 
          , {start: 3, end: 4, label: 'a'}

          , {start: 10, end: 11, label: 'a'}
          , {start: 12, end: 13, label: 'a'}
          ]
      }

  let 
    ctx :: ModuleCtx
    ctx = 
      { initial_gas: 1000
      , module_: makeModule dfa 
      }

  -- -- reachability
  -- do
  --   let db_reachability = emptyDatabase
  --   Console.log $ "[DfaMinimization.main] Input database:" <> pretty db_reachability <> "\n"
  --   db_reachability' <- runReaderT (runModuleT (fixpoint db_reachability _db_reachability _fix_reachability)) ctx
  --   Console.log $ "[DfaMinimization.main] Output database:" <> pretty db_reachability' <> "\n"

  -- live
  do
    let db_live = emptyDatabase
    Console.log $ "[DfaMinimization.main] Input database:" <> pretty db_live <> "\n"
    db_live' <- runReaderT (runModuleT (fixpoint db_live _fix_live)) ctx
    Console.log $ "[DfaMinimization.main] Output database:" <> pretty db_live' <> "\n"

  Console.log "[DfaMinimization.main] Finish"
  pure unit

