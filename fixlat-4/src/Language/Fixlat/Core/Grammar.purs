module Language.Fixlat.Core.Grammar where

import Data.Either.Nested
import Data.Tuple.Nested
import Data.Variant
import Prelude hiding (join)
import Prim hiding (Type)

import Control.Assert (Assertion, assert, assertI)
import Control.Assert.Assertions (equal, exactLength, just)
import Control.Assert.Refined (class Refined)
import Control.Bug (bug)
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Lattice (class JoinSemilattice, class MeetSemilattice, class PartialOrd, comparePartial, join, meet)
import Data.List (List(..), (:))
import Data.List as List
import Data.Make (class Make)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (maximum, traverse)
import Hole (hole)
import Prim as Prim
import Text.Pretty (class Pretty, braces, brackets, bullets, indent, lines, parens, pretty, ticks, (<+>))

--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------

newtype Module = Module
  { dataTypes :: Map.Map TypeName DataType
  , latticeTypes :: Map.Map TypeName LatticeType
  , functionSpecs :: Map.Map FunctionName FunctionSpec
  , relations :: Map.Map RelationName Relation
  , rules :: Map.Map RuleName Rule
  , axioms :: Map.Map AxiomName Axiom
  , fixpoints :: Map.Map FixpointSpecName FixpointSpec
  , queries :: Map.Map QuerySpecName QuerySpec
  , insertions :: Map.Map InsertionSpecName InsertionSpec
  }

derive instance Newtype Module _

instance Pretty Module where
  pretty (Module modl) = lines
    [ "module:"
    , indent $ lines
        [ "dataTypes:" <+> indent (pretty modl.dataTypes)
        , "latticeTypes:" <+> indent (pretty modl.latticeTypes)
        , "functionSpecs:" <+> indent (pretty modl.functionSpecs)
        , "relations:" <+> indent (pretty modl.relations)
        , "rules:" <+> indent (pretty modl.rules)
        , "axioms:" <+> indent (pretty modl.axioms)
        , "fixpoints:" <+> indent (pretty modl.fixpoints)
        , "queries:" <+> indent (pretty modl.queries)
        , "insertions:" <+> indent (pretty modl.insertions) ] ]

emptyModule :: Module
emptyModule = Module
  { dataTypes: Map.empty
  , latticeTypes: Map.empty
  , functionSpecs: Map.empty
  , relations: Map.empty
  , rules: Map.empty
  , axioms: Map.empty
  , fixpoints: Map.empty
  , queries: Map.empty
  , insertions: Map.empty }

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A DataType encodes the structure of the Terms of a type, but not a lattice
-- | ordering over them.
data DataType
  = BoolDataType
  | NatDataType
  | StringDataType
  | SetDataType DataType
  | TupleDataType DataType DataType

derive instance Generic DataType _
instance Show DataType where show x = genericShow x
derive instance Eq DataType 
derive instance Ord DataType

instance Pretty DataType where
  pretty = case _ of
    BoolDataType -> "bool"
    NatDataType -> "nat"
    StringDataType -> "string"
    SetDataType ty -> parens ("set " <> pretty ty)
    TupleDataType ty1 ty2 -> parens (pretty ty1 <> ", " <> pretty ty2)

-- | A LatticeType specifies a lattice ordering over a uniquely determined
-- | underlying DataType.
data LatticeType
  = BoolLatticeType
  | NatLatticeType
  | StringLatticeType
  | OpLatticeType LatticeType
  | PowerLatticeType DataType
  | TupleLatticeType TupleOrdering LatticeType LatticeType

derive instance Generic LatticeType _
instance Show LatticeType where show x = genericShow x
derive instance Eq LatticeType 
derive instance Ord LatticeType

instance Pretty LatticeType where
  pretty = case _ of
    BoolLatticeType -> "bool"
    NatLatticeType -> "nat"
    StringLatticeType -> "string"
    OpLatticeType lty -> "op" <+> parens (pretty lty)
    PowerLatticeType dty -> "power" <+> parens (pretty dty)
    TupleLatticeType LexicographicTupleOrdering lty1 lty2 -> parens (pretty lty1 <> ", " <> pretty lty2)
    TupleLatticeType ParallelTupleOrdering lty1 lty2 -> parens (pretty lty1 <> "||" <> pretty lty2)

data TupleOrdering
  = LexicographicTupleOrdering -- first component has precedence over second component
  | ParallelTupleOrdering -- both components are checked together yielding incomparable on mismatch

toDataType :: LatticeType -> DataType
toDataType BoolLatticeType = BoolDataType
toDataType NatLatticeType = NatDataType
toDataType StringLatticeType = StringDataType
toDataType (OpLatticeType lty) = toDataType lty
toDataType (PowerLatticeType dty) = dty
toDataType (TupleLatticeType _ lty1 lty2) = TupleDataType (toDataType lty1) (toDataType lty2)

derive instance Generic TupleOrdering _
instance Show TupleOrdering where show x = genericShow x
derive instance Eq TupleOrdering
derive instance Ord TupleOrdering

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

-- A Term, annotated with type information.
type SymbolicTerm = LatticeTerm TermName
type ConcreteTerm = LatticeTerm Void
type LatticeTerm = Term LatticeType
data Term ty x
  = ApplicationTerm FunctionName (Array (Term ty x)) ty
  | ConstructorTerm Constructor (Array (Term ty x)) ty
  | QuantTerm x ty
  | BoundTerm TermName ty

derive instance Generic (Term ty x) _
instance (Show x, Show ty) => Show (Term ty x) where show x = genericShow x
derive instance (Eq x, Eq ty) => Eq (Term ty x)
derive instance (Ord x, Ord ty) => Ord (Term ty x)
derive instance Bifunctor Term

instance Pretty (Term ty TermName) where
  -- pretty = case _ of
  --   ApplicationTerm fun tm _ -> pretty fun <> parens (pretty tm)
  --   term@(ConstructorTerm prim args _) -> case prim /\ args of
  --     BoolConstructor false /\ [] -> "false"
  --     BoolConstructor true /\ [] -> "true"
  --     ZeroConstructor /\ [] -> "z"
  --     SucConstructor /\ [x'] -> "s" <> pretty x'
  --     StringConstructor s /\ [] -> show s
  --     TupleConstructor /\ [x, y] -> parens (pretty x <> ", " <> pretty y)
  --     SetConstructor /\ xs -> braces (pretty xs)
  --     InfinityConstructor /\ [] -> "infinity"
  --     ZetaConstructor /\ [] -> "zeta"
  --     DomainConstructor /\ [] -> "domain"
  --     _ -> bug $ "invalid ConstructorTerm: " <> show (lmap (const unit) term)
  --   QuantTerm x _ -> pretty x
  --   BoundTerm x _ -> pretty x
  pretty = hole "TODO"

instance Pretty (Term ty Void) where pretty = pretty <<< toSymbolicTerm

typeOfTerm :: forall ty x. Term ty x -> ty
typeOfTerm (ApplicationTerm _ _ ty) = ty
typeOfTerm (ConstructorTerm _ _ ty) = ty
typeOfTerm (QuantTerm _ ty) = ty
typeOfTerm (BoundTerm _ ty) = ty

data Constructor 
  = NatConstructor NatConstructor
  | StringConstructor StringConstructor
  | BoolConstructor Boolean
  | TupleConstructor
  | SetConstructor SetConstructor

derive instance Generic Constructor _
instance Show Constructor where show x = genericShow x
derive instance Eq Constructor
derive instance Ord Constructor

data NatConstructor
  = Zero
  | Suc
  | Infinity -- top nat

derive instance Generic NatConstructor _
instance Show NatConstructor where show x = genericShow x
derive instance Eq NatConstructor
derive instance Ord NatConstructor

data SetConstructor
  = LiteralSet
  | Domain -- top set

derive instance Generic SetConstructor _
instance Show SetConstructor where show x = genericShow x
derive instance Eq SetConstructor
derive instance Ord SetConstructor

data StringConstructor 
  = LiteralString String 
  | Zeta -- top string

derive instance Generic StringConstructor _
instance Show StringConstructor where show x = genericShow x
derive instance Eq StringConstructor
derive instance Ord StringConstructor

instance Pretty Constructor where 
  pretty = case _ of
    NatConstructor Zero -> "zero"
    NatConstructor Suc -> "suc"
    NatConstructor Infinity -> "∞"
    StringConstructor (LiteralString s) -> show s
    StringConstructor Zeta -> show "ζ"
    BoolConstructor b -> show b
    TupleConstructor -> "tuple"
    SetConstructor LiteralSet -> "set"
    SetConstructor Domain -> "domain"

concreteTerm :: Assertion SymbolicTerm ConcreteTerm
concreteTerm = 
  { label: "concreteTerm"
  , check: checkConcreteTerm
  }

checkConcreteTerm :: SymbolicTerm -> Either String ConcreteTerm
checkConcreteTerm = case _ of
  ApplicationTerm fun tms ty -> ApplicationTerm fun <$> checkConcreteTerm `traverse` tms <*> pure ty
  ConstructorTerm prim tms ty -> ConstructorTerm prim <$> traverse checkConcreteTerm tms <*> pure ty
  BoundTerm name ty -> Right $ BoundTerm name ty
  term@(QuantTerm _ _) -> Left $ "Term " <> ticks (show term) <> " is not concrete."

toSymbolicTerm :: forall ty. Term ty Void -> Term ty TermName
toSymbolicTerm = rmap absurd

-- the top bool
trueTerm :: forall x. Term LatticeType x
trueTerm = ConstructorTerm (BoolConstructor true) [] BoolLatticeType

-- the bot bool
falseTerm :: forall x. Term LatticeType x
falseTerm = ConstructorTerm (BoolConstructor false) [] BoolLatticeType

-- the bot nat
zeroTerm :: forall x. Term LatticeType x
zeroTerm = ConstructorTerm (NatConstructor Zero) [] NatLatticeType

-- the top nat
infinityTerm :: forall x. Term LatticeType x
infinityTerm = ConstructorTerm (NatConstructor Infinity) [] NatLatticeType

-- the bot string
epsilonTerm :: forall x. Term LatticeType x
epsilonTerm = ConstructorTerm (StringConstructor (LiteralString "")) [] StringLatticeType

-- the top string
zetaTerm :: forall x. Term LatticeType x
zetaTerm = ConstructorTerm (StringConstructor Zeta) [] StringLatticeType

-- the bot set
nullTerm :: forall x. DataType -> Term LatticeType x
nullTerm dat = ConstructorTerm (SetConstructor LiteralSet) [] (PowerLatticeType dat)

-- the top set
domainTerm :: forall x. DataType -> Term LatticeType x
domainTerm dat = ConstructorTerm (SetConstructor Domain) [] (PowerLatticeType dat)

botTerm :: LatticeType -> ConcreteTerm
botTerm lattice = case lattice of
  BoolLatticeType -> falseTerm
  NatLatticeType -> zeroTerm
  StringLatticeType -> epsilonTerm
  OpLatticeType lat -> topTerm lat
  PowerLatticeType dat -> nullTerm dat
  TupleLatticeType _ lat1 lat2 -> ConstructorTerm TupleConstructor [botTerm lat1, botTerm lat2] lattice

topTerm :: LatticeType -> ConcreteTerm
topTerm lattice = case lattice of
  BoolLatticeType -> trueTerm
  NatLatticeType -> infinityTerm
  StringLatticeType -> zetaTerm
  OpLatticeType lat -> botTerm lat
  PowerLatticeType dat -> domainTerm dat
  TupleLatticeType _ lat1 lat2 -> ConstructorTerm TupleConstructor [topTerm lat1, topTerm lat2] lattice

--------------------------------------------------------------------------------
-- Function
--------------------------------------------------------------------------------

-- TODO: any other metadata that a function needs?
newtype FunctionSpec = FunctionSpec
  { functionType :: FunctionType
  , implementation :: Maybe (Array ConcreteTerm -> ConcreteTerm)
  }

derive instance Newtype FunctionSpec _
instance Show FunctionSpec where
  show (FunctionSpec funSpec) = "FunctionSpec {" <> "functionType: " <> show funSpec.functionType <> ", implementation: " <> case funSpec.implementation of
    Nothing -> "Nothing" <> "}"
    Just _ -> "Just <lambda :: Array ConcreteTerm -> ConcreteTerm>" <> "}"

instance Pretty FunctionSpec where 
  pretty (FunctionSpec funSpec) = "function:" <+> pretty funSpec.functionType

-- | A FunctionType, which can either be a data function (lattice-polymorphic)
-- | or a lattice function (lattice-specific). Each use of a Function must be
-- | monotonic.
data FunctionType = FunctionType (Array DataType) DataType

derive instance Generic FunctionType _
instance Show FunctionType where show x = genericShow x

instance Pretty FunctionType where pretty (FunctionType args ret) = parens (pretty args) <+> "->" <+> pretty ret

--------------------------------------------------------------------------------
-- Relation
--------------------------------------------------------------------------------

-- | A Relation over Terms of a LatticeType. A Relation must be be covariant in
-- | the argument's lattice.
data Relation = Relation LatticeType

instance Pretty Relation where pretty (Relation lty) = "relation:" <+> pretty lty

--------------------------------------------------------------------------------
-- Proposition
--------------------------------------------------------------------------------

-- | A proposition of a particular instance of a Relation.
type SymbolicProposition = Proposition LatticeType TermName
type ConcreteProposition = Proposition LatticeType Void
data Proposition ty x = Proposition RelationName (Term ty x)

derive instance Generic (Proposition ty x) _
instance (Show x, Show ty) => Show (Proposition ty x) where show x = genericShow x
instance (Eq x, Eq ty) => Eq (Proposition ty x) where eq x y = genericEq x y
derive instance Bifunctor Proposition

instance Pretty (Proposition ty TermName) where pretty (Proposition rel tm) = pretty rel <> brackets (pretty tm)
instance Pretty (Proposition ty Void) where pretty = pretty <<< toSymbolicProposition

propositionRelationName :: forall ty x. Proposition ty x -> RelationName
propositionRelationName (Proposition rel _) = rel

-- substituteProposition :: TermSub -> SymbolicProposition -> SymbolicProposition
-- substituteProposition sigma (Proposition rel arg) = Proposition rel (substituteTerm sigma arg)

concreteProposition :: Assertion SymbolicProposition ConcreteProposition
concreteProposition = 
  { label: "concreteProposition"
  , check: \(Proposition rel tm) -> Proposition rel <$> checkConcreteTerm tm
  }

toSymbolicProposition :: forall ty. Proposition ty Void -> Proposition ty TermName
toSymbolicProposition = rmap absurd

{-
instance PartialOrd ConcreteProposition where
  comparePartial (Proposition rel1 arg1) (Proposition rel2 arg2) =
    if rel1 == rel2 
      then comparePartial arg1 arg2
      else Nothing
-}

--------------------------------------------------------------------------------
-- Axiom
--------------------------------------------------------------------------------

newtype Axiom = Axiom ConcreteProposition

derive newtype instance Show Axiom

instance Pretty Axiom where pretty (Axiom prop) = "axiom:" <+> pretty prop

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | An derivation Rule for deriving propositions of a Relation. A Rule is built
-- | as a nested structure where each layer is one of the following:
-- |   - Quantifications, which are introduced into scope for the nested Rule
-- |   - Premise proposition
-- |   - Filter
-- |
-- | Finally, the last layer is the conclusion proposition.
-- |
-- | Requirements:
-- |   - Each existentially-quantified variable must be used _only_ in
-- |     hypotheses.
-- |   - Each universally-quantified variable must be used in hypotheses and the
-- |     conclusion.
-- |   - Each universally-quantified variable must be used in the
-- |     immediately-next hypothesis.

data Rule
  = FilterRule SymbolicTerm Rule
  | QuantificationRule Quantification Rule
  | LetRule TermName SymbolicTerm Rule
  | PremiseRule SymbolicProposition Rule
  | ConclusionRule SymbolicProposition
  -- | DeferRule Rule

derive instance Generic Rule _
instance Show Rule where show x = genericShow x
instance Eq Rule where eq x y = genericEq x y

instance Pretty Rule where
  pretty _rule = "╭\n" <> lines (strs1 <> [hline] <> strs2) <> "\n╰ "
    where
    go :: Array String -> Rule -> Array String /\ Array String
    go strs (FilterRule term rule) = go (Array.snoc strs $ "│ if " <> pretty term) rule
    go strs (PremiseRule prop rule) = go (Array.snoc strs $ "│ " <> pretty prop) rule
    go strs (QuantificationRule quant rule) = go (Array.snoc strs $ "│ " <> pretty quant) rule
    go strs (LetRule name term rule) = go (Array.snoc strs $ "│ let " <> pretty name <> " = " <> pretty term) rule
    go strs (ConclusionRule prop) = strs /\ ["│ " <> pretty prop]

    strs1 /\ strs2 = go [] _rule
    max_length = assertI just $ maximum (String.length <$> strs1 <> strs2)
    padding_right = 4
    hline = "├" <> (String.fromCodePointArray (Array.replicate (max_length + padding_right) (String.codePointFromChar '─')))

-- substituteRule :: TermSub -> Rule -> Rule
-- substituteRule sigma (PremiseRule prem rule) = PremiseRule (substituteProposition sigma prem) (substituteRule sigma rule)
-- substituteRule sigma (FilterRule cond rule) = FilterRule (substituteTerm sigma cond) (substituteRule sigma rule)
-- substituteRule sigma (QuantificationRule quant rule) = QuantificationRule quant (substituteRule sigma rule)
-- substituteRule sigma (LetRule name term rule) = LetRule name (substituteTerm sigma term) (substituteRule sigma rule)
-- substituteRule sigma (ConclusionRule prop) = ConclusionRule (substituteProposition sigma prop)

type Quantification = UniversalQuantification \/ ExistentialQuantification

data UniversalQuantification = UniversalQuantification TermName LatticeType

derive instance Generic UniversalQuantification _
instance Show UniversalQuantification where show x = genericShow x
derive instance Eq UniversalQuantification
derive instance Ord UniversalQuantification

instance Pretty UniversalQuantification where pretty (UniversalQuantification x ty) = "∀" <> parens (pretty x <> ":" <+> pretty ty)

data ExistentialQuantification = ExistentialQuantification TermName LatticeType

derive instance Generic ExistentialQuantification _
instance Show ExistentialQuantification where show x = genericShow x
derive instance Eq ExistentialQuantification
derive instance Ord ExistentialQuantification

instance Pretty ExistentialQuantification where pretty (ExistentialQuantification x ty) = "∃" <> parens (pretty x <> ":" <+> pretty ty)

--------------------------------------------------------------------------------
-- Fixpoint, Query, Insertion
--------------------------------------------------------------------------------

newtype FixpointSpec = FixpointSpec 
  { axiomNames :: Array AxiomName
  , ruleNames :: Array RuleName
  }

derive instance Newtype FixpointSpec _
derive newtype instance Show FixpointSpec

instance Pretty FixpointSpec where
  pretty (FixpointSpec fixpoint) = lines
    [ "fixpoint:"
    , indent $ lines
      [ "axioms:" <+> indent (pretty fixpoint.axiomNames)
      , "rules:" <+> indent (pretty fixpoint.ruleNames) ] ]

data InsertionSpec = InsertionSpec RelationName

derive instance Generic InsertionSpec _
instance Show InsertionSpec where show x = genericShow x

instance Pretty InsertionSpec where
  pretty (InsertionSpec rel) = "insertion" <+> pretty rel

data QuerySpec = QuerySpec Rule

derive instance Generic QuerySpec _
instance Show QuerySpec where show x = genericShow x

instance Pretty QuerySpec where
  pretty (QuerySpec rule) = "query" <+> pretty rule

--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

newtype Name (label :: Symbol) = Name String
derive newtype instance Show (Name label)
derive newtype instance Eq (Name label)
derive newtype instance Ord (Name label)

type TypeName = Name "Type"
type TermName = Name "Term"
type FunctionName = Name "Function"
type RelationName = Name "Relation"
type RuleName = Name "Rule"
type AxiomName = Name "Axiom"
type ModuleName = Name "Module"
type FixpointSpecName = Name "Fixpoint"
type QuerySpecName = Name "Query"
type InsertionSpecName = Name "Insertion"

instance Pretty (Name label) where
  pretty (Name str) = str

--------------------------------------------------------------------------------
-- PartialOrd instances
--------------------------------------------------------------------------------

-- instance PartialOrd ConcreteProposition where
--   comparePartial (Proposition rel1 arg1) (Proposition rel2 arg2) 
--     | otherwise = Nothing
--     | rel1 == rel2 = comparePartial arg1 arg2

-- instance MeetSemilattice ConcreteProposition where
--   meet (Proposition rel1 arg1) (Proposition rel2 arg2) 
--     | rel1 == rel2 = Proposition rel1 (join arg1 arg2)
--     | otherwise = bug $ "meet @ConcreteProposition: Should not take the meet of propositions of different relations."

-- instance JoinSemilattice ConcreteProposition where
--   join (Proposition rel1 arg1) (Proposition rel2 arg2) 
--     | rel1 == rel2 = Proposition rel1 (meet arg1 arg2)
--     | otherwise = bug $ "join @ConcreteProposition: Should not take the join of propositions of different relations."

-- instance PartialOrd ConcreteTerm where
--   comparePartial x y = do
--     let lat1 = typeOfTerm x
--     let lat2 = typeOfTerm y
--     let lat = assertI equal (lat1 /\ lat2)
--     case lat /\ x /\ y of
--       OpLatticeType _ /\ _ /\ _ -> comparePartial y x
--       TupleLatticeType LexicographicTupleOrdering _ _ /\ ConstructorTerm TupleConstructor [x1, x2] _ /\ ConstructorTerm TupleConstructor [y1, y2] _ -> 
--         case comparePartial x1 y1 of
--           Just EQ -> comparePartial x2 y2
--           mo -> mo
--       TupleLatticeType ParallelTupleOrdering _ _ /\ ConstructorTerm TupleConstructor [x1, x2] _ /\ ConstructorTerm TupleConstructor [y1, y2] _ -> do
--         let mo1 = comparePartial x1 y1
--         let mo2 = comparePartial x2 y2
--         if mo1 == mo2 then mo1 else Nothing
--       PowerLatticeType _ /\ ConstructorTerm SetConstructor xs _ /\ ConstructorTerm SetConstructor ys _ ->
--         if flip Array.all ys \y -> flip Array.any xs \x -> x == y
--         then if Array.length ys == Array.length xs 
--           then Just EQ
--           else Just GT
--         else Nothing
--       BoolLatticeType /\ ConstructorTerm (BoolConstructor a) [] BoolLatticeType /\ ConstructorTerm (BoolConstructor b) [] BoolLatticeType -> Just (a `compare` b)
      
--       NatLatticeType /\ ConstructorTerm ZeroConstructor [] NatLatticeType /\ ConstructorTerm ZeroConstructor [] NatLatticeType -> Just EQ
--       NatLatticeType /\ ConstructorTerm ZeroConstructor [] NatLatticeType /\ ConstructorTerm SucConstructor [y'] NatLatticeType -> Just LT
--       NatLatticeType /\ ConstructorTerm SucConstructor [x'] NatLatticeType /\ ConstructorTerm ZeroConstructor [] NatLatticeType -> Just GT
--       NatLatticeType /\ ConstructorTerm SucConstructor [x'] NatLatticeType /\ ConstructorTerm SucConstructor [y'] NatLatticeType -> x' `comparePartial` y'
      
--       StringLatticeType /\ ConstructorTerm (StringConstructor x) [] StringLatticeType /\ ConstructorTerm (StringConstructor y) [] StringLatticeType ->  Just (x `compare` y)
--       _ -> bug $ "comparePartial @ConcreteTerm: invalid case: " <> ticks (show x) <> ", " <> ticks (show y)

-- instance MeetSemilattice ConcreteTerm where
--   meet x y = do
--     let lat1 = typeOfTerm x
--     let lat2 = typeOfTerm y
--     let lat = assertI equal (lat1 /\ lat2)
--     case lat /\ x /\ y of
--       OpLatticeType _ /\ _ /\ _ -> join x y
--       TupleLatticeType LexicographicTupleOrdering _ _ /\ ConstructorTerm TupleConstructor [x1, x2] _ /\ ConstructorTerm TupleConstructor [y1, y2] _ -> ConstructorTerm TupleConstructor [meet x1 y1, meet x2 y2] lat
--       TupleLatticeType ParallelTupleOrdering _ _ /\ ConstructorTerm TupleConstructor [x1, x2] _ /\ ConstructorTerm TupleConstructor [y1, y2] _ -> ConstructorTerm TupleConstructor [meet x1 y1, meet x2 y2] lat
--       PowerLatticeType _ /\ ConstructorTerm SetConstructor xs _ /\ ConstructorTerm SetConstructor ys _ -> ConstructorTerm SetConstructor (union xs ys) lat
--       BoolLatticeType /\ ConstructorTerm (BoolConstructor a) [] BoolLatticeType /\ ConstructorTerm (BoolConstructor b) [] BoolLatticeType -> ConstructorTerm (BoolConstructor (a && b)) [] BoolLatticeType

--       NatLatticeType /\ ConstructorTerm ZeroConstructor []   _ /\ ConstructorTerm ZeroConstructor []   _ -> ConstructorTerm ZeroConstructor [] lat
--       NatLatticeType /\ ConstructorTerm ZeroConstructor []   _ /\ ConstructorTerm SucConstructor  [y'] _ -> ConstructorTerm SucConstructor  [y'] lat
--       NatLatticeType /\ ConstructorTerm SucConstructor  [x'] _ /\ ConstructorTerm ZeroConstructor []   _ -> ConstructorTerm SucConstructor  [x'] lat
--       NatLatticeType /\ ConstructorTerm SucConstructor  [x'] _ /\ ConstructorTerm SucConstructor  [y'] _ -> ConstructorTerm SucConstructor [meet x' y'] lat

--       StringLatticeType /\ _ /\ _ | ConstructorTerm (StringConstructor "") [] StringLatticeType `Array.elem` [x, y] -> ConstructorTerm (StringConstructor "") [] StringLatticeType

--       StringLatticeType /\ ConstructorTerm (StringConstructor s) [] _ /\ ConstructorTerm (StringConstructor t) [] _ ->
--         case String.stripPrefix (String.Pattern s) t of
--           Nothing -> case String.stripPrefix (String.Pattern t) s of
--             Nothing -> ConstructorTerm ZetaConstructor [] lat -- s nor t is a is a prefix of the other, so go to top
--             Just t' -> ConstructorTerm (StringConstructor s) [] lat -- t is a prefix of s, so t <= s
--           Just s' -> ConstructorTerm (StringConstructor t) [] lat -- s is a prefix of t, so s <= t
      
--       StringLatticeType /\ _ /\ _ | Just i <- ConstructorTerm ZetaConstructor [] StringLatticeType `Array.elemIndex` [x, y] -> if i == 1 then x else y

--       _ -> bug $ "meet @ConcreteTerm: invalid case: " <> ticks (show x) <> ", " <> ticks (show y)
--     where
--     union xy ys = Set.toUnfoldable (Set.union (Set.fromFoldable xy) (Set.fromFoldable ys))

-- instance JoinSemilattice ConcreteTerm where 
--   join x y = do
--     let lat1 = typeOfTerm x
--     let lat2 = typeOfTerm y
--     let lat = assertI equal (lat1 /\ lat2)
--     case lat /\ x /\ y of
--       OpLatticeType _ /\ _ /\ _ -> meet x y
--       TupleLatticeType LexicographicTupleOrdering _ _ /\ ConstructorTerm TupleConstructor [x1, x2] _ /\ ConstructorTerm TupleConstructor [y1, y2] _ -> ConstructorTerm TupleConstructor [join x1 y1, join x2 y2] lat
--       TupleLatticeType ParallelTupleOrdering _ _ /\ ConstructorTerm TupleConstructor [x1, x2] _ /\ ConstructorTerm TupleConstructor [y1, y2] _ -> ConstructorTerm TupleConstructor [join x1 y1, join x2 y2] lat
--       PowerLatticeType _ /\ ConstructorTerm SetConstructor xs _ /\ ConstructorTerm SetConstructor ys _ -> ConstructorTerm SetConstructor (intersection xs ys) lat
--       BoolLatticeType /\ ConstructorTerm (BoolConstructor a) [] BoolLatticeType /\ ConstructorTerm (BoolConstructor b) [] BoolLatticeType -> ConstructorTerm (BoolConstructor (a && b)) [] BoolLatticeType

--       NatLatticeType /\ ConstructorTerm ZeroConstructor []   _ /\ ConstructorTerm ZeroConstructor []   _ -> ConstructorTerm ZeroConstructor [] lat
--       NatLatticeType /\ ConstructorTerm ZeroConstructor []   _ /\ ConstructorTerm SucConstructor  [y'] _ -> ConstructorTerm SucConstructor  [y'] lat
--       NatLatticeType /\ ConstructorTerm SucConstructor  [x'] _ /\ ConstructorTerm ZeroConstructor []   _ -> ConstructorTerm SucConstructor  [x'] lat
--       NatLatticeType /\ ConstructorTerm SucConstructor  [x'] _ /\ ConstructorTerm SucConstructor  [y'] _ -> ConstructorTerm SucConstructor [join x' y'] lat

--       StringLatticeType /\ ConstructorTerm (StringConstructor s) [] _ /\ ConstructorTerm (StringConstructor t) [] _ ->
--         case String.stripPrefix (String.Pattern s) t of
--           Nothing -> case String.stripPrefix (String.Pattern t) s of
--             Nothing -> ConstructorTerm (StringConstructor "") [] lat -- s nor t is a is a prefix of the other, so go to bot
--             Just t' -> ConstructorTerm (StringConstructor t) [] lat -- t is a prefix of s, so t <= s
--           Just s' -> ConstructorTerm (StringConstructor s) [] lat -- s is a prefix of t, so s <= t
--       _ -> bug "join @ConcreteTerm: invalid case"
--     where
--     intersection xs ys = Set.toUnfoldable (Set.intersection (Set.fromFoldable xs) (Set.fromFoldable ys))

