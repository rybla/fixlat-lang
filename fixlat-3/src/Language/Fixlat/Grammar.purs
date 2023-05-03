module Language.Fixlat.Grammar where

import Prelude
import Prim hiding (Type)

import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, rmap)
import Data.Bitraversable (class Bitraversable, rtraverse)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable, intercalate, traverse)
import Text.Pretty (class Pretty, pretty, (<+>))

newtype Label = Label String
derive instance Generic Label _
derive instance Newtype Label _
derive newtype instance Show Label
derive newtype instance Eq Label
derive newtype instance Ord Label
instance Pretty Label where pretty (Label str) = str

newtype Var = Var String
derive instance Generic Var _
instance Show Var where show x = genericShow x
instance Pretty Var where pretty (Var str) = str
derive newtype instance Eq Var
derive newtype instance Ord Var

type CX = Var -- type of variables in a concrete structure

-- | Module
newtype Module y = Module
  { label :: Label
  , stmts :: Array (Stmt y)
  }

derive instance Generic (Module y) _
instance Show y => Show (Module y) where show x = genericShow x
derive instance Functor Module
derive instance Foldable Module
derive instance Traversable Module

-- | Statement
data Stmt y
  = PredStmt Pred
  | RuleStmt (Rule y CX)

derive instance Generic (Stmt y) _
instance Show y => Show (Stmt y) where show x = genericShow x
derive instance Functor Stmt
derive instance Foldable Stmt
derive instance Traversable Stmt

-- | Predicate
newtype Pred = Pred
  { label :: Label
  , bind :: CX
  , param :: CLat
  }

instance Pretty Pred where
  pretty (Pred p) = "pred" <+> pretty p.label <+> ":" <+> pretty p.bind <+> "(" <> pretty p.param <> ")"

derive instance Generic Pred _
instance Show Pred where show x = genericShow x

-- | Inference Rule
newtype Rule y xt = Rule
  { label :: Label
  , params :: Array (Param xt)
  , hyps :: Array (Prop y xt)
  , con :: Prop y xt
  }

instance (Pretty y, Pretty xt) => Pretty (Rule y xt) where
  pretty (Rule r) = 
    "rule" <+> pretty r.label <+>
    "(" <> intercalate ", " (pretty <$> r.params) <> ")" <+> 
    "{" <>
      intercalate ", " (pretty <$> r.hyps) <+>
      "|-" <+>
      pretty r.con <>
    "}"

derive instance Generic (Rule y xt) _
instance (Show y, Show xt) => Show (Rule y xt) where show x = genericShow x
derive instance Newtype (Rule y xt) _
derive instance Bifunctor Rule 
derive instance Bifoldable Rule 
derive instance Bitraversable Rule

instance Functor (Rule y) where map f x = rmap f x
instance Foldable (Rule y) where
  foldr f b x = bifoldr (flip const) f b x
  foldl f b x = bifoldl const f b x
  foldMap f x = bifoldMap mempty f x
instance Traversable (Rule y) where
  traverse f x = rtraverse f x
  sequence x = traverse identity x

-- | A parameter is quantified, named, and typed.
newtype Param xt = Param { quant :: Quant, bind :: xt, type_ :: CLat }

instance Pretty xt => Pretty (Param xt) where
  pretty (Param p) = pretty p.quant <+> "(" <> pretty p.quant <+> pretty p.bind <+> ":" <+> pretty p.type_ <> ")"

derive instance Newtype (Param xt) _
derive newtype instance Show xt => Show (Param xt)
derive instance Generic (Param xt) _
derive instance Functor Param
derive instance Foldable Param
derive instance Traversable Param

data Quant = UnivQuant | ExistQuant
derive instance Generic Quant  _
instance Show Quant where show x = genericShow x
instance Eq Quant where eq x y = genericEq x y

instance Pretty Quant where
  pretty UnivQuant = "∀"
  pretty ExistQuant = "∃"

-- | A proposition is a predicate applied to an argument term.
newtype Prop y xt = Prop { pred :: CX, arg :: Term y xt }

instance (Pretty y, Pretty xt) => Pretty (Prop y xt) where
  pretty (Prop p) = pretty p.pred <> " " <> pretty p.arg

derive instance Generic (Prop y xt) _
instance (Show y, Show xt) => Show (Prop y xt) where show x = genericShow x
derive instance Newtype (Prop y xt) _
derive instance Bifunctor Prop 
derive instance Bifoldable Prop 
derive instance Bitraversable Prop 

instance Functor (Prop y) where map f x = rmap f x
instance Foldable (Prop y) where
  foldr f b x = bifoldr (flip const) f b x
  foldl f b x = bifoldl const f b x
  foldMap f x = bifoldMap mempty f x
instance Traversable (Prop y) where
  traverse f x = rtraverse f x
  sequence x = traverse identity x

fromPropToRule :: Label -> CProp -> CRule
fromPropToRule label con = Rule
  { label
  , params: []
  , hyps: []
  , con
  }

-- | Lattice
data Lat xy
  = AtomicLat AtomicLat
  | VarLat xy
  | -- | the lattice where `∀ a b. a >< b`
    DiscreteLat (Type xy)
  | -- | the lattice over integers where join is minimum and meet is maximum
    PointLat (Type xy)
  | SumLat (Lat xy) (Lat xy)
  | ProdLat (Lat xy) (Lat xy)
  | NegLat (Lat xy)
  | FixLat xy (Lat xy)

instance Pretty xy => Pretty (Lat xy) where 
  pretty (AtomicLat ay) = pretty ay
  pretty (VarLat x) = pretty x
  pretty (DiscreteLat y) = "(Discrete " <> pretty y <> ")"
  pretty (PointLat y) = "(Point " <> pretty y <> ")"
  pretty (SumLat l1 l2) = "(" <> pretty l1 <> " | " <> pretty l2 <> ")"
  pretty (ProdLat l1 l2) = "(" <> pretty l1 <> " * " <> pretty l2 <> ")"
  pretty (FixLat x l) = "(fix " <> pretty x <> " in " <> pretty l <> ")"
  pretty (NegLat l) = "(Neg " <> pretty l <> ")"

derive instance Generic (Lat xy) _
instance Show xy => Show (Lat xy) where show x = genericShow x
-- over type variables
derive instance Functor Lat
derive instance Foldable Lat
derive instance Traversable Lat

data AtomicLat
  = UnitLat
  | IntLat

instance Pretty AtomicLat where
  pretty UnitLat = "Unit"
  pretty IntLat = "Int"

derive instance Generic AtomicLat _
instance Show AtomicLat where show x = genericShow x

unitLat = AtomicLat UnitLat
intLat = AtomicLat IntLat

-- folds right
prodsLat :: forall xy. Partial => Array (Lat xy) -> Lat xy
prodsLat = ArrayNE.fromArray >>> fromJust >>> ArrayNE.foldr1 ProdLat

-- folds right
sumsLat :: forall xy. Partial => Array (Lat xy) -> Lat xy
sumsLat = ArrayNE.fromArray >>> fromJust >>> ArrayNE.foldr1 SumLat

-- | Type
data Type xy
  = AtomicType AtomicType
  | VarType xy
  | SumType (Type xy) (Type xy)
  | ProdType (Type xy) (Type xy)
  | FixType xy (Type xy)

instance Pretty xy => Pretty (Type xy) where
  pretty (AtomicType ay) = pretty ay
  pretty (VarType x) = pretty x
  pretty (SumType y1 y2) = "(" <> pretty y1 <> " | " <> pretty y2 <> ")"
  pretty (ProdType y1 y2) = "(" <> pretty y1 <> " * " <> pretty y2 <> ")"
  pretty (FixType x y) = "(fix " <> pretty x <> " in " <> pretty y <> ")"

derive instance Generic (Type xy) _
instance Show xy => Show (Type xy) where show x = genericShow x
-- over lattice annotations and type variables
derive instance Functor Type
derive instance Foldable Type
derive instance Traversable Type

data AtomicType
  = UnitType
  | NatType

instance Pretty AtomicType where
  pretty UnitType = "Unit"
  pretty NatType = "Nat"

derive instance Generic AtomicType  _
instance Show AtomicType where show x = genericShow x

-- | Term 
data Term y xt
  = AtomicTerm AtomicTerm y
  | VarTerm xt y
  -- product
  | ProdTerm (Term y xt) (Term y xt) y
  -- !TODO not sure if i wanna add these, cuz then i have to deal with more complicated forms of unification
  | Proj1Term (Term y xt) y
  | Proj2Term (Term y xt) y
  -- sum
  | Inj1Term (Term y xt) y
  | Inj2Term (Term y xt) y
  -- !TODO not sure if i wanna add this, cuz then i have to deal with more complicated forms of unification
  -- | ElimTerm (Term y xt) CX (Term y xt) CX (Term y xt) y 

termAnn :: forall y xt. Term y xt -> y
termAnn (AtomicTerm _ y) = y
termAnn (VarTerm _ y) = y
termAnn (ProdTerm _ _ y) = y
termAnn (Proj1Term _ y) = y
termAnn (Proj2Term _ y) = y
termAnn (Inj1Term _ y) = y
termAnn (Inj2Term _ y) = y


instance Pretty xt => Pretty (Term y xt) where
  pretty (AtomicTerm at _) = pretty at
  pretty (VarTerm x _) = pretty x
  pretty (ProdTerm t1 t2 _) = "(" <> pretty t1 <> ", " <> pretty t2 <> ")"
  pretty (Proj1Term t _) = "(π1" <+> pretty t <> ")"
  pretty (Proj2Term t _) = "(π2" <+> pretty t <> ")"
  pretty (Inj1Term t _) = "(μ1 " <> pretty t <> ")"
  pretty (Inj2Term t _) = "(μ2 " <> pretty t <> ")"

derive instance Generic (Term y xt) _
instance (Show y, Show xt) => Show (Term y xt) where show x = genericShow x
-- over type annotations
derive instance Bifunctor Term
derive instance Bifoldable Term
derive instance Bitraversable Term

instance Functor (Term y) where map f x = rmap f x
instance Foldable (Term y) where
  foldr f b x = bifoldr (flip const) f b x
  foldl f b x = bifoldl const f b x
  foldMap f x = bifoldMap mempty f x
instance Traversable (Term y) where
  traverse f x = rtraverse f x
  sequence x = traverse identity x

data AtomicTerm
  = UnitTerm
  | NatTerm Int

instance Pretty AtomicTerm where
  pretty UnitTerm = "unit"
  pretty (NatTerm x) = show x

derive instance Generic AtomicTerm _
instance Show AtomicTerm where show x = genericShow x
instance Eq AtomicTerm where eq x y = genericEq x y

unitTerm = AtomicTerm UnitTerm
natTerm n = AtomicTerm (NatTerm n)

prodsTerm :: forall y xt. Partial => (y -> y -> y) -> Array (Term y xt) -> Term y xt
prodsTerm f = ArrayNE.fromArray >>> fromJust >>> ArrayNE.foldr1 \t1 t2 ->
  ProdTerm t1 t2 (f (termAnn t1) (termAnn t2))

-- | Aliases

-- typed
type TModule = Module CType
type TStmt = Stmt CType
type TRule = Rule CType
type TProp = Prop CType
type TTerm = Term CType

-- latticed (=> typed)
type LModule = Module CLat
type LStmt = Stmt CLat
type LType = Type CLat
type LProp = Prop CLat
type LTerm = Term CLat
type LRule = Rule CLat

-- concrete (=> latticed)
type CRule = LRule CX
type CParam = Param CX
type CProp = LProp CX
type CLat = Lat CX
type CType = Type CX
type CTerm = LTerm CX
