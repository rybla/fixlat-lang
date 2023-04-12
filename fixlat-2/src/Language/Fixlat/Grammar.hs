{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Fixlat.Grammar
  ( Module (..),
    Statement (..),
    Rule (..),
    Predicate (..),
    Prop (..),
    Lat(..),
    Type (..),
    Term (..),
    extractType,
    Label (..),
    Proof (..),
    PredName (..),
    TermName (..),
    FieldName (..),
    CtrName (..),
    TypeName (..),
  )
where

import Prettyprinter

-- | Module
data Module l y = Module
  { moduleLabel :: Label,
    moduleStatements :: Statement l y
  }
  deriving (Show)

-- | Statements
data Statement l y
  = PredicateStatement (Predicate l)
  | RuleStatement (Rule l y)
  | QueryStatement (Rule l y)
  deriving (Show)

-- | Predicate
data Predicate l = Predicate
  { predicatelabel :: Label,
    predicateName :: PredName,
    predicateParamType :: Type l
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | Rule
data Rule l y = Rule
  { ruleLabel :: Label,
    ruleParams :: [Param l],
    ruleHyps :: [Prop y],
    ruleCon :: Prop y
  }
  deriving (Show)

-- | Data
data Data l = Data
  { dataName :: TypeName,
    dataType :: Type l
  }
  deriving (Show)

-- | Parameter
data Param l = Param
  { paramName :: TermName,
    paramType :: Type l
  }
  deriving (Show)

-- | Prop
data Prop y
  = PredProp PredName (Term y) -- predicate applied to argument
  deriving (Show, Functor, Foldable, Traversable)

-- | Lat
data Lat
  = AtomicLat AtomicType
  | SumLat CtrLat (Maybe Lat)
  | ProdLat FieldLat (Maybe Lat)
  | NegLat Lat
  | DiscreteLat
  | FixLat TypeName Lat
  | NamedLat TypeName
  deriving (Show)

data CtrLat = MakeCtrLat CtrName Lat deriving (Show)

data FieldLat = MakeFieldLat FieldName Lat deriving (Show)

instance Pretty Lat where
  pretty (AtomicLat at) = pretty at
  pretty (SumLat ctr l) = parens (pretty ctr <+> "+" <+> pretty l)
  pretty (ProdLat fld l) = parens (pretty fld <+> "*" <+> pretty l)
  pretty (NegLat l) = parens ("-" <+> pretty l)
  pretty DiscreteLat = "@"
  pretty (FixLat x l) = parens ("fix" <+> pretty x <+> "in" <+> pretty l)
  pretty (NamedLat x) = pretty x

instance Pretty CtrLat where
  pretty (MakeCtrLat x l) = pretty x <> brackets (pretty l)

instance Pretty FieldLat where
  pretty (MakeFieldLat x l) = braces (pretty x <+> ":" <+> pretty l)

-- | Type
data Type l
  = AtomicType AtomicType l
  | SumType CtrName (Type l) (Maybe (Type l))
  | ProdType FieldName (Type l) (Maybe (Type l))
  | FixType TypeName (Type l)
  | NamedType TypeName
  | AscribeType (Type l) Lat
  deriving (Show, Functor, Foldable, Traversable)

data AtomicType
  = NatAtomicType
  -- !TODO other atomic types
  deriving (Show)

instance Pretty (Type l) where
  pretty (AtomicType at _) = pretty at
  pretty (SumType cn ct y) = parens ((pretty cn <> brackets (pretty ct)) <+> "+" <+> pretty y)
  pretty (ProdType fn ft y) = parens ((pretty fn <+> ":" <+> pretty ft) <+> "*" <+> pretty y)
  pretty (FixType x y) = parens ("fix" <+> pretty x <+> "in" <+> pretty y)
  pretty (NamedType x) = pretty x
  pretty (AscribeType y l) = pretty y <+> ":" <+> pretty l

instance Pretty AtomicType where
  pretty NatAtomicType = "Nat"

-- | Term
data Term y
  = AtomicTerm AtomicTerm y
  | NamedTerm TermName y
  | ProdTerm FieldName (Term y) (Maybe (Term y)) y -- product
  | ProjTerm (Term y) FieldName y -- project product
  | CtrTerm CtrName (Term y) y -- constructor
  | ElimTerm (Term y) CtrName TermName (Term y) (Term y) y -- eliminate sum
  deriving (Show, Functor, Foldable, Traversable)

instance Pretty (Term y) where
  pretty (AtomicTerm a _) = pretty a
  pretty (NamedTerm x _) = pretty x
  pretty (ProdTerm fn ft mb_t _) = braces $ pretty fn <+> "=" <+> pretty ft <+> "," <+> maybe mempty pretty mb_t
  pretty (ProjTerm t fn _) = pretty t <> dot <> pretty fn
  pretty (CtrTerm cn t _) = pretty cn <> brackets (pretty t)
  pretty (ElimTerm t1 cn tn t2 t3 _) = parens $ "case" <+> pretty t1 <+> "of" <+> parens ((pretty cn <> brackets (pretty tn)) <+> "=>" <+> pretty t2) <+> "else" <+> pretty t3

data AtomicTerm
  = NatAtomicTerm Int
  deriving (Show)

-- !TODO other atomic terms

instance Pretty AtomicTerm where
  pretty (NatAtomicTerm i) = pretty i

-- | Extract the type of a typed term.
-- !TODO probably want to define a generic display for Type that doesn't require Show l
extractType :: Term (Type l) -> Type l
extractType = error "extractType"

-- extractType (AtomicTerm at) = extractAtomicType at
-- extractType (NamedTerm _x y) = y
-- extractType (ProdTerm fn _ _ mb_t2) = ProdType (MakeFieldType x (extractType t1)) (extractType <$> mb_t2)
-- extractType _ = _

-- extractType (CtrTerm _x _t y) = y

-- extractType t0@(FieldTerm x t) = go (extractType t)
--   where
--     go (ProdType (MakeFieldType x' y) _) | x == x' = y
--     go (ProdType _ y) = go y
--     go _ = error . show $ "extractType: maltyped term `" <> pretty t0 <> "`"

-- | Label
newtype Label = Label String deriving (Show, Ord, Eq)

-- | Names

-- | TermName
newtype TermName = TermName String deriving (Show, Ord, Eq)

instance Pretty TermName where
  pretty (TermName str) = pretty str

-- | PredName
newtype PredName = PredName String deriving (Show, Ord, Eq)

instance Pretty PredName where
  pretty (PredName str) = pretty str

-- | CtrName
newtype CtrName = CtrName String deriving (Show, Ord, Eq)

instance Pretty CtrName where
  pretty (CtrName str) = pretty str

-- | FieldName
newtype FieldName = FieldName String deriving (Show, Ord, Eq)

instance Pretty FieldName where
  pretty (FieldName str) = pretty str

-- | TypeName
newtype TypeName = TypeName String deriving (Show, Ord, Eq)

instance Pretty TypeName where pretty (TypeName str) = pretty str

-- | A proof is encoded as a tree, where the rule is the `Rule Type` used in
-- this step of the proof and the `[Proof]` are the proofs of the hypotheses of
-- that rule.
data Proof = Proof (Rule Lat (Type Lat)) [Proof]
