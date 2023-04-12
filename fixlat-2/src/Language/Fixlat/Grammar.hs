{-# LANGUAGE DeriveTraversable #-}

module Language.Fixlat.Grammar
  ( Module (..),
    Statement (..),
    Rule (..),
    Predicate (..),
    Prop (..),
    Type (..),
    Term (..),
    extractType,
    Label (..),
    Proof (..),
  )
where

-- | Module
data Module l y = Module
  { moduleLabel :: Label,
    moduleStatements :: Statement l y
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | Statements
data Statement l y
  = PredicateStatement (Predicate l)
  | RuleStatement (Rule l y)
  | QueryStatement (Rule l y)
  deriving (Show, Functor, Foldable, Traversable)

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
  deriving (Show, Functor, Foldable, Traversable)

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
data Prop ty
  = PredProp PredName (Term ty) -- predicate applied to argument
  deriving (Show, Functor, Foldable, Traversable)

-- | Lattice
data Lattice
  = AtomicLattice AtomicType
  | SumLattice CtorLattice Lattice
  | ProdLattice FieldLattice Lattice
  | NegLattice Lattice
  | DiscreteLattice
  | FixLattice TypeName Lattice
  | NamedLattice TypeName
  deriving (Show)

data CtorLattice = CtorLattice CtorName Lattice deriving (Show)

data FieldLattice = FieldLattice FieldName Lattice deriving (Show)

-- | Type
data Type l
  = AtomicType AtomicType l
  | SumType (CtorType l) (Type l)
  | ProdType (FieldType l) (Type l)
  | FixType TypeName (Type l)
  | NamedType TypeName
  | AscribeType (Type l) Lattice
  deriving (Show, Functor, Foldable, Traversable)

data AtomicType
  = NatAtomicType
  deriving (Show)

data CtorType l = CtorType CtorName (Type l) deriving (Show)

data FieldType l = FieldType FieldName (Type l) deriving (Show)

-- | Term
data Term y
  = AtomicTerm (AtomicTerm y)
  | NamedTerm TermName y
  | ProdTerm FieldName (Term y) (Term y)
  | FieldTerm FieldName (Term y)
  | CtorTerm CtorName (Term y) y
  deriving (Show, Functor, Foldable, Traversable)

data AtomicTerm y
  = NatAtomicTerm Int y
  deriving (Show, Functor, Foldable, Traversable)

-- | Extract the type of a typed term.
-- !TODO probably want to define a generic display for Typ that doesn't require Show l
extractType :: Show l => Term (Type l) -> Type l
extractType (AtomicTerm at) = extractAtomicType at
extractType (NamedTerm _x y) = y
extractType (ProdTerm x t1 t2) = ProdType (FieldType x (extractType t1)) (extractType t2)
extractType (CtorTerm _x _t y) = y
extractType t0@(FieldTerm x t) = go (extractType t)
  where
    go (ProdType (FieldType x' y) _) | x == x' = y
    go (ProdType _ y) = go y
    go _ = error $ "extractType: maltyped term `" ++ show t0 ++ "`"

extractAtomicType :: AtomicTerm (Type l) -> Type l
extractAtomicType (NatAtomicTerm _ y) = y

-- | A proof is encoded as a tree, where the rule is the `Rule Type` used in
-- this step of the proof and the `[Proof]` are the proofs of the hypotheses of
-- that rule.
data Proof = Proof (Rule Lattice (Type Lattice)) [Proof]

-- | Label
newtype Label = Label String deriving (Show, Ord, Eq)

-- | Names
newtype TermName = TermName String deriving (Show, Ord, Eq)

newtype PredName = PredName String deriving (Show, Ord, Eq)

newtype CtorName = CtorName String deriving (Show, Ord, Eq)

newtype FieldName = FieldName String deriving (Show, Ord, Eq)

newtype TypeName = TypeName String deriving (Show, Ord, Eq)
