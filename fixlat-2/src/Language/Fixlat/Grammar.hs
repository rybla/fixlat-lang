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
    Name (..),
    Label (..),
    Proof (..),
  )
where

-- | Module
data Module ty = Module
  { moduleLabel :: Label,
    moduleStatements :: Statement ty
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | Statements
data Statement ty
  = PredicateStatement (Predicate ty)
  | RuleStatement (Rule ty)
  | QueryStatement (Rule ty)
  deriving (Show, Functor, Foldable, Traversable)

-- | Predicate
data Predicate ty = Predicate
  { predicatelabel :: Label,
    predicateName :: Name,
    predicateParamType :: Type
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | Rule
data Rule ty = Rule
  { ruleLabel :: Label,
    ruleParams :: [Param],
    ruleHyps :: [Prop ty],
    ruleCon :: Prop ty
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | Data
data Data = Data
  { dataName :: Name,
    dataCtors :: [Ctor]
  }
  deriving (Show)

data Ctor = Ctor
  { ctorName :: Name,
    ctorParam :: Param
  }
  deriving (Show)

-- | Parameter
data Param = Param
  { paramName :: Name,
    paramType :: Type
  }
  deriving (Show)

-- | Prop
data Prop ty
  = PredicateProp Name (Term ty)
  deriving (Show, Functor, Foldable, Traversable)

-- | Type
data Type
  = DataType Name
  | TupleType [Type]
  deriving (Show)

-- | Term
data Term ty
  = VarTerm Name ty
  | DataTerm Name [Term ty] ty -- datatype constructor applied to arguments
  | TupleTerm [Term ty]
  deriving (Show, Functor, Foldable, Traversable)

-- | Extract the type of a typed term.
extractType :: Term Type -> Type
extractType (VarTerm _x ty) = ty
extractType (DataTerm _x _args ty) = ty
extractType (TupleTerm tms) = TupleType (extractType <$> tms)

-- | A proof is encoded as a tree, where the rule is the `Rule Type` used in
-- this step of the proof and the `[Proof]` are the proofs of the hypotheses of
-- that rule.
data Proof = Proof (Rule Type) [Proof]

-- | Label
newtype Label = Label String
  deriving (Show, Ord, Eq)

-- | Name
newtype Name = Name String
  deriving (Show, Ord, Eq)
