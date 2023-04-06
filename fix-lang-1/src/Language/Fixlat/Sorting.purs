module Language.Fixlat.Sorting where

import Container
import Control.Monad.Except
import Control.Monad.Reader
import Data.Array
import Data.Traversable
import Data.Tuple
import Language.Fixlat.Grammar
import Prelude
import Pretty
import Data.Map as Map
import Data.Maybe
import Data.Newtype (wrap)
import Language.Fixlat.M as M
import Partial.Unsafe (unsafeCrashWith)

-- | The sorting monad.
type SortingM
  = ReaderT SortingCtx (ExceptT SortingError M.M)

-- | Sorting errors.
newtype SortingError
  = SortingError Grid

-- | The sorting context.
type SortingCtx
  = { predicates :: Map.Map Name Predicate -- pred name => pred
    }

initSortingCtx :: Module Unit -> SortingCtx
initSortingCtx _mdl = unsafeCrashWith "initSortingCtx"

lookupPredicate :: Name -> SortingM Predicate
lookupPredicate name =
  asks (_.predicates >>> Map.lookup name)
    >>= case _ of
        Nothing -> throwError <<< SortingError $ "unknown predicate:" ~ name
        Just pred -> pure pred

-- | Sort-annoate everything in a module.
typeModule :: Module Unit -> SortingM (Module Sort)
typeModule (Module mdl) = do
  stmts' <- for mdl.stmts typeStatement
  pure <<< Module
    $ { label: mdl.label
      , stmts: stmts'
      }

typeStatement :: Statement Unit -> SortingM (Statement Sort)
typeStatement (PredicateStatement pred) = pure $ PredicateStatement pred

typeStatement (RuleStatement rule) = RuleStatement <$> typeRule rule

typeStatement (QueryStatement query) = QueryStatement <$> typeQuery query

typeRule :: Rule Unit -> SortingM (Rule Sort)
typeRule (Rule rule) = do
  hyps' <- synthTerm `traverse` rule.hyps
  con' <- synthTerm rule.con
  pure <<< Rule
    $ { label: rule.label
      , params: rule.params
      , hyps: hyps'
      , con: con'
      }

typeQuery :: Query Unit -> SortingM (Query Sort)
typeQuery (Query query) = do
  hyps' <- synthTerm `traverse` query.hyps
  con' <- synthTerm query.con
  pure <<< Query
    $ { params: query.params
      , hyps: hyps'
      , con: con'
      }

synthTerm :: Term Unit -> SortingM (Term Sort)
synthTerm (PropTerm prop) = do
  Predicate pred <- lookupPredicate prop.name
  args' <- ((pred.params <#> _.sort) `zip` prop.args) `for` uncurry synthCheckTerm
  pure $ PropTerm prop { args = args', ann = PredSort pred.name }

synthTerm (PrimTerm (UnitPrimTerm it)) = pure $ PrimTerm $ UnitPrimTerm it { ann = PrimSort UnitPrimSort }

synthTerm (PrimTerm (BoolPrimTerm bool)) = pure $ PrimTerm $ BoolPrimTerm bool { ann = PrimSort BoolPrimSort }

synthCheckTerm :: Sort -> Term Unit -> SortingM (Term Sort)
synthCheckTerm sr tm = do
  tm' <- synthTerm tm
  if open tm' == sr then
    pure tm'
  else
    throwError <<< SortingError $ "synthesized term" ~ ticks tm' \~ "to have sort" ~ ticks (open tm') \~ "but expected it to have sort" ~ ticks sr
