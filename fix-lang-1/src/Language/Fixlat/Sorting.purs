module Language.Fixlat.Sorting where

import Container
import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.Monad.Reader
import Data.Array
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Language.Fixlat.Grammar
import Prelude
import Pretty
import Control.Plus
import Data.Map as Map
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
    , sortings :: Map.Map Name Sort -- locally-quantified names
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
  statements' <- for mdl.statements typeStatement
  pure <<< Module
    $ { label: mdl.label
      , statements: statements'
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
-- synthTerm (PropTerm prop) = do
--   Predicate pred <- lookupPredicate prop.name
--   args' <- ((pred.params <#> _.sort) `zip` prop.args) `for` uncurry synthCheckTerm
--   pure $ PropTerm prop { args = args', ann = PredSort pred.name }
synthTerm (VarTerm v) =
  asks (_.sortings >>> Map.lookup v.name)
    >>= case _ of
        Nothing -> throwError <<< SortingError $ "unknown term variable:" ~ v.name
        Just sr -> pure $ VarTerm v { ann = sr }

synthTerm (UnitTerm u) = pure $ UnitTerm u { ann = UnitSort }

synthTerm (BoolTerm b) = pure $ BoolTerm b { ann = BoolSort }

synthTerm (TupleTerm t) = do
  cs <- for t.components synthTerm
  pure $ TupleTerm t { components = cs, ann = TupleSort { mb_ord: Nothing, components: cs <#> open } }

synthCheckTerm :: Sort -> Term Unit -> SortingM (Term Sort)
synthCheckTerm sr tm = do
  tm' <- synthTerm tm
  runMaybeT (unifySorts sr (open tm'))
    >>= case _ of
        Nothing -> throwError <<< SortingError $ "synthesized term" ~ ticks tm' \~ "to have sort" ~ ticks (open tm') \~ "but expected it to have sort" ~ ticks sr
        Just sr' -> pure $ mapContainer (const sr') tm'

unifySorts :: Sort -> Sort -> MaybeT SortingM Sort
unifySorts UnitSort UnitSort = pure UnitSort

unifySorts BoolSort BoolSort = pure BoolSort

unifySorts (TupleSort t1@{ mb_ord: Nothing }) (TupleSort t2@{ mb_ord: Just ord }) = unifySorts (TupleSort t1 { mb_ord = Just ord }) (TupleSort t2)

unifySorts (TupleSort t1@{ mb_ord: Just ord }) (TupleSort t2@{ mb_ord: Nothing }) = unifySorts (TupleSort t1) (TupleSort t2 { mb_ord = Just ord })

unifySorts (TupleSort t1) (TupleSort t2)
  | t1.mb_ord == t2.mb_ord = do
    cs <- uncurry unifySorts `traverse` (t1.components `zip` t2.components)
    pure $ TupleSort t1 { components = cs }

unifySorts _ _ = empty
