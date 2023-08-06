module Control.Monad.Computation where

import Prelude

import Data.Tuple.Nested
import Control.Monad.Except (class MonadTrans, ExceptT, runExceptT)
import Control.Monad.Except as Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader as Reader
import Control.Monad.State (StateT, evalStateT, runStateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Hole (hole)

newtype ComputationT (name :: Symbol) ctx env err m a = 
  ComputationT (ExceptT err (ReaderT ctx (StateT env m)) a)

derive instance Newtype (ComputationT name ctx env err m a) _

derive newtype instance Monad m => Functor (ComputationT name ctx env err m)
derive newtype instance Monad m => Bind (ComputationT name ctx env err m)
derive newtype instance Monad m => Apply (ComputationT name ctx env err m)
derive newtype instance Monad m => Applicative (ComputationT name ctx env err m)
instance MonadTrans (ComputationT name ctx env err) where lift = liftComputationT

instance Monad m => Monad (ComputationT name ctx env err m)

runComputationT :: forall name ctx env err m a. Functor m => ctx -> env -> ComputationT name ctx env err m a -> m ((err \/ a) /\ env)
runComputationT ctx env (ComputationT m) = (runExceptT >>> flip runReaderT ctx >>> flip runStateT env) m

evalComputationT :: forall name ctx env err m a. Functor m => ctx -> env -> ComputationT name ctx env err m a -> m (err \/ a)
evalComputationT ctx env (ComputationT m) = (runExceptT >>> flip runReaderT ctx >>> flip evalStateT env) m

liftComputationT :: forall name ctx env err m a. Monad m => m a -> ComputationT name ctx env err m a
liftComputationT = ComputationT <<< lift <<< lift <<< lift

-- TODO: does this modify the state even on failure?
tryComputation :: forall name ctx env err m a. Monad m => 
  ComputationT name ctx env err m a -> 
  ComputationT name ctx env err m (err \/ a)
tryComputation m = ComputationT do
  ctx <- Reader.ask
  env <- State.get
  (lift <<< lift <<< lift) (runComputationT ctx env m) >>= case _ of
    Left err /\ _env' -> do
      pure (Left err)
    Right a /\ env' -> do
      -- update the state
      State.put env'
      pure (Right a)

-- ComputationT (lift (runExceptT m))

-- ReaderT
local f (ComputationT m) = ComputationT (Reader.local f m)
ask = ComputationT Reader.ask
asks f = ComputationT (Reader.asks f)

-- ExceptT
throwError err = ComputationT (Except.throwError err)

-- StateT
get = ComputationT State.get
gets f = ComputationT (State.gets f)
put env = ComputationT (State.put env)
modify f = ComputationT (State.modify f)
