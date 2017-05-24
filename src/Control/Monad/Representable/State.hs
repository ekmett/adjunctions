{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
----------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Representable.State
-- Copyright   :  (c) Edward Kmett & Sjoerd Visscher 2011
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
--
-- A generalized State monad, parameterized by a Representable functor.
-- The representation of that functor serves as the state.
----------------------------------------------------------------------
module Control.Monad.Representable.State
   ( State
   , runState
   , evalState
   , execState
   , mapState
   , StateT(..)
   , stateT
   , runStateT
   , evalStateT
   , execStateT
   , mapStateT
   , liftCallCC
   , liftCallCC'
   , MonadState(..)
   ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Data.Functor.Bind
import Data.Functor.Bind.Trans
import Control.Monad.State.Class
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.Free.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Functor.Rep

-- ---------------------------------------------------------------------------
-- | A memoized state monad parameterized by a representable functor @g@, where
-- the representatation of @g@, @Rep g@ is the state to carry.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
type State g = StateT g Identity


-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runState :: Representable g
         => State g a   -- ^ state-passing computation to execute
         -> Rep g       -- ^ initial state
         -> (a, Rep g)  -- ^ return value and final state
runState m = runIdentity . runStateT m

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalState' m s = 'fst' ('runState' m s)@
evalState :: Representable g
          => State g a  -- ^state-passing computation to execute
          -> Rep g      -- ^initial value
          -> a          -- ^return value of the state computation
evalState m s = fst (runState m s)

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execState' m s = 'snd' ('runState' m s)@
execState :: Representable g
          => State g a  -- ^state-passing computation to execute
          -> Rep g      -- ^initial value
          -> Rep g      -- ^final state
execState m s = snd (runState m s)

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runState' ('mapState' f m) = f . 'runState' m@
mapState :: Functor g => ((a, Rep g) -> (b, Rep g)) -> State g a -> State g b
mapState f = mapStateT (Identity . f . runIdentity)

-- ---------------------------------------------------------------------------
-- | A state transformer monad parameterized by:
--
--   * @g@ - A representable functor used to memoize results for a state @Rep g@
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT g m a = StateT { getStateT :: g (m (a, Rep g)) }

stateT :: Representable g => (Rep g -> m (a, Rep g)) -> StateT g m a
stateT = StateT . tabulate

runStateT :: Representable g => StateT g m a -> Rep g -> m (a, Rep g)
runStateT (StateT m) = index m

mapStateT :: Functor g => (m (a, Rep g) -> n (b, Rep g)) -> StateT g m a -> StateT g n b
mapStateT f (StateT m) = StateT (fmap f m)

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@
evalStateT :: (Representable g, Monad m) => StateT g m a -> Rep g -> m a
evalStateT m s = do
    (a, _) <- runStateT m s
    return a

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: (Representable g, Monad m) => StateT g m a -> Rep g -> m (Rep g)
execStateT m s = do
    (_, s') <- runStateT m s
    return s'

instance (Functor g, Functor m) => Functor (StateT g m) where
  fmap f = StateT . fmap (fmap (\ ~(a, s) -> (f a, s))) . getStateT

instance (Representable g, Bind m) => Apply (StateT g m) where
  mf <.> ma = mf >>- \f -> fmap f ma

instance (Representable g, Functor m, Monad m) => Applicative (StateT g m) where
  pure = StateT . leftAdjunctRep return
  mf <*> ma = mf >>= \f -> fmap f ma

instance (Representable g, Bind m) => Bind (StateT g m) where
  StateT m >>- f = StateT $ fmap (>>- rightAdjunctRep (runStateT . f)) m

instance (Representable g, Monad m) => Monad (StateT g m) where
#if __GLASGOW_HASKELL__ < 710
  return = StateT . leftAdjunctRep return
#endif
  StateT m >>= f = StateT $ fmap (>>= rightAdjunctRep (runStateT . f)) m

instance Representable f => BindTrans (StateT f) where
  liftB m = stateT $ \s -> fmap (\a -> (a, s)) m

instance Representable f => MonadTrans (StateT f) where
  lift m = stateT $ \s -> liftM (\a -> (a, s)) m

instance (Representable g, Monad m, Rep g ~ s) => MonadState s (StateT g m) where
  get = stateT $ \s -> return (s, s)
  put s = StateT $ pureRep $ return ((),s)
#if MIN_VERSION_transformers(0,3,0)
  state f = stateT (return . f)
#endif

instance (Representable g, MonadReader e m) => MonadReader e (StateT g m) where
  ask = lift ask
  local = mapStateT . local

instance (Representable g, MonadWriter w m) => MonadWriter w (StateT g m) where
  tell = lift . tell
  listen = mapStateT $ \ma -> do
     ((a,s'), w) <- listen ma
     return ((a,w), s')
  pass = mapStateT $ \ma -> pass $ do
    ((a, f), s') <- ma
    return ((a, s'), f)

instance (Representable g, MonadCont m) => MonadCont (StateT g m) where
    callCC = liftCallCC' callCC

instance (Functor f, Representable g, MonadFree f m) => MonadFree f (StateT g m) where
    wrap as = stateT $ \s -> wrap (fmap (`runStateT` s) as)

leftAdjunctRep :: Representable u => ((a, Rep u) -> b) -> a -> u b
leftAdjunctRep f a = tabulate (\s -> f (a,s))

rightAdjunctRep :: Representable u => (a -> u b) -> (a, Rep u) -> b
rightAdjunctRep f ~(a, k) = f a `index` k

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original state on entering the
-- continuation.
liftCallCC :: Representable g => ((((a,Rep g) -> m (b,Rep g)) -> m (a,Rep g)) -> m (a,Rep g)) ->
    ((a -> StateT g m b) -> StateT g m a) -> StateT g m a
liftCallCC callCC' f = stateT $ \s ->
    callCC' $ \c ->
    runStateT (f (\a -> StateT $ pureRep $ c (a, s))) s

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current state on entering the continuation.
-- It does not satisfy the laws of a monad transformer.
liftCallCC' :: Representable g => ((((a,Rep g) -> m (b,Rep g)) -> m (a,Rep g)) -> m (a,Rep g)) ->
    ((a -> StateT g m b) -> StateT g m a) -> StateT g m a
liftCallCC' callCC' f = stateT $ \s ->
    callCC' $ \c ->
    runStateT (f (\a -> stateT $ \s' -> c (a, s'))) s

