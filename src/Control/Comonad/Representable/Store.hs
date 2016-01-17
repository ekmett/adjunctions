{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
----------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Representable.Store
-- Copyright   :  (c) Edward Kmett & Sjoerd Visscher 2011
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
--
-- This is a generalized 'Store' 'Comonad', parameterized by a 'Representable' 'Functor'.
-- The representation of that 'Functor' serves as the index of the store.
--
-- This can be useful if the representable functor serves to memoize its
-- contents and will be inspected often.
----------------------------------------------------------------------
module Control.Comonad.Representable.Store
   ( Store
   , store
   , runStore
   , StoreT(..)
   , storeT
   , runStoreT
   , ComonadStore(..)
   ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.Comonad.Cofree.Class
import Control.Comonad.Env.Class
import Control.Comonad.Hoist.Class
import Control.Comonad.Store.Class
import Control.Comonad.Traced.Class
import Control.Comonad.Trans.Class
import Control.Monad.Identity
import Data.Functor.Apply
import Data.Functor.Extend
import Data.Functor.Rep
import Data.Semigroup

-- | A memoized store comonad parameterized by a representable functor @g@, where
-- the representatation of @g@, @Rep g@ is the index of the store.
--
type Store g = StoreT g Identity

-- | Construct a store comonad computation from a function and a current index.
-- (The inverse of 'runStore'.)
store :: Representable g
      => (Rep g -> a)  -- ^ computation
      -> Rep g         -- ^ index
      -> Store g a
store = storeT . Identity

-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runStore :: Representable g
         => Store g a           -- ^ a store to access
         -> (Rep g -> a, Rep g) -- ^ initial state
runStore (StoreT (Identity ga) k) = (index ga, k)

-- ---------------------------------------------------------------------------
-- | A store transformer comonad parameterized by:
--
--   * @g@ - A representable functor used to memoize results for an index @Rep g@
--
--   * @w@ - The inner comonad.
data StoreT g w a = StoreT (w (g a)) (Rep g)

storeT :: (Functor w, Representable g) => w (Rep g -> a) -> Rep g -> StoreT g w a
storeT = StoreT . fmap tabulate

runStoreT :: (Functor w, Representable g) => StoreT g w a -> (w (Rep g -> a), Rep g)
runStoreT (StoreT w s) = (index <$> w, s)

instance (Comonad w, Representable g, Rep g ~ s) => ComonadStore s (StoreT g w) where
  pos (StoreT _ s) = s
  peek s (StoreT w _) = extract w `index` s
  peeks f (StoreT w s) = extract w `index` f s
  seek s (StoreT w _) = StoreT w s
  seeks f (StoreT w s) = StoreT w (f s)

instance (Functor w, Functor g) => Functor (StoreT g w) where
  fmap f (StoreT w s) = StoreT (fmap (fmap f) w) s

instance (Apply w, Semigroup (Rep g), Representable g) => Apply (StoreT g w) where
  StoreT ff m <.> StoreT fa n = StoreT (apRep <$> ff <.> fa) (m <> n)

instance (ComonadApply w, Semigroup (Rep g), Representable g) => ComonadApply (StoreT g w) where
  StoreT ff m <@> StoreT fa n = StoreT (apRep <$> ff <@> fa) (m <> n)

instance (Applicative w, Monoid (Rep g), Representable g) => Applicative (StoreT g w) where
  pure a = StoreT (pure (pureRep a)) mempty
  StoreT ff m <*> StoreT fa n = StoreT (apRep <$> ff <*> fa) (m `mappend` n)

instance (Extend w, Representable g) => Extend (StoreT g w) where
  duplicated (StoreT wf s) = StoreT (extended (tabulate . StoreT) wf) s

instance (Comonad w, Representable g) => Comonad (StoreT g w) where
  duplicate (StoreT wf s) = StoreT (extend (tabulate . StoreT) wf) s
  extract (StoreT wf s) = index (extract wf) s

instance Representable g => ComonadTrans (StoreT g) where
  lower (StoreT w s) = fmap (`index` s) w

instance ComonadHoist (StoreT g) where
  cohoist f (StoreT w s) = StoreT (f w) s

instance (ComonadTraced m w, Representable g) => ComonadTraced m (StoreT g w) where
  trace m = trace m . lower

instance (ComonadEnv m w, Representable g) => ComonadEnv m (StoreT g w) where
  ask = ask . lower

instance (Representable g, ComonadCofree f w) => ComonadCofree f (StoreT g w) where
  unwrap (StoreT w s) = fmap (`StoreT` s) (unwrap w)
