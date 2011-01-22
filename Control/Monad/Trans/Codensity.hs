{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Codensity
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Monad.Trans.Codensity
  ( Codensity(..)
  , lowerCodensity
  , codensityToAdjunction
  , adjunctionToCodensity
  ) where

import Control.Applicative
import Control.Monad (ap)
import Data.Functor.Adjunction
import Data.Functor.Apply
import Control.Monad.Trans.Class

newtype Codensity m a = Codensity { runCodensity :: forall b. (a -> m b) -> m b }

instance Functor (Codensity k) where
  fmap f m = Codensity (\k -> runCodensity m (k . f))

instance FunctorApply (Codensity f) where
  (<.>) = ap

instance Applicative (Codensity f) where
  pure x = Codensity (\k -> k x)
  (<*>) = ap

instance Monad (Codensity f) where
  return x = Codensity (\k -> k x)
  m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))

{-
instance MonadIO m => MonadIO (Codensity m) where
  liftIO = liftCodensity . liftIO 
-}

instance MonadTrans Codensity where
  lift m = Codensity (m >>=)

lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity a = runCodensity a return

codensityToAdjunction :: Adjunction f g => Codensity g a -> g (f a)
codensityToAdjunction r = runCodensity r unit

adjunctionToCodensity :: Adjunction f g => g (f a) -> Codensity g a
adjunctionToCodensity f = Codensity (\a -> fmap (rightAdjunct a) f)
