{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Adjoint
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Control.Monad.Trans.Adjoint
  ( Adjoint
  , runAdjoint
  , adjoint
  , AdjointT(..)
  ) where

import Control.Applicative
import Control.Monad (ap, liftM)
import Control.Monad.Trans.Class
import Data.Functor.Adjunction
import Data.Functor.Identity

type Adjoint f g = AdjointT f g Identity

newtype AdjointT f g m a = AdjointT { runAdjointT :: g (m (f a)) }

adjoint :: Functor g => g (f a) -> Adjoint f g a
adjoint = AdjointT . fmap Identity

runAdjoint :: Functor g => Adjoint f g a -> g (f a)
runAdjoint = fmap runIdentity . runAdjointT

instance (Adjunction f g, Monad m) => Functor (AdjointT f g m) where
  fmap f (AdjointT g) = AdjointT $ fmap (liftM (fmap f)) g
  b <$ (AdjointT g) = AdjointT $ fmap (liftM (b <$)) g
  
instance (Adjunction f g, Monad m) => Applicative (AdjointT f g m) where
  pure = AdjointT . leftAdjunct return
  (<*>) = ap

instance (Adjunction f g, Monad m) => Monad (AdjointT f g m) where
  return = AdjointT . leftAdjunct return
  AdjointT m >>= f = AdjointT $ fmap (>>= rightAdjunct (runAdjointT . f)) m

instance Adjunction f g => MonadTrans (AdjointT f g)
