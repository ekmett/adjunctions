{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2013 Edward Kmett
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

import Prelude hiding (sequence)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad (ap, liftM)
import Control.Monad.Trans.Class
import Data.Traversable
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
  b <$ AdjointT g = AdjointT $ fmap (liftM (b <$)) g

instance (Adjunction f g, Monad m) => Applicative (AdjointT f g m) where
  pure = AdjointT . leftAdjunct return
  (<*>) = ap

instance (Adjunction f g, Monad m) => Monad (AdjointT f g m) where
  return = pure
  AdjointT m >>= f = AdjointT $ fmap (>>= rightAdjunct (runAdjointT . f)) m

-- | Exploiting this instance requires that we have the missing Traversables for Identity, (,)e and IdentityT
instance (Adjunction f g, Traversable f) => MonadTrans (AdjointT f g) where
  lift = AdjointT . fmap sequence . unit
