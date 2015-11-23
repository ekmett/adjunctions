{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Contravariant.Adjoint
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
-- Uses a contravariant adjunction:
--
-- f -| g : Hask^op -> Hask
--
-- to build a 'Comonad' to 'Monad' transformer. Sadly, the dual construction,
-- which builds a 'Comonad' out of a 'Monad', is uninhabited, because any
-- 'Adjunction' of the form
--
-- > f -| g : Hask -> Hask^op
--
-- would trivially admit unsafePerformIO.
--
----------------------------------------------------------------------------

module Control.Monad.Trans.Contravariant.Adjoint
  ( Adjoint
  , runAdjoint
  , adjoint
  , AdjointT(..)
  ) where

import Prelude hiding (sequence)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.Monad (ap)
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Adjunction

type Adjoint f g = AdjointT f g Identity

newtype AdjointT f g w a = AdjointT { runAdjointT :: g (w (f a)) }

adjoint :: Contravariant g => g (f a) -> Adjoint f g a
adjoint = AdjointT . contramap runIdentity

runAdjoint :: Contravariant g => Adjoint f g a -> g (f a)
runAdjoint = contramap Identity . runAdjointT

instance (Adjunction f g, Functor w) => Functor (AdjointT f g w) where
  fmap f (AdjointT g) = AdjointT $ contramap (fmap (contramap f)) g

instance (Adjunction f g, Comonad w) => Applicative (AdjointT f g w) where
  pure = AdjointT . leftAdjunct extract
  (<*>) = ap

instance (Adjunction f g, Comonad w) => Monad (AdjointT f g w) where
  return = pure
  AdjointT m >>= f = AdjointT $ contramap (extend (rightAdjunct (runAdjointT . f))) m
