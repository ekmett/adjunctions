{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs
--
-- Use a contravariant dual adjunction from Hask^op to build a 'Monad' to
-- 'Comonad' transformer.
----------------------------------------------------------------------------

module Control.Comonad.Contra.Adjoint
  ( Adjoint
  , runAdjoint
  , adjoint
  , AdjointT(..)
  ) where

import Control.Comonad
import Control.Monad (liftM)
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor.Contravariant.DualAdjunction

type Adjoint f g = AdjointT f g Identity

newtype AdjointT f g m a = AdjointT { runAdjointT :: f (m (g a)) }

adjoint :: Contravariant f => f (g a) -> Adjoint f g a
adjoint = AdjointT . contramap runIdentity

runAdjoint :: Contravariant f => Adjoint f g a -> f (g a)
runAdjoint = contramap Identity . runAdjointT

instance (Contravariant f, Contravariant g, Monad m) => Functor (AdjointT f g m) where
  fmap f (AdjointT g) = AdjointT $ contramap (liftM (contramap f)) g

instance (DualAdjunction f g, Monad m) => Comonad (AdjointT f g m) where
  extract = rightAdjunct return . runAdjointT
  extend f = AdjointT . contramap (>>= leftAdjunct (f . AdjointT)) . runAdjointT
