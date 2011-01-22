{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Contra
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
-- Use a contravariant dual adjunction from Hask^op to build a 'Monad' to 
-- 'Comonad' transformer.
----------------------------------------------------------------------------

module Control.Comonad.Contra
  ( Contra
  , runContra
  , contra
  , ContraT(..)
  ) where

import Prelude hiding (sequence)
import Control.Applicative
import Control.Comonad
import Control.Monad (liftM, ap)
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor.Contravariant.DualAdjunction

type Contra f g = ContraT f g Identity

newtype ContraT f g m a = ContraT { runContraT :: f (m (g a)) }

contra :: Contravariant f => f (g a) -> Contra f g a
contra = ContraT . contramap runIdentity

runContra :: Contravariant f => Contra f g a -> f (g a)
runContra = contramap Identity . runContraT

instance (Contravariant f, Contravariant g, Monad m) => Functor (ContraT f g m) where
  fmap f (ContraT g) = ContraT $ contramap (liftM (contramap f)) g
  
instance (DualAdjunction f g, Monad m) => Comonad (ContraT f g m) where
  extract = rightAdjunctOp return . runContraT
  extend f = ContraT . contramap (>>= leftAdjunctOp (f . ContraT)) . runContraT

