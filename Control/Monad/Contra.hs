{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Contra
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
-- Use a contravariant adjunction to Hask^op to build a 'Comonad' to 
-- 'Monad' transformer.
----------------------------------------------------------------------------

module Control.Monad.Contra
  ( Contra
  , runContra
  , contra
  , ContraT(..)
  ) where

import Prelude hiding (sequence)
import Control.Applicative
import Control.Comonad
import Control.Monad (ap)
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Adjunction

type Contra f g = ContraT f g Identity

newtype ContraT f g m a = ContraT { runContraT :: g (m (f a)) }

contra :: Contravariant g => g (f a) -> Contra f g a
contra = ContraT . contramap runIdentity

runContra :: Contravariant g => Contra f g a -> g (f a)
runContra = contramap Identity . runContraT

instance (Adjunction f g, Functor w) => Functor (ContraT f g w) where
  fmap f (ContraT g) = ContraT $ contramap (fmap (contramap f)) g
  
instance (Adjunction f g, Comonad w) => Applicative (ContraT f g w) where
  pure = ContraT . leftAdjunct extract
  (<*>) = ap

instance (Adjunction f g, Comonad w) => Monad (ContraT f g w) where
  return = ContraT . leftAdjunct extract
  ContraT m >>= f = ContraT $ contramap (extend (rightAdjunct (runContraT . f))) m
    
-- | Exploiting this instance requires that we have the missing Traversables for Identity, (,)e and IdentityT
-- instance (Adjunction f g, Traversable f) => MonadTrans (ContraT f g) where
--  lift = ContraT . fmap sequence . unit
