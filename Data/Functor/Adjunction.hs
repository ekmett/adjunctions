{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

-------------------------------------------------------------------------------------------
-- |
-- Module	: Data.Functor.Adjunction
-- Copyright 	: 2008-2011 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: rank 2 types, MPTCs, fundeps
--
-------------------------------------------------------------------------------------------
module Data.Functor.Adjunction 
  ( Adjunction(..)
  , tabulateAdjunction
  , indexAdjunction
  ) where

import Control.Monad.Instances ()
import Control.Monad.Trans.Identity

import Control.Monad.Representable
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Traced

import Data.Functor.Identity
import Data.Functor.Compose

-- | An adjunction between Hask and Hask.
--
-- > rightAdjunct unit = id
-- > leftAdjunct counit = id 
class (Functor f, Representable g) => Adjunction f g | f -> g, g -> f where
  unit :: a -> g (f a)
  counit :: f (g a) -> a
  leftAdjunct :: (f a -> b) -> a -> g b
  rightAdjunct :: (a -> g b) -> f a -> b
  
  unit = leftAdjunct id
  counit = rightAdjunct id
  leftAdjunct f = fmap f . unit
  rightAdjunct f = counit . fmap f

tabulateAdjunction :: Adjunction f g => (f () -> b) -> g b
tabulateAdjunction f = leftAdjunct f ()

indexAdjunction :: Adjunction f g => g b -> f a -> b
indexAdjunction = rightAdjunct . const

instance Adjunction ((,)e) ((->)e) where
  leftAdjunct f a e = f (e, a)
  rightAdjunct f ~(e, a) = f a e

instance Adjunction Identity Identity where
  leftAdjunct f = Identity . f . Identity
  rightAdjunct f = runIdentity . f . runIdentity

instance Adjunction f g => Adjunction (IdentityT f) (IdentityT g) where
  unit = IdentityT . leftAdjunct IdentityT
  counit = rightAdjunct runIdentityT . runIdentityT

instance Adjunction w m => Adjunction (EnvT e w) (ReaderT e m) where
  unit = ReaderT . flip fmap EnvT . flip leftAdjunct
  counit (EnvT e w) = counit $ fmap (flip runReaderT e) w

instance Adjunction m w => Adjunction (WriterT s m) (TracedT s w) where
  unit = TracedT . leftAdjunct (\ma s -> WriterT (fmap (\a -> (a, s)) ma)) 
  -- counit (WriterT mwas) = 

instance (Adjunction f g, Adjunction f' g') => Adjunction (Compose f' f) (Compose g g') where
  unit = Compose . leftAdjunct (leftAdjunct Compose) 
  counit = rightAdjunct (rightAdjunct getCompose) . getCompose
