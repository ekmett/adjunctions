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
  , Representation(..)
  , repAdjunction
  ) where

import Control.Monad.Instances ()
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.Functor.Compose

-- |
-- > rightAdjunct unit = id
-- > leftAdjunct counit = id 
class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where
  unit :: a -> g (f a)
  counit :: f (g a) -> a
  leftAdjunct :: (f a -> b) -> a -> g b
  rightAdjunct :: (a -> g b) -> f a -> b
  
  unit = leftAdjunct id
  counit = rightAdjunct id
  leftAdjunct f = fmap f . unit
  rightAdjunct f = counit . fmap f

instance Adjunction ((,)e) ((->)e) where
  leftAdjunct f a e = f (e, a)
  rightAdjunct f ~(e, a) = f a e

instance Adjunction Identity Identity where
  leftAdjunct f = Identity . f . Identity
  rightAdjunct f = runIdentity . f . runIdentity

instance Adjunction f g => Adjunction (IdentityT f) (IdentityT g) where
  unit = IdentityT . leftAdjunct IdentityT
  counit = rightAdjunct runIdentityT . runIdentityT

instance (Adjunction f1 g1, Adjunction f2 g2) => Adjunction (Compose f2 f1) (Compose g1 g2) where
  unit = Compose . leftAdjunct (leftAdjunct Compose) 
  counit = rightAdjunct (rightAdjunct getCompose) . getCompose

data Representation f x = Representation
  { rep :: forall a. (x -> a) -> f a
  , unrep :: forall a. f a -> x -> a
  }
 
repAdjunction :: Adjunction f g => Representation g (f ())
repAdjunction = Representation 
  { rep = flip leftAdjunct ()
  , unrep = rightAdjunct . const
  }
