{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Data.Functor.Adjunction 
  ( Adjunction(..)
  ) where

import Control.Monad.Instances ()
import Control.Monad.Trans.Identity
import Data.Functor.Identity

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
  unit = IdentityT . fmap IdentityT . unit
  counit = counit . fmap runIdentityT . runIdentityT
