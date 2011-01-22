{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Data.Functor.Contravariant.Adjunction 
  ( Adjunction(..)
  , Representation(..)
  ) where

import Control.Monad.Instances ()
import Data.Functor.Contravariant

class (Contravariant f, Contravariant g) => Adjunction f g | f -> g, g -> f where
  unit :: a -> g (f a)
  counit :: f (g a) -> a
  leftAdjunct :: (a -> f b) -> b -> g a
  rightAdjunct :: (g a -> b) -> f b -> a
  
  unit = leftAdjunct id
  counit = rightAdjunct id
  leftAdjunct f = contramap f . unit
  rightAdjunct f = counit . contramap f

{-
instance Adjunction (Op r) (Op r) where
  unit a = Op (\k -> getOp k a)
  counit ... = not defined intuitionistically
-}

data Representation f x = Representation
  { rep :: forall a. (a -> x) -> f a
  , unrep :: forall a. f a -> a -> x
  }
 
{-
repAdjunction :: Adjunction f g => Representation g (f ())
repAdjunction = Representation { rep = flip leftAdjunct (), ... }
-}
