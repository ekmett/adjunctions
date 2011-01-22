{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Data.Functor.Contravariant.Adjunction 
  ( Adjunction(..)
  , DualAdjunction(..)
  , Representation(..)
  , repAdjunction, repFlippedAdjunction
  ) where

import Control.Monad.Instances ()
import Data.Functor.Contravariant

-- | An adjunction from Hask^op to Hask
-- 
-- > Op (f a) b ~ Hask a (g b)
--
-- > rightAdjunct unit = id
-- > leftAdjunct counit = id
class (Contravariant f, Contravariant g) => Adjunction f g | f -> g, g -> f where
  unit :: a -> g (f a) -- monad in Hask
  counit :: a -> f (g a) -- comonad in Hask^op
  leftAdjunct  :: (b -> f a) -> a -> g b 
  rightAdjunct :: (a -> g b) -> b -> f a
  unit = leftAdjunct id 
  counit = rightAdjunct id
  leftAdjunct f = contramap f . unit 
  rightAdjunct f = contramap f . counit

-- | This adjunction gives rise to the Cont monad
instance Adjunction (Op r) (Op r) where
  unit a = Op (\k -> getOp k a)
  counit = unit

-- | This gives rise to the Cont Bool monad
instance Adjunction Predicate Predicate where
  unit a = Predicate (\k -> getPredicate k a)
  counit = unit

-- | A representation of a contravariant functor
data Representation f x = Representation
  { rep :: forall a. (a -> x) -> f a
  , unrep :: forall a. f a -> (a -> x)
  }
 
-- | Represent a contravariant functor that has a left adjoint
repAdjunction :: Adjunction f g => Representation g (f ())
repAdjunction = Representation 
  { rep = flip leftAdjunct () 
  , unrep = rightAdjunct . const
  }

repFlippedAdjunction :: Adjunction f g => Representation f (g ()) 
repFlippedAdjunction = Representation 
  { rep = flip rightAdjunct () 
  , unrep = leftAdjunct . const
  }

-- | An adjunction from Hask to Hask^op
-- 
-- >  Hask (f a) b ~ Op a (g b)
--
-- > rightAdjunct unit = id
-- > leftAdjunct counit = id
class (Contravariant f, Contravariant g) => DualAdjunction f g | f -> g, g -> f where
  unitOp :: g (f a) -> a
  counitOp :: f (g a) -> a
  leftAdjunctOp :: (f a -> b) -> g b -> a
  rightAdjunctOp :: (g b -> a) -> f a -> b

  unitOp = leftAdjunctOp id
  counitOp = rightAdjunctOp id
  leftAdjunctOp f = unitOp . contramap f
  rightAdjunctOp f = counitOp . contramap f
