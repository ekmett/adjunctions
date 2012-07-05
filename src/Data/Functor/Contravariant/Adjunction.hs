{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Data.Functor.Contravariant.Adjunction 
  ( Adjunction(..)
  , corepAdjunction
  , coindexAdjunction
  ) where

import Control.Monad.Instances ()
import Data.Functor.Contravariant
import Data.Functor.Corepresentable

-- | An adjunction from Hask^op to Hask
-- 
-- > Op (f a) b ~ Hask a (g b)
--
-- > rightAdjunct unit = id
-- > leftAdjunct counit = id
--
-- Any adjunction from Hask to Hask^op would indirectly
-- permit unsafePerformIO, and therefore does not exist.

class (Contravariant f, Corepresentable g) => Adjunction f g | f -> g, g -> f where
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

-- | Represent a contravariant functor that has a left adjoint
corepAdjunction :: Adjunction f g => (a -> f ()) -> g a
corepAdjunction = flip leftAdjunct () 

coindexAdjunction :: Adjunction f g => g a -> a -> f ()
coindexAdjunction = rightAdjunct . const
