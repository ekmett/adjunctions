{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Data.Functor.Contravariant.DualAdjunction 
  ( DualAdjunction(..)
  ) where

import Data.Functor.Contravariant

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
