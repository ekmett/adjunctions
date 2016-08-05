{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
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
----------------------------------------------------------------------------

module Data.Functor.Contravariant.Adjunction
  ( Adjunction(..)
  , adjuncted
  , contrarepAdjunction
  , coindexAdjunction
  ) where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 707
import Control.Monad.Instances ()
#endif
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Rep
import Data.Profunctor

-- | An adjunction from @Hask^op@ to @Hask@
--
-- @'Op' (f a) b ~ 'Hask' a (g b)@
--
-- @
-- 'rightAdjunct' 'unit' = 'id'
-- 'leftAdjunct' 'counit' = 'id'
-- @
--
-- Any adjunction from @Hask@ to @Hask^op@ would indirectly
-- permit @unsafePerformIO@, and therefore does not exist.

class (Contravariant f, Representable g) => Adjunction f g | f -> g, g -> f where
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL (unit, counit) | (leftAdjunct, rightAdjunct) #-}
#endif
  unit :: a -> g (f a) -- monad in Hask
  counit :: a -> f (g a) -- comonad in Hask^op
  leftAdjunct  :: (b -> f a) -> a -> g b
  rightAdjunct :: (a -> g b) -> b -> f a

  unit = leftAdjunct id
  counit = rightAdjunct id
  leftAdjunct f = contramap f . unit
  rightAdjunct f = contramap f . counit

-- | 'leftAdjunct' and 'rightAdjunct' form two halves of an isomorphism.
--
-- This can be used with the combinators from the @lens@ package.
--
-- @'adjuncted' :: 'Adjunction' f g => 'Iso'' (b -> f a) (a -> g b)@
adjuncted :: (Adjunction f g, Profunctor p, Functor h) 
          => p (a -> g b) (h (c -> g d)) -> p (b -> f a) (h (d -> f c))
adjuncted = dimap leftAdjunct (fmap rightAdjunct)
{-# INLINE adjuncted #-}

-- | This 'Adjunction' gives rise to the @Cont@ 'Monad'
instance Adjunction (Op r) (Op r) where
  unit a = Op (\k -> getOp k a)
  counit = unit

-- | This gives rise to the @Cont Bool@ 'Monad'
instance Adjunction Predicate Predicate where
  unit a = Predicate (\k -> getPredicate k a)
  counit = unit

-- | Represent a 'Contravariant' functor that has a left adjoint
contrarepAdjunction :: Adjunction f g => (a -> f ()) -> g a
contrarepAdjunction = flip leftAdjunct ()

coindexAdjunction :: Adjunction f g => g a -> a -> f ()
coindexAdjunction = rightAdjunct . const
