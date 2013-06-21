{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Contravariant.Yoneda
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Data.Functor.Contravariant.Yoneda
  ( YonedaT(..)
  , liftYonedaT
  , lowerYonedaT
  , maxF, minF
  ) where

import Prelude hiding (sequence)
import Data.Functor.Identity
import Data.Functor.Contravariant.Adjunction
import Data.Functor.Contravariant.DualAdjunction
import Data.Functor.Contravariant
import Data.Distributive
import Text.Read hiding (lift)

-- | The covariant Yoneda lemma applied to a contravariant functor

newtype YonedaT f a = YonedaT { runYonedaT :: forall b. (b -> a) -> f b } 

liftYonedaT :: Contravariant f => f a -> YonedaT f a 
liftYonedaT a = YonedaT (\f -> contramap f a)

lowerYonedaT :: YonedaT f a -> f a 
lowerYonedaT (YonedaT f) = f id

instance Contravariant (YonedaT f) where
  contramap f m = YonedaT (\k -> runYonedaT m (f . k))

instance Adjunction f g => Adjunction (YonedaT f) (YonedaT g) where
  unit = liftYonedaT . contramap lowerYonedaT . unit
  counit = liftYonedaT . contramap lowerYonedaT . counit

instance DualAdjunction f g => DualAdjunction (YonedaT f) (YonedaT g) where
  unitOp = unitOp . contramap liftYonedaT . lowerYonedaT
  counitOp = counitOp . contramap liftYonedaT . lowerYonedaT

instance Eq (f a) => Eq (YonedaT f a) where
  YonedaT f == YonedaT g = f id == g id

instance Ord (f a) => Ord (YonedaT f a) where
  YonedaT f `compare` YonedaT g = f id `compare` g id

maxF :: (Contravariant f, Ord (f a)) => YonedaT f a -> YonedaT f a -> YonedaT f a
YonedaT f `maxF` YonedaT g = liftYonedaT $ f id `max` g id
-- {-# RULES "max/maxF" max = maxF #-}
{-# INLINE maxF #-}

minF :: (Contravariant f, Ord (f a)) => YonedaT f a -> YonedaT f a -> YonedaT f a
YonedaT f `minF` YonedaT g = liftYonedaT $ f id `max` g id
-- {-# RULES "min/minF" min = minF #-}
{-# INLINE minF #-}
