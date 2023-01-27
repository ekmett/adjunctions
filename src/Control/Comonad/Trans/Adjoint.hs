{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Control.Comonad.Trans.Adjoint
  ( Adjoint
  , runAdjoint
  , adjoint
  , AdjointT(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import GHC.Generics
import Control.Comonad
import Control.Comonad.Trans.Class
import Data.Functor.Adjunction
import Data.Functor.Extend
import Data.Functor.Identity
import Data.Distributive

type Adjoint f g = AdjointT f g Identity

newtype AdjointT f g w a = AdjointT { runAdjointT :: f (w (g a)) }

adjoint :: Functor f => f (g a) -> Adjoint f g a
adjoint = AdjointT . fmap Identity

runAdjoint :: Functor f => Adjoint f g a -> f (g a)
runAdjoint = fmap runIdentity . runAdjointT

instance (Adjunction f g, Functor w) => Functor (AdjointT f g w) where
  fmap f (AdjointT g) = AdjointT $ fmap (fmap (fmap f)) g
  b <$ (AdjointT g) = AdjointT $ fmap (fmap (b <$)) g

instance (Adjunction f g, Extend w) => Extend (AdjointT f g w) where
  extended f (AdjointT m) = AdjointT $ fmap (extended $ leftAdjunct (f . AdjointT)) m

instance (Adjunction f g, Comonad w) => Comonad (AdjointT f g w) where
  extend f (AdjointT m) = AdjointT $ fmap (extend $ leftAdjunct (f . AdjointT)) m
  extract = rightAdjunct extract . runAdjointT

{-
instance (Adjunction f g, Monad m) => Applicative (AdjointT f g m) where
  pure = AdjointT . leftAdjunct return
  (<*>) = ap
-}

instance (Adjunction f g, Distributive g) => ComonadTrans (AdjointT f g) where
  lower = counit . fmap distribute . runAdjointT

-- | Compose two adjoints in one
compsedAdjoint :: (Adjunction f g, Adjunction f2 g2, Comonad w, ComonadApply w, Functor g)
	=> AdjointT f g w a -> AdjointT f2 g2 w b -> AdjointT (f2 :.: f) (g :.: g2) w (a,b)
compsedAdjoint (AdjointT a1) (AdjointT a2) = 
	AdjointT $ (fmap . fmap) Comp1 $ Comp1 $ 
	fmap (\x-> fmap (\y-> liftW2 (\t h-> fmap (\a-> fmap (\b-> (a,b) )  t) h) x y )  a1 ) a2

-- | Combine two adjoints in list of union adjoints
combineAdjoint :: (Adjunction f g, Adjunction f2 g2, Comonad w, Functor g)
	=> AdjointT f g w a -> AdjointT f2 g2 w b -> [AdjointT (f :+: f2) (g :*: g2) w (Either a b)]
combineAdjoint (AdjointT a1) (AdjointT a2) = 
	[AdjointT $ ((fmap . fmap) (\g-> (fmap Left g) :*: (fmap Right $ extract $ extractL $ a2 ) ) $ L1 a1)] ++
	[AdjointT $ (((fmap . fmap) (\g2-> (fmap Left $ extract $ extractL a1 ) :*: (fmap Right g2)) . R1) a2)]
