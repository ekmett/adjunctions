{-# LANGUAGE Rank2Types, MultiParamTypeClasses #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Data.Functor.Zap
-- Copyright 	: 2008-2011 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: rank-2 types, MPTCs
--
-------------------------------------------------------------------------------------------

module Data.Functor.Zap
	( Zap(..), zap, flipZap, zapAdjunction, composeZap
        , Bizap(..), bizap, flipBizap, bizapProductSum
	) where

import Data.Functor.Compose
import Data.Functor.Adjunction

newtype Zap f g = Zap { zapWith :: forall a b c. (a -> b -> c) -> f a -> g b -> c }

zap :: Zap f g -> f (a -> b) -> g a -> b
zap z = zapWith z id

flipZap :: Zap f g -> Zap g f 
flipZap (Zap z) = Zap $ \f a b -> z (flip f) b a

strength :: Functor f => a -> f b -> f (a, b)
strength = fmap . (,)

zapAdjunction :: Adjunction f g => Zap g f 
zapAdjunction = Zap $ \f a b -> uncurry (flip f) $ rightAdjunct (uncurry (flip strength)) $ strength a b 

composeZap :: Zap f g -> Zap h i -> Zap (Compose f h) (Compose g i) 
composeZap (Zap u) (Zap v) = Zap $ \f (Compose a) (Compose b) -> u (v f) a b

newtype Bizap p q = Bizap { bizapWith :: forall a b c d e.  (a -> c -> e) -> (b -> d -> e) -> p a b -> q c d -> e }

bizap :: Bizap p q -> p (a -> c) (b -> c) -> q a b -> c
bizap z = bizapWith z id id

flipBizap :: Bizap p q -> Bizap q p
flipBizap (Bizap z) = Bizap $ \f g a b -> z (flip f) (flip g) b a

bizapProductSum :: Bizap (,) Either
bizapProductSum = Bizap go where
  go l _ (f,_) (Left a) = l f a
  go _ r (_,g) (Right b) = r g b 
