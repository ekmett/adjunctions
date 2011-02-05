{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}

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
  , tabulateAdjunction
  , indexAdjunction
  , zipR, unzipF
  , inhabitedL
  , cozipL, uncozipF
  ) where

import Control.Applicative
import Control.Arrow ((&&&), (|||))
import Control.Monad.Instances ()
import Control.Monad.Trans.Identity

import Control.Monad.Representable
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Traced

import Data.Functor.Identity
import Data.Functor.Compose

import Data.Void

-- | An adjunction between Hask and Hask.
--
-- Minimal definition: both 'unit' and 'counit' or both 'leftAdjunct' and 'rightAdjunct', 
-- subject to the constraints imposed by the default definitions that the following laws
-- should hold.
--
-- > unit = leftAdjunct id
-- > counit = rightAdjunct id
-- > leftAdjunct f = fmap f . unit
-- > rightAdjunct f = counit . fmap f
--
-- Any implementation is required to ensure that 'leftAdjunct' and 'rightAdjunct' witness
-- an isomorphism from @Nat (f a, b)@ to @Nat (a, g b)@
--
-- > rightAdjunct unit = id
-- > leftAdjunct counit = id 
class (Functor f, Representable u) => Adjunction f u | f -> u, u -> f where
  unit :: a -> u (f a)
  counit :: f (u a) -> a
  leftAdjunct :: (f a -> b) -> a -> u b
  rightAdjunct :: (a -> u b) -> f a -> b
  
  unit = leftAdjunct id
  counit = rightAdjunct id
  leftAdjunct f = fmap f . unit
  rightAdjunct f = counit . fmap f

-- | Every right adjoint is representable by its left adjoint applied to a unit element
-- 
-- Use this definition and the primitives in Data.Functor.Representable to meet the requirements
-- of the superclasses of Representable.
tabulateAdjunction :: Adjunction f u => (f () -> b) -> u b
tabulateAdjunction f = leftAdjunct f ()

-- | This definition admits a default definition for the 'index' method of 'Index", one of the
-- superclasses of Representable.
indexAdjunction :: Adjunction f u => u b -> f a -> b
indexAdjunction = rightAdjunct . const

-- | A right adjoint functor admits an intrinsic notion of zipping
zipR :: Adjunction f u => (u a, u b) -> u (a, b)
zipR = leftAdjunct (rightAdjunct fst &&& rightAdjunct snd)

-- | Every functor in Haskell permits unzipping
unzipF :: Functor u => u (a, b) -> (u a, u b)
unzipF = fmap fst &&& fmap snd

-- | A left adjoint must be inhabited, or we can derive bottom
inhabitedL :: Adjunction f u => f Void -> Void
inhabitedL = rightAdjunct absurd

-- | And a left adjoint must be inhabited by exactly one element
cozipL :: Adjunction f u => f (Either a b) -> Either (f a) (f b)
cozipL = rightAdjunct (leftAdjunct Left ||| leftAdjunct Right)

-- | Every functor in Haskell permits 'uncozipping'
uncozipF :: Functor f => Either (f a) (f b) -> f (Either a b)
uncozipF = fmap Left ||| fmap Right

-- Requires deprecated Impredicative types

-- limitR :: Adjunction f u => (forall a. u a) -> u (forall a. a)
-- limitR = leftAdjunct (rightAdjunct (\(x :: forall a. a) -> x))

instance Adjunction ((,)e) ((->)e) where
  leftAdjunct f a e = f (e, a)
  rightAdjunct f ~(e, a) = f a e

instance Adjunction Identity Identity where
  leftAdjunct f = Identity . f . Identity
  rightAdjunct f = runIdentity . f . runIdentity

instance Adjunction f g => Adjunction (IdentityT f) (IdentityT g) where
  unit = IdentityT . leftAdjunct IdentityT
  counit = rightAdjunct runIdentityT . runIdentityT

instance Adjunction w m => Adjunction (EnvT e w) (ReaderT e m) where
  unit = ReaderT . flip fmap EnvT . flip leftAdjunct
  counit (EnvT e w) = rightAdjunct (flip runReaderT e) w

instance Adjunction m w => Adjunction (WriterT s m) (TracedT s w) where
  unit = TracedT . leftAdjunct (\ma s -> WriterT (fmap (\a -> (a, s)) ma)) 
  counit  = rightAdjunct (\(t, s) -> ($s) <$> runTracedT t) . runWriterT

instance (Adjunction f g, Adjunction f' g') => Adjunction (Compose f' f) (Compose g g') where
  unit = Compose . leftAdjunct (leftAdjunct Compose) 
  counit = rightAdjunct (rightAdjunct getCompose) . getCompose
