{-# LANGUAGE Rank2Types
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeOperators
           , UndecidableInstances #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
#endif

-------------------------------------------------------------------------------------------
-- |
-- Copyright 	: 2008-2013 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: rank 2 types, MPTCs, fundeps
--
-------------------------------------------------------------------------------------------
module Data.Functor.Adjunction
  ( Adjunction(..)
  , adjuncted
  , tabulateAdjunction
  , indexAdjunction
  , zapWithAdjunction
  , zipR, unzipR
  , unabsurdL, absurdL
  , cozipL, uncozipL
  , extractL, duplicateL
  , splitL, unsplitL
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow ((&&&), (|||))
import Control.Monad.Free
#if __GLASGOW_HASKELL__ < 707
import Control.Monad.Instances ()
#endif
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Traced

import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Rep
import Data.Functor.Sum
import Data.Profunctor
import Data.Void
import GHC.Generics

-- | An adjunction between Hask and Hask.
--
-- Minimal definition: both 'unit' and 'counit' or both 'leftAdjunct'
-- and 'rightAdjunct', subject to the constraints imposed by the
-- default definitions that the following laws should hold.
--
-- > unit = leftAdjunct id
-- > counit = rightAdjunct id
-- > leftAdjunct f = fmap f . unit
-- > rightAdjunct f = counit . fmap f
--
-- Any implementation is required to ensure that 'leftAdjunct' and
-- 'rightAdjunct' witness an isomorphism from @Nat (f a, b)@ to
-- @Nat (a, g b)@
--
-- > rightAdjunct unit = id
-- > leftAdjunct counit = id
class (Functor f, Representable u) =>
      Adjunction f u | f -> u, u -> f where
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL (unit, counit) | (leftAdjunct, rightAdjunct) #-}
#endif
  unit         :: a -> u (f a)
  counit       :: f (u a) -> a
  leftAdjunct  :: (f a -> b) -> a -> u b
  rightAdjunct :: (a -> u b) -> f a -> b

  unit           = leftAdjunct id
  counit         = rightAdjunct id
  leftAdjunct f  = fmap f . unit
  rightAdjunct f = counit . fmap f

-- | 'leftAdjunct' and 'rightAdjunct' form two halves of an isomorphism.
--
-- This can be used with the combinators from the @lens@ package.
--
-- @'adjuncted' :: 'Adjunction' f u => 'Iso'' (f a -> b) (a -> u b)@
adjuncted :: (Adjunction f u, Profunctor p, Functor g)
          => p (a -> u b) (g (c -> u d)) -> p (f a -> b) (g (f c -> d))
adjuncted = dimap leftAdjunct (fmap rightAdjunct)
{-# INLINE adjuncted #-}

-- | Every right adjoint is representable by its left adjoint
-- applied to a unit element
--
-- Use this definition and the primitives in
-- Data.Functor.Representable to meet the requirements of the
-- superclasses of Representable.
tabulateAdjunction :: Adjunction f u => (f () -> b) -> u b
tabulateAdjunction f = leftAdjunct f ()

-- | This definition admits a default definition for the
-- 'index' method of 'Index", one of the superclasses of
-- Representable.
indexAdjunction :: Adjunction f u => u b -> f a -> b
indexAdjunction = rightAdjunct . const

zapWithAdjunction :: Adjunction f u => (a -> b -> c) -> u a -> f b -> c
zapWithAdjunction f ua = rightAdjunct (\b -> fmap (flip f b) ua)

splitL :: Adjunction f u => f a -> (a, f ())
splitL = rightAdjunct (flip leftAdjunct () . (,))

unsplitL :: Functor f => a -> f () -> f a
unsplitL = (<$)

extractL :: Adjunction f u => f a -> a
extractL = fst . splitL

duplicateL :: Adjunction f u => f a -> f (f a)
duplicateL as = as <$ as

-- | A right adjoint functor admits an intrinsic
-- notion of zipping
zipR :: Adjunction f u => (u a, u b) -> u (a, b)
zipR = leftAdjunct (rightAdjunct fst &&& rightAdjunct snd)

-- | Every functor in Haskell permits unzipping
unzipR :: Functor u => u (a, b) -> (u a, u b)
unzipR = fmap fst &&& fmap snd

absurdL :: Void -> f Void
absurdL = absurd

-- | A left adjoint must be inhabited, or we can derive bottom.
unabsurdL :: Adjunction f u => f Void -> Void
unabsurdL = rightAdjunct absurd

-- | And a left adjoint must be inhabited by exactly one element
cozipL :: Adjunction f u => f (Either a b) -> Either (f a) (f b)
cozipL = rightAdjunct (leftAdjunct Left ||| leftAdjunct Right)

-- | Every functor in Haskell permits 'uncozipping'
uncozipL :: Functor f => Either (f a) (f b) -> f (Either a b)
uncozipL = fmap Left ||| fmap Right

-- Requires deprecated Impredicative types
-- limitR :: Adjunction f u => (forall a. u a) -> u (forall a. a)
-- limitR = leftAdjunct (rightAdjunct (\(x :: forall a. a) -> x))

instance Adjunction ((,) e) ((->) e) where
  leftAdjunct f a e      = f (e, a)
  rightAdjunct f ~(e, a) = f a e

instance Adjunction Identity Identity where
  leftAdjunct f  = Identity . f . Identity
  rightAdjunct f = runIdentity . f . runIdentity

instance Adjunction f g =>
         Adjunction (IdentityT f) (IdentityT g) where
  unit   = IdentityT . leftAdjunct IdentityT
  counit = rightAdjunct runIdentityT . runIdentityT

instance Adjunction w m =>
         Adjunction (EnvT e w) (ReaderT e m) where
  unit              = ReaderT . flip fmap EnvT . flip leftAdjunct
  counit (EnvT e w) = rightAdjunct (flip runReaderT e) w

instance Adjunction m w =>
         Adjunction (WriterT s m) (TracedT s w) where
  unit   = TracedT . leftAdjunct (\ma s -> WriterT (fmap (\a -> (a, s)) ma))
  counit = rightAdjunct (\(t, s) -> ($s) <$> runTracedT t) . runWriterT

instance (Adjunction f g, Adjunction f' g') =>
         Adjunction (Compose f' f) (Compose g g') where
  unit   = Compose . leftAdjunct (leftAdjunct Compose)
  counit = rightAdjunct (rightAdjunct getCompose) . getCompose

instance (Adjunction f g, Adjunction f' g') =>
         Adjunction (Sum f f') (Product g g') where
  unit a = Pair (leftAdjunct InL a) (leftAdjunct InR a)
  counit (InL l) = rightAdjunct (\(Pair x _) -> x) l
  counit (InR r) = rightAdjunct (\(Pair _ x) -> x) r

instance Adjunction f u =>
         Adjunction (Free f) (Cofree u) where
  unit a = return a :< tabulateAdjunction (\k -> leftAdjunct (wrap . flip unsplitL k) a)
  counit (Pure a) = extract a
  counit (Free k) = rightAdjunct (flip indexAdjunction k . unwrap) (extractL k)

instance Adjunction V1 U1 where
  unit _ = U1
  counit = absurdV1

absurdV1 :: V1 a -> b
#if __GLASGOW_HASKELL__ >= 708
absurdV1 x = case x of {}
#else
absurdV1 x = x `seq` undefined
#endif

instance Adjunction Par1 Par1 where
  leftAdjunct f = Par1 . f . Par1
  rightAdjunct f = unPar1 . f . unPar1

instance Adjunction f g => Adjunction (Rec1 f) (Rec1 g) where
  unit   = Rec1 . leftAdjunct Rec1
  counit = rightAdjunct unRec1 . unRec1

-- @i@ and @c@ indexes have to be the same due functional dependency.
-- But we want them to be different, therefore we rather not define this instance
{-
instance Adjunction f g => Adjunction (M1 i c f) (M1 i c g) where
  unit   = M1 . leftAdjunct M1
  counit = rightAdjunct unM1 . unM1
-}

instance (Adjunction f g, Adjunction f' g') => Adjunction (f' :.: f) (g :.: g') where
  unit   = Comp1 . leftAdjunct (leftAdjunct Comp1)
  counit = rightAdjunct (rightAdjunct unComp1) . unComp1

instance (Adjunction f g, Adjunction f' g') => Adjunction (f :+: f') (g :*: g') where
  unit a = leftAdjunct L1 a :*: leftAdjunct R1 a
  counit (L1 l) = rightAdjunct (\(x :*: _) -> x) l
  counit (R1 r) = rightAdjunct (\(_ :*: x) -> x) r
