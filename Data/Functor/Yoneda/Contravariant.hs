{-# LANGUAGE CPP, GADTs, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Yoneda.Contravariant
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs, MPTCs, fundeps
--
----------------------------------------------------------------------------
module Data.Functor.Yoneda.Contravariant
  ( Yoneda
  , yoneda
  , liftYoneda
  , lowerYoneda
  , liftYonedaT
  , lowerYonedaT
  , lowerM
  , YonedaT(..)
  ) where

import Prelude hiding (sequence)
import Control.Applicative
import Control.Monad (MonadPlus(..), ap, liftM)
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Monad.Fix
import Data.Foldable
import Data.Traversable
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Identity
import Data.Functor.Adjunction
import Data.Distributive
import Text.Read hiding (lift)

type Yoneda = YonedaT Identity

-- | The contravariant Yoneda lemma applied to a covariant functor
data YonedaT f a where
  YonedaT :: (b -> a) -> f b -> YonedaT f a

yoneda :: (b -> a) -> b -> Yoneda a
yoneda f = YonedaT f . Identity

liftYoneda :: a -> Yoneda a 
liftYoneda = YonedaT id . Identity

lowerYoneda :: Yoneda a -> a
lowerYoneda (YonedaT f (Identity a)) = f a

liftYonedaT :: f a -> YonedaT f a 
liftYonedaT = YonedaT id

lowerYonedaT :: Functor f => YonedaT f a -> f a
lowerYonedaT (YonedaT f m) = fmap f m

lowerM :: Monad m => YonedaT f a -> f a 
lowerM (YonedaT f m) = liftM f m

instance Functor (YonedaT f) where
  fmap f (YonedaT g v) = YonedaT (f . g) v

instance Applicative f => Applicative (YonedaT f) where
  pure = hreturn . pure
  m <*> n = YonedaT id (lowerYonedaT m <*> lowerYonedaT n)

instance Monad m => Monad (YonedaT m) where
  return = YonedaT id . return
  YonedaT f v >>= k = lift (v >>= lowerM . k . f)

instance MonadTrans YonedaT where
  lift = YonedaT id

instance Comonad w => Comonad (YonedaT w) where
  extract (YonedaT f v) = f (extract v)
  extend k (YonedaT f v) = YonedaT id $ extend (k . CoYoneda f) v

instance ComonadTrans YonedaT where
  lower (YonedaT f a) = fmap f a

instance (Foldable f, Functor f) => Foldable (YonedaT f) where
  foldMap f (YonedaT k a) = foldMap (f . k) a

instance Traversable f => Traversable (YonedaT f) where
  traverse f (YonedaT k a) = YonedaT id <$> traverse (f . k) a

instance (Functor f, Show (f a)) => Show (YonedaT f) where
  showsPrec d (YonedaT f a) = showParen (d > 10) $
    showString "liftYonedaT " . showsPrec 11 (f a)

#ifdef __GLASGOW_HASKELL__
instance (Functor f, Read (f a)) => Read (YonedaT f) where
  readPrec = paren $ prec 10 $ do
    Ident "liftYonedaT" <- lexP
    liftYonedaT <$> step readPrec
#endif

instance (Functor f, Eq (f a)) => Eq (YonedaT f) where
  (==) = (==) `on` lowerYonedaT

instance (Functor f, Ord (f a)) => Ord (YonedaT f) where
  compare = compare `on` lowerYonedaT
