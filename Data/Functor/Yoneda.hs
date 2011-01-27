{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Yoneda
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------

module Data.Functor.Yoneda
  ( Yoneda
  , yoneda
  , runYoneda
  , liftYoneda
  , lowerYoneda
  , YonedaT(..)
  , liftYonedaT
  , lowerYonedaT
  , maxF, minF, maxM, minM
  ) where

import Prelude hiding (sequence)
import Control.Applicative
import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Comonad
import Control.Comonad.Trans.Class
import Data.Distributive
import Data.Foldable
import Data.Function (on)
import Data.Functor.Apply
import Data.Functor.Plus
import Data.Functor.Identity
import Data.Functor.Adjunction
import Data.Traversable
import Text.Read hiding (lift)

type Yoneda = YonedaT Identity 

yoneda :: (forall b. (a -> b) -> b) -> Yoneda a
yoneda f = YonedaT (Identity . f)
{-# INLINE yoneda #-}

runYoneda :: Yoneda a -> (a -> b) -> b
runYoneda (YonedaT f) = runIdentity . f
{-# INLINE runYoneda #-}

liftYoneda :: a -> Yoneda a
liftYoneda a = YonedaT (\f -> Identity (f a))
{-# INLINE liftYoneda #-}

lowerYoneda :: Yoneda a -> a
lowerYoneda m = runIdentity (runYonedaT m id)
{-# INLINE lowerYoneda #-}

newtype YonedaT f a = YonedaT { runYonedaT :: forall b. (a -> b) -> f b } 

liftYonedaT :: Functor f => f a -> YonedaT f a 
liftYonedaT a = YonedaT (\f -> fmap f a)

lowerYonedaT :: YonedaT f a -> f a 
lowerYonedaT (YonedaT f) = f id

{-# RULES "lower/lift=id" liftYonedaT . lowerYonedaT = id #-}
{-# RULES "lift/lower=id" lowerYonedaT . liftYonedaT = id #-}

instance Functor (YonedaT f) where
  fmap f m = YonedaT (\k -> runYonedaT m (k . f))

instance Apply f => Apply (YonedaT f) where
  YonedaT m <.> YonedaT n = YonedaT (\f -> m (f .) <.> n id)
  
instance Applicative f => Applicative (YonedaT f) where
  pure a = YonedaT (\f -> pure (f a))
  YonedaT m <*> YonedaT n = YonedaT (\f -> m (f .) <*> n id)

instance Foldable f => Foldable (YonedaT f) where
  foldMap f = foldMap f . lowerYonedaT

-- a traversable isntance with a function in it!
instance Traversable f => Traversable (YonedaT f) where
  traverse f = fmap liftYonedaT . traverse f . lowerYonedaT

instance Distributive f => Distributive (YonedaT f) where
  collect f = liftYonedaT . collect (lowerYonedaT . f)

instance Adjunction f g => Adjunction (YonedaT f) (YonedaT g) where
  unit = liftYonedaT . fmap liftYonedaT . unit
  counit (YonedaT m) = counit (m lowerYonedaT)

-- instance Show1 f => Show1 (YonedaT f) where
instance Show (f a) => Show (YonedaT f a) where
  showsPrec d (YonedaT f) = showParen (d > 10) $
    showString "liftYonedaT " . showsPrec 11 (f id)

-- instance Read1 f => Read1 (YonedaT f) where
#ifdef __GLASGOW_HASKELL__
instance (Functor f, Read (f a)) => Read (YonedaT f a) where
  readPrec = parens $ prec 10 $ do
     Ident "liftYonedaT" <- lexP
     liftYonedaT <$> step readPrec
#endif

instance Eq (f a) => Eq (YonedaT f a) where
  (==) = (==) `on` lowerYonedaT

instance Ord (f a) => Ord (YonedaT f a) where
  compare = compare `on` lowerYonedaT

maxF :: (Functor f, Ord (f a)) => YonedaT f a -> YonedaT f a -> YonedaT f a
YonedaT f `maxF` YonedaT g = liftYonedaT $ f id `max` g id
-- {-# RULES "max/maxF" max = maxF #-}
{-# INLINE maxF #-}

minF :: (Functor f, Ord (f a)) => YonedaT f a -> YonedaT f a -> YonedaT f a
YonedaT f `minF` YonedaT g = liftYonedaT $ f id `max` g id
-- {-# RULES "min/minF" min = minF #-}
{-# INLINE minF #-}

maxM :: (Monad m, Ord (m a)) => YonedaT m a -> YonedaT m a -> YonedaT m a
YonedaT f `maxM` YonedaT g = lift $ f id `max` g id
-- {-# RULES "max/maxM" max = maxM #-}
{-# INLINE maxM #-}

minM :: (Monad m, Ord (m a)) => YonedaT m a -> YonedaT m a -> YonedaT m a
YonedaT f `minM` YonedaT g = lift $ f id `min` g id
-- {-# RULES "min/minM" min = minM #-}
{-# INLINE minM #-}

instance Alt f => Alt (YonedaT f) where
  YonedaT f <!> YonedaT g = YonedaT (\k -> f k <!> g k)

instance Plus f => Plus (YonedaT f) where
  zero = YonedaT $ const zero

instance Alternative f => Alternative (YonedaT f) where
  empty = YonedaT $ const empty
  YonedaT f <|> YonedaT g = YonedaT (\k -> f k <|> g k)
  
instance Monad m => Monad (YonedaT m) where
  return a = YonedaT (\f -> return (f a))
  YonedaT m >>= k = YonedaT (\f -> m id >>= \a -> runYonedaT (k a) f)

instance MonadFix m => MonadFix (YonedaT m) where
  mfix f = lift $ mfix (lowerYonedaT . f)

instance MonadPlus m => MonadPlus (YonedaT m) where
  mzero = YonedaT (const mzero)
  YonedaT f `mplus` YonedaT g = YonedaT (\k -> f k `mplus` g k)

instance MonadTrans YonedaT where
  lift a = YonedaT (\f -> liftM f a)

instance Extend w => Extend (YonedaT w) where
  extend k (YonedaT m) = YonedaT (\f -> extend (f . k . liftYonedaT) (m id))

instance Comonad w => Comonad (YonedaT w) where
  extract = extract . lowerYonedaT 

instance ComonadTrans YonedaT where
  lower = lowerYonedaT 
