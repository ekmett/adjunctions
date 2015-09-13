{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- > Cont r ~ Contravariant.Adjoint (Op r) (Op r)
-- > Conts r ~ Contravariant.AdjointT (Op r) (Op r)
-- > ContsT r w m ~ Contravariant.AdjointT (Op (m r)) (Op (m r)) w
----------------------------------------------------------------------------

module Control.Monad.Trans.Conts
  (
  -- * Continuation passing style
    Cont
  , cont
  , runCont
  -- * Multiple-continuation passing style
  , Conts
  , runConts
  , conts
  -- * Multiple-continuation passing style transformer
  , ContsT(..)
  , callCC
  ) where

import Prelude hiding (sequence)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.Monad.Trans.Class
import Control.Monad (ap)
import Data.Functor.Apply
import Data.Functor.Identity

type Cont r = ContsT r Identity Identity

cont :: ((a -> r) -> r) -> Cont r a
cont f = ContsT $ \ (Identity k) -> Identity $ f $ runIdentity . k

runCont :: Cont r a -> (a -> r) -> r
runCont (ContsT k) f = runIdentity $ k $ Identity (Identity . f)

type Conts r w = ContsT r w Identity

conts :: Functor w => (w (a -> r) -> r) -> Conts r w a
conts k = ContsT $ Identity . k . fmap (runIdentity .)

runConts :: Functor w => Conts r w a -> w (a -> r) -> r
runConts (ContsT k) = runIdentity . k . fmap (Identity .)

newtype ContsT r w m a = ContsT { runContsT :: w (a -> m r) -> m r }

instance Functor w => Functor (ContsT r w m) where
  fmap f (ContsT k) = ContsT $ k . fmap (. f)

instance Comonad w => Apply (ContsT r w m) where
  (<.>) = ap

instance Comonad w => Applicative (ContsT r w m) where
  pure x = ContsT $ \f -> extract f x
  (<*>) = ap

instance Comonad w => Monad (ContsT r w m) where
  return = pure
  ContsT k >>= f = ContsT $ k . extend (\wa a -> runContsT (f a) wa)

callCC :: Comonad w => ((a -> ContsT r w m b) -> ContsT r w m a) -> ContsT r w m a
callCC f = ContsT $ \wamr -> runContsT (f (\a -> ContsT $ \_ -> extract wamr a)) wamr

{-
callCCs :: Comonad w => (w (a -> ContsT r w m b) -> ContsT r w m a) -> ContsT r w m a
callCCs f =
-}

instance Comonad w => MonadTrans (ContsT r w) where
  lift m = ContsT $ extract . fmap (m >>=)
