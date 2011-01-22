{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Contra.Cont
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs, fundeps
--
-- > ContT r ~ AdjointT (Op r) (Op r)
----------------------------------------------------------------------------

module Control.Monad.Contra.Cont
  ( Cont
  , runCont
  , cont
  , ContT(..)
  , callCC
  ) where

import Prelude hiding (sequence)
import Control.Applicative
import Control.Comonad
import Control.Monad (ap)
import Data.Functor.Apply
import Data.Functor.Identity

type Cont r = ContT r Identity

newtype ContT r w a = ContT { runContT :: w (a -> r) -> r }

cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT $ f . runIdentity

runCont :: Cont r a -> (a -> r) -> r
runCont (ContT k) = k . Identity

instance Functor w => Functor (ContT r w) where
  fmap f (ContT k) = ContT $ k . fmap (. f)

instance Comonad w => FunctorApply (ContT r w) where
  (<.>) = ap
  
instance Comonad w => Applicative (ContT r w) where
  pure x = ContT $ \wk -> extract wk x
  (<*>) = ap

instance Comonad w => Monad (ContT r w) where
  return = pure
  ContT k >>= f = ContT $ k . extend (\wa a -> runContT (f a) wa)

callCC :: Comonad w => ((a -> ContT r w b) -> ContT r w a) -> ContT r w a
callCC f = ContT $ \wc -> runContT (f (\a -> ContT $ \_ -> extract wc a)) wc

