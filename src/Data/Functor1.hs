{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module Data.Functor1 where

import Control.Applicative (Const(..))
import Data.Functor.Identity
import Data.Proxy

class Functor1 w where
  -- | @
  -- 'map1' f . 'map1' g = 'map1' (f . g)
  -- 'map1' id = id
  -- @
  map1 :: (forall a. f a -> g a) -> w f -> w g

map1Identity :: Functor1 w => (forall x. f x -> x) -> w f -> w Identity
map1Identity f = map1 (Identity . f)

instance Functor1 Proxy where
  map1 _ Proxy = Proxy

instance Functor1 (Const a) where
  map1 _ = Const . getConst
