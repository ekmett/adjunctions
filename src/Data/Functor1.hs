{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module Data.Functor1 where

import Data.Functor.Identity

#if MIN_VERSION_base(4,7,0)
import Data.Proxy
#endif

#if MIN_VERSION_base(4,9,0)
import Control.Applicative (Const(..))
#endif

class Functor1 w where
  -- | @
  -- 'map1' f . 'map1' g = 'map1' (f . g)
  -- 'map1' id = id
  -- @
  map1 :: (forall a. f a -> g a) -> w f -> w g

map1Identity :: Functor1 w => (forall x. f x -> x) -> w f -> w Identity
map1Identity f = map1 (Identity . f)

#if MIN_VERSION_base(4,7,0)
instance Functor1 Proxy where
  map1 _ Proxy = Proxy
#endif

#if MIN_VERSION_base(4,9,0)
instance Functor1 (Const a) where
  map1 _ = Const . getConst
#endif
