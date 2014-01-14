{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
----------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2011-2014
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
--
-- Representable contravariant endofunctors over the category of Haskell
-- types are isomorphic to @(_ -> r)@ and resemble mappings to a
-- fixed range.
----------------------------------------------------------------------
module Data.Functor.Contravariant.Rep
  (
  -- * Representable Contravariant Functors
    Representable(..)
  -- * Default definitions
  , contramapRep
  ) where

import Control.Monad.Reader
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Day
import Data.Functor.Product
import Data.Proxy
import Prelude hiding (lookup)

-- | A 'Contravariant' functor @f@ is 'Representable' if 'tabulate' and 'index' witness an isomorphism to @(_ -> Rep f)@.
class Contravariant f => Representable f where
  type Rep f :: *
  -- |
  -- @
  -- 'contramap' f ('tabulate' g) = 'tabulate' (g . f)
  -- @
  tabulate :: (a -> Rep f) -> f a

  index :: f a -> a -> Rep f

  -- |
  -- @
  -- 'contramapWithRep' f p ≡ 'tabulate' $ 'either (index p) 'id' . f
  -- @
  contramapWithRep :: (b -> Either a (Rep f)) -> f a -> f b
  contramapWithRep f p = tabulate $ either (index p) id . f

contramapRep :: Representable f => (a -> b) -> f b -> f a
contramapRep f = tabulate . (. f) . index

instance Representable Proxy where
  type Rep Proxy = ()
  tabulate _ = Proxy
  index Proxy _ = ()
  contramapWithRep _ Proxy = Proxy

instance (Representable f, Representable g) => Representable (Day f g) where
  type Rep (Day f g) = (Rep f, Rep g)
  tabulate a2fg = Day (tabulate fst) (tabulate snd) $ \a -> let b = a2fg a in (b,b)
  index (Day fb gc abc) a = case abc a of
    (b, c) -> (index fb b, index gc c)
  contramapWithRep d2eafg (Day fb gc abc) = Day (contramapWithRep id fb) (contramapWithRep id gc) $ \d -> case d2eafg d of
    Left a -> case abc a of
      (b, c) -> (Left b, Left c)
    Right (vf, vg) -> (Right vf, Right vg)
  {-# INLINE tabulate #-}

instance Representable (Op r) where
  type Rep (Op r) = r
  tabulate = Op
  index = getOp

instance Representable Predicate where
  type Rep Predicate = Bool
  tabulate = Predicate
  index = getPredicate

instance (Representable f, Representable g) => Representable (Product f g) where
  type Rep (Product f g) = (Rep f, Rep g)
  tabulate f = Pair (tabulate (fst . f)) (tabulate (snd . f))
  index (Pair f g) a = (index f a, index g a)
  contramapWithRep h (Pair f g) = Pair
      (contramapWithRep (fmap fst . h) f)
      (contramapWithRep (fmap snd . h) g)
