{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  , tabulated
  -- * Default definitions
  , contramapRep
  ) where

import Control.Monad.Reader
import Data.Functor.Contravariant
import Data.Functor.Product
import Data.Profunctor
import Data.Proxy
import GHC.Generics hiding (Rep)
import Prelude hiding (lookup)

-- | A 'Contravariant' functor @f@ is 'Representable' if 'tabulate' and 'index' witness an isomorphism to @(_ -> Rep f)@.
--
-- @
-- 'tabulate' . 'index' ≡ id
-- 'index' . 'tabulate' ≡ id
-- @
class Contravariant f => Representable f where
  type Rep f :: *
  -- |
  -- @
  -- 'contramap' f ('tabulate' g) = 'tabulate' (g . f)
  -- @
  tabulate :: (a -> Rep f) -> f a

  index    :: f a -> a -> Rep f

  -- |
  -- @
  -- 'contramapWithRep' f p ≡ 'tabulate' $ 'either' ('index' p) 'id' . f
  -- @
  contramapWithRep :: (b -> Either a (Rep f)) -> f a -> f b
  contramapWithRep f p = tabulate $ either (index p) id . f

{-# RULES
"tabulate/index" forall t. tabulate (index t) = t #-}

-- | 'tabulate' and 'index' form two halves of an isomorphism.
--
-- This can be used with the combinators from the @lens@ package.
--
-- @'tabulated' :: 'Representable' f => 'Iso'' (a -> 'Rep' f) (f a)@
tabulated :: (Representable f, Representable g, Profunctor p, Functor h)
          => p (f a) (h (g b)) -> p (a -> Rep f) (h (b -> Rep g))
tabulated = dimap tabulate (fmap index)
{-# INLINE tabulated #-}

contramapRep :: Representable f => (a -> b) -> f b -> f a
contramapRep f = tabulate . (. f) . index

instance Representable Proxy where
  type Rep Proxy = ()
  tabulate _ = Proxy
  index Proxy _ = ()
  contramapWithRep _ Proxy = Proxy

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

instance Representable U1 where
  type Rep U1 = ()
  tabulate _ = U1
  index U1 _ = ()
  contramapWithRep _ U1 = U1

instance (Representable f, Representable g) => Representable (f :*: g) where
  type Rep (f :*: g) = (Rep f, Rep g)
  tabulate f = tabulate (fst . f) :*: tabulate (snd . f)
  index (f :*: g) a = (index f a, index g a)
  contramapWithRep h (f :*: g) =
    contramapWithRep (fmap fst . h) f :*: contramapWithRep (fmap snd . h) g
