{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
----------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2011-2014
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
--
-- Representable endofunctors over the category of Haskell types are
-- isomorphic to the reader monad and so inherit a very large number
-- of properties for free.
----------------------------------------------------------------------

module Data.Functor.Rep
  (
  -- * Representable Functors
    Representable(..)
  , tabulated
  -- * Wrapped representable functors
  , Co(..)
  -- * Default definitions
  -- ** Functor
  , fmapRep
  -- ** Distributive
  , distributeRep
  -- ** Apply/Applicative
  , apRep
  , pureRep
  , liftR2
  , liftR3
  -- ** Bind/Monad
  , bindRep
  -- ** MonadFix
  , mfixRep
  -- ** MonadZip
  , mzipRep
  , mzipWithRep
  -- ** MonadReader
  , askRep
  , localRep
  -- ** Extend
  , duplicatedRep
  , extendedRep
  -- ** Comonad
  , duplicateRep
  , extendRep
  , extractRep
  -- ** Comonad, with user-specified monoid
  , duplicateRepBy
  , extendRepBy
  , extractRepBy
  ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Traced
import Control.Comonad.Cofree
import Control.Monad.Trans.Identity
import Control.Monad.Reader
#if MIN_VERSION_base(4,4,0)
import Data.Complex
#endif
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Extend
import Data.Functor.Product
import qualified Data.Monoid as Monoid
import Data.Profunctor
import Data.Proxy
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Semigroup hiding (Product)
import Data.Tagged
import Data.Void
import Prelude hiding (lookup)

-- | A 'Functor' @f@ is 'Representable' if 'tabulate' and 'index' witness an isomorphism to @(->) x@.
--
-- Every 'Distributive' 'Functor' is actually 'Representable'.
--
-- Every 'Representable' 'Functor' from Hask to Hask is a right adjoint.
--
-- @
-- 'tabulate' . 'index'  ≡ id
-- 'index' . 'tabulate'  ≡ id
-- 'tabulate' . 'return' ≡ 'return'
-- @

class Distributive f => Representable f where
  type Rep f :: *
  -- |
  -- @
  -- 'fmap' f . 'tabulate' ≡ 'tabulate' . 'fmap' f
  -- @
  tabulate :: (Rep f -> a) -> f a
  index    :: f a -> Rep f -> a

{-# RULES
"tabulate/index" forall t. tabulate (index t) = t #-}

-- | 'tabulate' and 'index' form two halves of an isomorphism.
--
-- This can be used with the combinators from the @lens@ package.
--
-- @'tabulated' :: 'Representable' f => 'Iso'' ('Rep' f -> a) (f a)@
tabulated :: (Representable f, Representable g, Profunctor p, Functor h)
          => p (f a) (h (g b)) -> p (Rep f -> a) (h (Rep g -> b))
tabulated = dimap tabulate (fmap index)
{-# INLINE tabulated #-}

-- * Default definitions

fmapRep :: Representable f => (a -> b) -> f a -> f b
fmapRep f = tabulate . fmap f . index

pureRep :: Representable f => a -> f a
pureRep = tabulate . const

bindRep :: Representable f => f a -> (a -> f b) -> f b
bindRep m f = tabulate $ \a -> index (f (index m a)) a

mfixRep :: Representable f => (a -> f a) -> f a
mfixRep = tabulate . mfix . fmap index

mzipWithRep :: Representable f => (a -> b -> c) -> f a -> f b -> f c
mzipWithRep f as bs = tabulate $ \k -> f (index as k) (index bs k)

mzipRep :: Representable f => f a -> f b -> f (a, b)
mzipRep as bs = tabulate (index as &&& index bs)

askRep :: Representable f => f (Rep f)
askRep = tabulate id

localRep :: Representable f => (Rep f -> Rep f) -> f a -> f a
localRep f m = tabulate (index m . f)

apRep :: Representable f => f (a -> b) -> f a -> f b
apRep f g = tabulate (index f <*> index g)

distributeRep :: (Representable f, Functor w) => w (f a) -> f (w a)
distributeRep wf = tabulate (\k -> fmap (`index` k) wf)

duplicateRepBy :: Representable f => (Rep f -> Rep f -> Rep f) -> f a -> f (f a)
duplicateRepBy plus w = tabulate (\m -> tabulate (index w . plus m))

extendRepBy :: Representable f => (Rep f -> Rep f -> Rep f) -> (f a -> b) -> f a -> f b
extendRepBy plus f w = tabulate (\m -> f (tabulate (index w . plus m)))

extractRepBy :: Representable f => (Rep f) -> f a -> a
extractRepBy = flip index

duplicatedRep :: (Representable f, Semigroup (Rep f)) => f a -> f (f a)
duplicatedRep = duplicateRepBy (<>)

extendedRep :: (Representable f, Semigroup (Rep f)) => (f a -> b) -> f a -> f b
extendedRep = extendRepBy (<>)

duplicateRep :: (Representable f, Monoid (Rep f)) => f a -> f (f a)
duplicateRep = duplicateRepBy mappend

extendRep :: (Representable f, Monoid (Rep f)) => (f a -> b) -> f a -> f b
extendRep = extendRepBy mappend

extractRep :: (Representable f, Monoid (Rep f)) => f a -> a
extractRep = extractRepBy mempty

-- * Instances

instance Representable Proxy where
  type Rep Proxy = Void
  index Proxy = absurd
  tabulate _ = Proxy

instance Representable Identity where
  type Rep Identity = ()
  index (Identity a) () = a
  tabulate f = Identity (f ())

instance Representable (Tagged t) where
  type Rep (Tagged t) = ()
  index (Tagged a) () = a
  tabulate f = Tagged (f ())

instance Representable m => Representable (IdentityT m) where
  type Rep (IdentityT m) = Rep m
  index (IdentityT m) i = index m i
  tabulate = IdentityT . tabulate

instance Representable ((->) e) where
  type Rep ((->) e) = e
  index = id
  tabulate = id

instance Representable m => Representable (ReaderT e m) where
  type Rep (ReaderT e m) = (e, Rep m)
  index (ReaderT f) (e,k) = index (f e) k
  tabulate = ReaderT . fmap tabulate . curry

instance (Representable f, Representable g) => Representable (Compose f g) where
  type Rep (Compose f g) = (Rep f, Rep g)
  index (Compose fg) (i,j) = index (index fg i) j
  tabulate = Compose . tabulate . fmap tabulate . curry

instance Representable w => Representable (TracedT s w) where
  type Rep (TracedT s w) = (s, Rep w)
  index (TracedT w) (e,k) = index w k e
  tabulate = TracedT . unCo . collect (Co . tabulate) . curry

instance (Representable f, Representable g) => Representable (Product f g) where
  type Rep (Product f g) = Either (Rep f) (Rep g)
  index (Pair a _) (Left i)  = index a i
  index (Pair _ b) (Right j) = index b j
  tabulate f = Pair (tabulate (f . Left)) (tabulate (f . Right))

instance Representable f => Representable (Cofree f) where
  type Rep (Cofree f) = Seq (Rep f)
  index (a :< as) key = case Seq.viewl key of
      Seq.EmptyL -> a
      k Seq.:< ks -> index (index as k) ks
  tabulate f = f Seq.empty :< tabulate (\k -> tabulate (f . (k Seq.<|)))

instance Representable Monoid.Dual where
  type Rep Monoid.Dual = ()
  index (Monoid.Dual d) () = d
  tabulate f = Monoid.Dual (f ())

instance Representable Monoid.Product where
  type Rep Monoid.Product = ()
  index (Monoid.Product p) () = p
  tabulate f = Monoid.Product (f ())

instance Representable Monoid.Sum where
  type Rep Monoid.Sum = ()
  index (Monoid.Sum s) () = s
  tabulate f = Monoid.Sum (f ())

#if MIN_VERSION_base(4,4,0)
instance Representable Complex where
  type Rep Complex = Bool
  index (r :+ i) key = if key then i else r
  tabulate f = f False :+ f True
#endif

newtype Co f a = Co { unCo :: f a } deriving Functor

instance Representable f => Representable (Co f) where
  type Rep (Co f) = Rep f
  tabulate = Co . tabulate
  index (Co f) i = index f i

instance Representable f => Apply (Co f) where
  (<.>) = apRep

instance Representable f => Applicative (Co f) where
  pure = pureRep
  (<*>) = apRep

instance Representable f => Distributive (Co f) where
  distribute = distributeRep

instance Representable f => Bind (Co f) where
  (>>-) = bindRep

instance Representable f => Monad (Co f) where
  return = pure
  (>>=) = bindRep

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
instance (Representable f, Rep f ~ a) => MonadReader a (Co f) where
  ask = askRep
  local = localRep
#endif

instance (Representable f, Semigroup (Rep f)) => Extend (Co f) where
  extended = extendedRep

instance (Representable f, Monoid (Rep f)) => Comonad (Co f) where
  extend = extendRep
  extract = extractRep

instance ComonadTrans Co where
  lower (Co f) = f

liftR2 :: Representable f => (a -> b -> c) -> f a -> f b -> f c
liftR2 f fa fb = tabulate $ \i -> f (index fa i) (index fb i)

liftR3 :: Representable f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftR3 f fa fb fc = tabulate $ \i -> f (index fa i) (index fb i) (index fc i)
