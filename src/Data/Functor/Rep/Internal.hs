{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
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
  , tabulateAlg
  , tabulated
  -- * Logarithms
  , Logarithm(..)
  , contramapLogarithm
  , logarithmRep
  -- * Wrapped representable functors
  , Co(..)
  -- * Default definitions
  -- ** Functor
  , fmapRep
  -- ** Distributive
  , distributeRep
  , collectRep
  , cotraverseRep
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
  -- ** WithIndex
  , imapRep
  , ifoldMapRep
  , itraverseRep
  -- ** Representable
  , tabulateCotraverse1
  , indexLogarithm
  , cotraverse1Iso
  -- ** Generics
  , gcotraverse1
  , GRep
  , gindex
  , gtabulate
  , WrappedRep(..)
  ) where

import Control.Applicative
import Control.Applicative.Backwards
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif
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
import Data.Foldable (Foldable(fold))
import Data.Functor.Bind
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Extend
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor1
import Data.Functor1.Applied
import qualified Data.Monoid as Monoid
import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Semigroup hiding (Product)
import Data.Tagged
import Data.Traversable (Traversable(sequenceA))
import Data.Void
import GHC.Generics hiding (Rep)
import Prelude

-- | A 'Functor' @f@ is 'Representable' if 'tabulate' and 'index' witness an
-- isomorphism to @(->) r@, for some @r@.
--
-- Alternatively, an instance can be derived from 'cotraverse1', by defining:
--
-- @
-- 'Rep' f = 'Logarithm' f
-- 'tabulate' = 'tabulateCotraverse1'
-- 'index' = 'indexLogarithm'
-- @
--
-- Instances without random access should implement 'cotraverse1', as it can
-- allow asymptotically faster zipping.
--
-- Every 'Distributive' 'Functor' is actually 'Representable'.
--
-- Every 'Representable' 'Functor' from Hask to Hask is a right adjoint.
--
-- @
-- 'tabulate' . 'index'  ≡ id
-- 'index' . 'tabulate'  ≡ id
-- 'tabulate' . 'return' ≡ 'return'
--
-- 'distribute1' . 'Applied' ≡ 'fmap' ('Applied' . 'Identity')
-- 'distribute1' ('Const' x) ≡ 'Const' x '<$' xs
--
-- 'cotraverse1' f ≡ 'fmap' f . 'distribute1'
-- 'collect1' f ≡ 'distribute1' . 'map1' f
-- 'cotraverseMap1' f g ≡ 'cotraverse1' f . 'map1' g
-- @

class Distributive f => Representable f where
  -- | If no definition is provided, this will default to 'GRep'.
  type Rep f :: *
  type Rep f = GRep f

  -- |
  -- @
  -- 'fmap' f . 'tabulate' ≡ 'tabulate' . 'fmap' f
  -- @
  --
  -- If no definition is provided, this will default to 'gtabulate'.
  tabulate :: (Rep f -> a) -> f a
  default tabulate :: (Generic1 f, GRep f ~ Rep f, GTabulate (Rep1 f))
                   => (Rep f -> a) -> f a
  tabulate = gtabulate

  -- | If no definition is provided, this will default to 'gindex'.
  index    :: f a -> Rep f -> a
  default index :: (Generic1 f, GRep f ~ Rep f, GIndex (Rep1 f))
                => f a -> Rep f -> a
  index = gindex

  -- | A more powerful version of 'cotraverse'
  --
  -- @'cotraverse1' f = 'fmap' f . 'distribute1'@
  cotraverse1 :: Functor1 w => (w Identity -> a) -> w f -> f a
  cotraverse1 f w = tabulateAlg (\g -> f $ map1Identity g w)

  -- | A more powerful version of 'distribute'
  --
  -- @
  -- 'distribute1' . 'Applied' ≡ 'fmap' ('Applied' . 'Identity')
  -- 'distribute1' ('Const' x) ≡ 'Const' x '<$' xs
  -- @
  distribute1 :: Functor1 w => w f -> f (w Identity)
  distribute1 = cotraverse1 id

  -- | A more powerful version of 'collect'
  --
  -- @'collect1' f ≡ 'distribute1' . 'map1' f@
  collect1 :: Functor1 w => (forall x. g x -> f x) -> w g -> f (w Identity)
  collect1 f = distribute1 . map1 f

  -- | @'cotraverseMap1' f g ≡ 'cotraverse1' f . 'map1' g@
  cotraverseMap1 ::
       Functor1 w => (w Identity -> a) -> (forall x. g x -> f x) -> w g -> f a
  cotraverseMap1 f g = cotraverse1 f . map1 g

tabulateAlg :: Representable f => ((forall x. f x -> x) -> a) -> f a
tabulateAlg f = tabulate $ \i -> f (`index` i)

-- | A default implementation of 'Rep' for a datatype that is an instance of
-- 'Generic1'. This is usually composed of 'Either', tuples, unit tuples, and
-- underlying 'Rep' values. For instance, if you have:
--
-- @
-- data Foo a = MkFoo a (Bar a) (Baz (Quux a)) deriving ('Functor', 'Generic1')
-- instance 'Representable' Foo
-- @
--
-- Then you'll get:
--
-- @
-- 'GRep' Foo = Either () (Either ('WrappedRep' Bar) ('WrappedRep' Baz, 'WrappedRep' Quux))
-- @
--
-- (See the Haddocks for 'WrappedRep' for an explanation of its purpose.)
type GRep f = GRep' (Rep1 f)

-- | A default implementation of 'tabulate' in terms of 'GRep'.
gtabulate :: (Generic1 f, GRep f ~ Rep f, GTabulate (Rep1 f))
          => (Rep f -> a) -> f a
gtabulate = to1 . gtabulate'

-- | A default implementation of 'index' in terms of 'GRep'.
gindex :: (Generic1 f, GRep f ~ Rep f, GIndex (Rep1 f))
       => f a -> Rep f -> a
gindex = gindex' . from1

type family GRep' (f :: * -> *) :: *
class GTabulate f where
  gtabulate' :: (GRep' f -> a) -> f a
class GIndex f where
  gindex' :: f a -> GRep' f -> a

type instance GRep' (f :*: g) = Either (GRep' f) (GRep' g)
instance (GTabulate f, GTabulate g) => GTabulate (f :*: g) where
  gtabulate' f = gtabulate' (f . Left) :*: gtabulate' (f . Right)
instance (GIndex f, GIndex g) => GIndex (f :*: g) where
  gindex' (a :*: _) (Left  i) = gindex' a i
  gindex' (_ :*: b) (Right j) = gindex' b j

type instance GRep' (f :.: g) = (WrappedRep f, GRep' g)
instance (Representable f, GTabulate g) => GTabulate (f :.: g) where
  gtabulate' f = Comp1 $ tabulate $ gtabulate' <$> fmap (curry f) WrapRep
instance (Representable f, GIndex g) => GIndex (f :.: g) where
  gindex' (Comp1 fg) (i, j) = gindex' (index fg (unwrapRep i)) j

type instance GRep' Par1 = ()
instance GTabulate Par1 where
  gtabulate' f = Par1 (f ())
instance GIndex Par1 where
  gindex' (Par1 a) () = a

type instance GRep' (Rec1 f) = WrappedRep f
#if __GLASGOW_HASKELL__ >= 708
-- Using coerce explicitly here seems a bit more readable, and
-- likely a drop easier on the simplifier.
instance Representable f => GTabulate (Rec1 f) where
  gtabulate' = coerce (tabulate :: (Rep f -> a) -> f a)
                 :: forall a . (WrappedRep f -> a) -> Rec1 f a
instance Representable f => GIndex (Rec1 f) where
  gindex' = coerce (index :: f a -> Rep f -> a)
                 :: forall a . Rec1 f a -> WrappedRep f -> a
#else
instance Representable f => GTabulate (Rec1 f) where
  gtabulate' = Rec1 #. tabulate .# (. WrapRep)
instance Representable f => GIndex (Rec1 f) where
  gindex' = (. unwrapRep) #. index .# unRec1
#endif

type instance GRep' (M1 i c f) = GRep' f
instance GTabulate f => GTabulate (M1 i c f) where
  gtabulate' = M1 #. gtabulate'
instance GIndex f => GIndex (M1 i c f) where
  gindex' = gindex' .# unM1

-- | On the surface, 'WrappedRec' is a simple wrapper around 'Rep'. But it plays
-- a very important role: it prevents generic 'Representable' instances for
-- recursive types from sending the typechecker into an infinite loop. Consider
-- the following datatype:
--
-- @
-- data Stream a = a :< Stream a deriving ('Functor', 'Generic1')
-- instance 'Representable' Stream
-- @
--
-- With 'WrappedRep', we have its 'Rep' being:
--
-- @
-- 'Rep' Stream = 'Either' () ('WrappedRep' Stream)
-- @
--
-- If 'WrappedRep' didn't exist, it would be:
--
-- @
-- 'Rep' Stream = Either () (Either () (Either () ...))
-- @
--
-- An infinite type! 'WrappedRep' breaks the potentially infinite loop.
newtype WrappedRep f = WrapRep { unwrapRep :: Rep f }

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

-- * Logarithms

-- | Can be used as a value for 'Rep'
newtype Logarithm f = Logarithm { runLogarithm :: forall x. f x -> x }

contramapLogarithm :: (forall x. f x -> g x) -> Logarithm g -> Logarithm f
contramapLogarithm f (Logarithm g) = Logarithm (g . f)

-- | An index is equivalent to a function which gets the element at that index.
--
-- This can be used with the combinators from the @lens@ package.
--
-- @'logarithmRep' :: 'Representable' f => 'Iso' ('Logarithm' f) ('Rep' f)@
logarithmRep ::
  (Representable f, Representable g, Profunctor p, Functor h) =>
  p (Rep f) (h (Rep g)) -> p (Logarithm f) (h (Logarithm g))
logarithmRep =
  dimap (\(Logarithm f) -> f askRep) (fmap (\x -> Logarithm (`index` x)))

-- * Default definitions

fmapRep :: Representable f => (a -> b) -> f a -> f b
fmapRep f = cotraverse1 (f . runIdentity . runApplied) . Applied

pureRep :: Representable f => a -> f a
pureRep = tabulate . const

bindRep :: Representable f => f a -> (a -> f b) -> f b
bindRep m f = distribute f `apRep` m

mfixRep :: Representable f => (a -> f a) -> f a
mfixRep = cotraverseRep fix

data PairOf a b f = PairOf (f a) (f b)
instance Functor1 (PairOf a b) where
  map1 f (PairOf x y) = PairOf (f x) (f y)

mzipWithRep :: Representable f => (a -> b -> c) -> f a -> f b -> f c
mzipWithRep f as bs =
  cotraverse1 (\(PairOf (Identity a) (Identity b)) -> f a b) (PairOf as bs)

mzipRep :: Representable f => f a -> f b -> f (a, b)
mzipRep = mzipWithRep (,)

askRep :: Representable f => f (Rep f)
askRep = tabulate id

localRep :: Representable f => (Rep f -> Rep f) -> f a -> f a
localRep f m = tabulate (index m . f)

apRep :: Representable f => f (a -> b) -> f a -> f b
apRep = mzipWithRep ($)

distributeRep :: (Representable f, Functor w) => w (f a) -> f (w a)
distributeRep = cotraverseRep id

collectRep :: (Representable f, Functor w) => (a -> f b) -> w a -> f (w b)
collectRep f = distributeRep . fmap f

newtype Composed g a f = Composed { runComposed :: g (f a) }
instance Functor g => Functor1 (Composed g a) where
  map1 f = Composed . fmap f . runComposed

cotraverseRep :: (Representable f, Functor w) => (w a -> b) -> w (f a) -> f b
cotraverseRep f = cotraverse1 (f . fmap runIdentity . runComposed) . Composed

duplicateRepBy :: Representable f => (Rep f -> Rep f -> Rep f) -> f a -> f (f a)
duplicateRepBy plus w = tabulate (\m -> tabulate (index w . plus m))

extendRepBy :: Representable f => (Rep f -> Rep f -> Rep f) -> (f a -> b) -> f a -> f b
extendRepBy plus f w = tabulate (\m -> f (tabulate (index w . plus m)))

extractRepBy :: Representable f => Rep f -> f a -> a
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

imapRep :: Representable r => (Rep r -> a -> b) -> r a -> r b
imapRep f = mzipWithRep f askRep

ifoldMapRep :: forall r m a. (Representable r, Foldable r, Monoid m)
            => (Rep r -> a -> m) -> (r a -> m)
ifoldMapRep ix = fold . imapRep ix

itraverseRep :: forall r f a a'. (Representable r, Traversable r, Applicative f)
             => (Rep r -> a -> f a') -> (r a -> f (r a'))
itraverseRep ix = sequenceA . imapRep ix

newtype TabulateArg a f = TabulateArg (Logarithm f -> a)
instance Functor1 (TabulateArg a) where
  map1 f (TabulateArg g) = TabulateArg (g . contramapLogarithm f)

-- | Derive 'tabulate' given @'Rep' f ~ 'Logarithm' f@ and an
-- implementation of 'cotraverse1'
tabulateCotraverse1 :: Representable f => (Logarithm f -> a) -> f a
tabulateCotraverse1 =
  cotraverse1 (\(TabulateArg g) -> g (Logarithm runIdentity)) . TabulateArg

-- | Derive 'index', given @'Rep' f ~ 'Logarithm' f@
indexLogarithm :: f a -> Logarithm f -> a
indexLogarithm = flip runLogarithm

-- | Derive 'cotraverse1' via an isomorphism
cotraverse1Iso ::
     (Representable g, Functor1 w)
  => (forall x. f x -> g x)
  -> (forall x. g x -> f x)
  -> (w Identity -> a)
  -> w f
  -> f a
cotraverse1Iso t frm f = frm . cotraverseMap1 f t

gcotraverse1 ::
     (Representable (Rep1 f), Functor1 w, Generic1 f)
  => (w Identity -> a)
  -> w f
  -> f a
gcotraverse1 = cotraverse1Iso from1 to1

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
  index = index .# runIdentityT
  tabulate = IdentityT #. tabulate
  cotraverse1 = cotraverse1Iso runIdentityT IdentityT

instance Representable ((->) e) where
  type Rep ((->) e) = e
  index = id
  tabulate = id

instance Representable m => Representable (ReaderT e m) where
  type Rep (ReaderT e m) = (e, Rep m)
  index (ReaderT f) (e,k) = index (f e) k
  tabulate = ReaderT . fmap tabulate . curry
  cotraverse1 = cotraverse1Iso (Comp1 . runReaderT) (ReaderT . unComp1)

instance (Representable f, Representable g) => Representable (Compose f g) where
  type Rep (Compose f g) = (Rep f, Rep g)
  index (Compose fg) (i,j) = index (index fg i) j
  tabulate = Compose . tabulate . fmap tabulate . curry
  cotraverse1 = cotraverse1Iso (Comp1 . getCompose) (Compose . unComp1)

instance Representable w => Representable (TracedT s w) where
  type Rep (TracedT s w) = (s, Rep w)
  index (TracedT w) (e,k) = index w k e
  tabulate = TracedT . unCo . collect (Co #. tabulate) . curry
  cotraverse1 = cotraverse1Iso (Comp1 . runTracedT) (TracedT . unComp1)

instance (Representable f, Representable g) => Representable (Product f g) where
  type Rep (Product f g) = Either (Rep f) (Rep g)
  index (Pair a _) (Left i)  = index a i
  index (Pair _ b) (Right j) = index b j
  tabulate f = Pair (tabulate (f . Left)) (tabulate (f . Right))
  cotraverse1 = cotraverse1Iso (\(Pair x y) -> x :*: y) (\(x :*: y) -> Pair x y)

instance Representable f => Representable (Cofree f) where
  type Rep (Cofree f) = Seq (Rep f)
  index (a :< as) key = case Seq.viewl key of
      Seq.EmptyL -> a
      k Seq.:< ks -> index (index as k) ks
  tabulate f = f Seq.empty :< tabulate (\k -> tabulate (f . (k Seq.<|)))

  -- this could be derived via isomorphism to
  -- Identity :*: (f :.: Cofree f), but then the instance would be
  -- recursive which would prevent specialization
  cotraverse1 f = go
    where
      go w =
        f (map1Identity extract w) :<
        cotraverse1
          (go . map1 (runIdentity . unComp1) . runAppCompose)
          (AppCompose $ map1 (Comp1 . unwrap) w)

instance Representable f => Representable (Backwards f) where
  type Rep (Backwards f) = Rep f
  index = index .# forwards
  tabulate = Backwards #. tabulate
  cotraverse1 = cotraverse1Iso forwards Backwards

instance Representable f => Representable (Reverse f) where
  type Rep (Reverse f) = Rep f
  index = index .# getReverse
  tabulate = Reverse #. tabulate
  cotraverse1 = cotraverse1Iso getReverse Reverse

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

instance Representable U1 where
  type Rep U1 = Void
  index U1 = absurd
  tabulate _ = U1

instance (Representable f, Representable g) => Representable (f :*: g) where
  type Rep (f :*: g) = Either (Rep f) (Rep g)
  index (a :*: _) (Left  i) = index a i
  index (_ :*: b) (Right j) = index b j
  tabulate f = tabulate (f . Left) :*: tabulate (f . Right)
  cotraverse1 f w =
    cotraverseMap1 f (\(a :*: _) -> a) w :*: cotraverseMap1 f (\(_ :*: b) -> b) w

newtype AppCompose w g f = AppCompose { runAppCompose :: w (f :.: g) }
instance Functor1 w => Functor1 (AppCompose w g) where
  map1 f = AppCompose . map1 (Comp1 . f . unComp1) . runAppCompose

instance (Representable f, Representable g) => Representable (f :.: g) where
  type Rep (f :.: g) = (Rep f, Rep g)
  index (Comp1 fg) (i, j) = index (index fg i) j
  tabulate = Comp1 . tabulate . fmap tabulate . curry
  cotraverse1 f w =
    Comp1 $
    cotraverse1 (cotraverseMap1 f (runIdentity . unComp1) . runAppCompose) $
    AppCompose w

instance Representable Par1 where
  type Rep Par1 = ()
  index (Par1 a) () = a
  tabulate f = Par1 (f ())

instance Representable f => Representable (Rec1 f) where
  type Rep (Rec1 f) = Rep f
  index = index .# unRec1
  tabulate = Rec1 #. tabulate
  cotraverse1 = cotraverse1Iso unRec1 Rec1

instance Representable f => Representable (M1 i c f) where
  type Rep (M1 i c f) = Rep f
  index = index .# unM1
  tabulate = M1 #. tabulate
  cotraverse1 = cotraverse1Iso unM1 M1

newtype Co f a = Co { unCo :: f a } deriving Functor

instance Representable f => Representable (Co f) where
  type Rep (Co f) = Rep f
  tabulate = Co #. tabulate
  index = index .# unCo
  cotraverse1 = cotraverse1Iso unCo Co

instance Representable f => Apply (Co f) where
  (<.>) = apRep

instance Representable f => Applicative (Co f) where
  pure = pureRep
  (<*>) = apRep

instance Representable f => Distributive (Co f) where
  distribute = distributeRep
  collect = collectRep

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
liftR2 = mzipWithRep

data TripleOf a b c f = TripleOf (f a) (f b) (f c)
instance Functor1 (TripleOf a b c) where
  map1 f (TripleOf a b c) = TripleOf (f a) (f b) (f c)

liftR3 :: Representable f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftR3 f fa fb fc =
  cotraverse1
    (\(TripleOf (Identity a) (Identity b) (Identity c)) -> f a b c)
    (TripleOf fa fb fc)
