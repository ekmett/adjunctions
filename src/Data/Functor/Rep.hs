{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
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
  , cotraverse1
  , distribute1
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
  , cotraverseMap1Iso
#if __GLASGOW_HASKELL__ >= 708
  -- We only export this "publicly" when we have a safe way
  -- to implement it.
  , cotraverseMap1Coerce
#endif
  -- ** Generics
  , gcotraverseMap1
  , GRep
  , gindex
  , gtabulate
  , WrappedRep(..)
  ) where

import Data.Functor.Rep.Internal
