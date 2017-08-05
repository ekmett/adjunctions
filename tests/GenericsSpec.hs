{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
----------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2011-2014
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
--
-- Tests for generically derived 'Representable' instances.
----------------------------------------------------------------------
module GenericsSpec (main, spec) where

import           Data.Distributive (Distributive(..))
import           Data.Functor.Rep (Representable(..), WrappedRep(..))

#if __GLASGOW_HASKELL__ >= 706
import           Generics.Deriving.Base hiding (Rep)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll1)
#endif

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Id" $
    itIndexes "idExample" "idRep" idExample idRep 42
  describe "Stream" $ do
    let streamIndexes :: String -> Rep Stream -> Int -> Spec
        streamIndexes repNum = itIndexes "streamExample" ("streamRep" ++ repNum) streamExample
    streamIndexes "1" streamRep1 0
    streamIndexes "2" streamRep2 1
    streamIndexes "3" streamRep3 2
  describe "PolyRec" $ do
    let polyRecIndexes :: String -> Rep PolyRec -> Int -> Spec
        polyRecIndexes repNum = itIndexes "polyRecExample" ("polyRecRep" ++ repNum)
                                          polyRecExample
    polyRecIndexes "1" polyRecRep1 1
    polyRecIndexes "2" polyRecRep2 2
    polyRecIndexes "3" polyRecRep3 0

itIndexes :: (Eq a, Representable f, Show a)
          => String -> String -> f a -> Rep f -> a -> Spec
itIndexes exampleStr repStr exampleVal rep res =
  it ("index " ++ exampleStr ++ " " ++ repStr ++ " = " ++ show res) $
    index exampleVal rep `shouldBe` res

-------------------------------------------------------------------------------

newtype Id a = Id { runId :: a }
  deriving Functor
instance Distributive Id where
  collect f  = Id . fmap (runId . f)
  distribute = Id . fmap runId
instance Representable Id
  -- type Rep Id = ()

idExample :: Id Int
idExample = Id 42

idRep :: Rep Id
idRep = ()

data Stream a = (:>) { shead :: a, stail :: Stream a }
  deriving Functor
instance Distributive Stream where
  distribute w = fmap shead w :> distribute (fmap stail w)
instance Representable Stream
  -- type Rep Stream = Either () (WrappedRep Stream)

streamExample :: Stream Int
streamExample = let s = 0 :> fmap (+1) s in s

streamRep1, streamRep2, streamRep3 :: Rep Stream
streamRep1 = Left ()
streamRep2 = Right $ WrapRep $ Left ()
streamRep3 = Right $ WrapRep $ Right $ WrapRep $ Left ()

data PolyRec a = PolyRec (Id (PolyRec a)) a
  deriving Functor
instance Distributive PolyRec where
  distribute fpa = PolyRec (Id $ distribute fpa) (fmap (\(PolyRec _ a) -> a) fpa)
instance Representable PolyRec
  -- type Rep PolyRec = Either (WrappedRep Id, WrappedRep PolyRec) ()

polyRecExample :: PolyRec Int
polyRecExample = let p = PolyRec (Id (fmap (+1) p)) 0 in p

polyRecRep1, polyRecRep2, polyRecRep3 :: Rep PolyRec
polyRecRep1 = Left (WrapRep (), WrapRep $ Right ())
polyRecRep2 = Left (WrapRep (), WrapRep $ Left (WrapRep (), WrapRep $ Right ()))
polyRecRep3 = Right ()

#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic1 Id
deriving instance Generic1 Stream
deriving instance Generic1 PolyRec
#else
$(Generics.deriveAll1 ''Id)
$(Generics.deriveAll1 ''Stream)
$(Generics.deriveAll1 ''PolyRec)
#endif
