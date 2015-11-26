{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706)
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Rep.TH
-- Copyright   :  (C) 2012-13 Edward Kmett, Ben Gamari
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Functor.Rep.TH
  (
  -- * Generating 'Representable' instances
    deriveRep
  , deriveRepWith
  , defaultRepRules
  , RepRules
  ) where

import Data.Functor.Rep
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Typeable (Typeable)
import Data.Char (toUpper)
import Data.List (foldl')

type RepRules = Con -> [String]

-- | Generate @Representable@ instance and representation type for given type
--
-- /e.g./
-- 
-- @
-- data A a = A { foo, bar, baz :: a }
-- 'deriveRep' ''A
-- @
--
-- will create
-- 
-- @
-- data ARep = AFoo | ABar | ABaz
--           deriving (Show, Read, Ord, Eq, Typeable)
--
-- instance 'Representable' A where
--   type 'Rep' = ARep
--   'tabulate' = ...
--   'index' = ...
-- @
-- 
-- For a type with unnamed fields,
--
-- @
-- data B a = B a a
-- @
-- 
-- 'deriveRep' will produce a representation with numeric field names,
-- 
-- @
-- data BRep = BField1 | BField2
-- @
--
-- For a type operator,
-- 
-- @
-- data Op a = a :@: a
-- @
--
-- @deriveRep@ will produce a representation,
-- 
-- @
-- data OpRep = OpFieldL | OpFieldR
-- @
deriveRep :: Name -> Q [Dec]
deriveRep = deriveRepWith defaultRepRules

deriveRepWith :: RepRules -> Name -> Q [Dec]
deriveRepWith rules name = do
    inf <- reify name        
    case inf of
      TyConI decl -> deriveRepForDec rules decl
      _           -> fail "makeRep: Expected the name of a data type or newtype"
      
deriveRepForDec :: RepRules -> Dec -> Q [Dec]
deriveRepForDec rules (NewtypeD _ name _ con _) =
  deriveRepForCon rules name con
deriveRepForDec rules (DataD _ name _ cons _)
  | [con] <- cons = deriveRepForCon rules name con
  | otherwise     = fail "deriveRepForDec: Can only derive Representable for single-constructor types"
deriveRepForDec rules dec = fail $ "deriveRepForDec: Unsupported declaration: "++show dec

getConName :: Con -> Name
getConName (NormalC name _) = name
getConName (RecC name _) = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con
 
-- | Make a @Representable@ instance for a single-constructor type
deriveRepForCon :: RepRules -> Name -> Con -> Q [Dec]
deriveRepForCon rules tyName con =
  deriveRepForConFields rules tyName (getConName con) (rules con)

makeTabulate :: Name -> [Name] -> Q Dec
makeTabulate conName repConNames = do
  f <- newName "f"
  let body = foldl' (\e con->AppE e (AppE (VarE f) (ConE con)))
                    (ConE conName)
                    repConNames
  return $ FunD 'tabulate [ Clause [VarP f] (NormalB body) [] ]

makeIndex :: Name -> [Name] -> Q Dec
makeIndex conName repConNames = do
  f <- newName "f"          
  r <- newName "r"
  x <- newName "x"
  let patterns initial = pat : rest
        where
          pat = take (length repConNames) $ initial ++ [VarP x] ++ repeat WildP
          rest = patterns (WildP:initial)
      matches = map (ConP conName) (patterns [])
  let clauses = zipWith (\pat con->Clause [pat, ConP con []] (NormalB $ VarE x) [])
                matches repConNames
  return $ FunD 'index clauses

deriveRepForConFields :: RepRules -> Name -> Name -> [String] -> Q [Dec]
deriveRepForConFields rules tyName conName fields = do
  let repTyName = mkName $ nameBase tyName++"Rep"
      repConNames = map (\fldName->mkName $ nameBase tyName++upperHead fldName) fields
  let repCons = map (\name->NormalC name []) repConNames
  tabulateDec <- makeTabulate conName repConNames
  indexDec <- makeIndex conName repConNames
  return [ DataD [] repTyName [] repCons [''Show, ''Read, ''Ord, ''Eq, ''Typeable]
         , InstanceD [] (AppT (ConT ''Representable) (ConT tyName))
           [ tySynInstD' ''Rep [ConT tyName] (ConT repTyName)
           , tabulateDec
           , indexDec
           ]
         ]
  where
    upperHead [] = []
    upperHead (x:xs) = toUpper x:xs

tySynInstD' :: Name -> [Type] -> Type -> Dec
#if MIN_VERSION_template_haskell(2,9,0)
tySynInstD' name tys ty = TySynInstD name (TySynEqn tys ty)
#else
tySynInstD' = TySynInstD
#endif

defaultRepRules :: RepRules
defaultRepRules con =
  case con of
    NormalC conName fields -> zipWith (\n _->"field"++show n) [0::Integer ..] fields
    RecC conName fields    -> [nameBase name | (name,_,_) <- fields]
    InfixC _ conName _     -> ["fieldL", "fieldR"]
    ForallC _ _ con'       -> defaultRepRules con'
