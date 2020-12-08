{-# LANGUAGE FlexibleInstances #-}

-- | Module Reify provides a typeclass and instances for turning internal compiler types and data into
-- corresponding representations in the Carp language.
module Reify where

import Info
import Obj
import System.FilePath
import Types

-- | The Reifiable class ranges over internal Carp compiler types that
-- may have corresponding representations in Carp itself.
class Reifiable a where
  reify :: a -> XObj

symbol :: Show a => a -> XObj
symbol x = XObj (Sym (SymPath [] (show x)) Symbol) Nothing Nothing

-- Show on strings results in a symbol that includes quotes ""
-- This function is the same as symbol, for string literals.
literal :: String -> XObj
literal x = XObj (Sym (SymPath [] x) Symbol) Nothing Nothing

array :: (Reifiable a) => [a] -> XObj
array x = XObj (Arr (map reify x)) Nothing Nothing

lifetime :: Show a => a -> XObj
lifetime x = literal ("<" ++ show x ++ ">")

-- Types
instance Reifiable Kind where
  reify k = symbol k

instance Reifiable Ty where
  reify (StructTy t []) = reify t
  reify (StructTy t vs) = XObj (Lst (reify t : map reify vs)) Nothing (Just TypeTy)
  reify (RefTy t lt) = XObj (Lst [literal "Ref", reify t, lifetime lt]) Nothing (Just TypeTy)
  reify (PointerTy t) = XObj (Lst [literal "Ptr", reify t]) Nothing (Just TypeTy)
  reify (FuncTy ats rt lt) = XObj (Lst [literal "Fn", array ats, reify rt, lifetime lt]) Nothing (Just TypeTy)
  reify TypeTy = XObj (Sym (SymPath [] (show TypeTy)) Symbol) Nothing (Just Universe)
  reify UnitTy = XObj (Sym (SymPath [] "Unit") Symbol) Nothing (Just TypeTy)
  reify t = XObj (Sym (SymPath [] (show t)) Symbol) Nothing (Just TypeTy)

instance Reifiable String where
  reify s = XObj (Str s) Nothing (Just StringTy)

instance Reifiable Int where
  reify i = XObj (Num IntTy (fromIntegral i)) Nothing (Just IntTy)

getInfoAsXObj :: (Reifiable a) => (Info -> a) -> Maybe Info -> Maybe XObj
getInfoAsXObj f = fmap (reify . f)

getFileAsXObj :: FilePathPrintLength -> Maybe Info -> Maybe XObj
getFileAsXObj FullPath = getInfoAsXObj infoFile
getFileAsXObj ShortPath = getInfoAsXObj (takeFileName . infoFile)

getLineAsXObj :: Maybe Info -> Maybe XObj
getLineAsXObj = getInfoAsXObj infoLine

getColumnAsXObj :: Maybe Info -> Maybe XObj
getColumnAsXObj = getInfoAsXObj infoColumn
