module TypesToC
  ( tyToC,
    tyToCLambdaFix,
    tyToCRawFunctionPtrFix,
  )
where

import Data.List
import Data.Text (pack, splitOn, unpack)
import SymPath
import Types
import Util

tyToC :: Ty -> String
tyToC = tyToCManglePtr False

tyToCLambdaFix :: Ty -> String
tyToCLambdaFix FuncTy {} = "Lambda"
tyToCLambdaFix (RefTy FuncTy {} _) = "Lambda*"
tyToCLambdaFix (RefTy (RefTy FuncTy {} _) _) = "Lambda**"
tyToCLambdaFix (RefTy (RefTy (RefTy FuncTy {} _) _) _) = "Lambda***" -- TODO: More cases needed?! What's a better way to do it..?
tyToCLambdaFix t = tyToCManglePtr False t

tyToCRawFunctionPtrFix :: Ty -> String
tyToCRawFunctionPtrFix FuncTy {} = "void*"
tyToCRawFunctionPtrFix t = tyToCManglePtr False t

tyToCManglePtr :: Bool -> Ty -> String
tyToCManglePtr b (PointerTy p) = tyToCManglePtr b p ++ (if b then mangle "*" else "*")
tyToCManglePtr b (RefTy r _) = tyToCManglePtr b r ++ (if b then mangle "*" else "*")
tyToCManglePtr _ ty = f ty
  where
    f IntTy = "int"
    f BoolTy = "bool"
    f FloatTy = "float"
    f DoubleTy = "double"
    f LongTy = "Long"
    f ByteTy = "uint8_t"
    f StringTy = "String"
    f PatternTy = "Pattern"
    f CharTy = "Char"
    f UnitTy = "void"
    f (VarTy x) = x
    f (FuncTy argTys retTy _) = "Fn__" ++ joinWithUnderscore (map (tyToCManglePtr True) argTys) ++ "_" ++ tyToCManglePtr True retTy
    f (StructTy s []) = tyToCManglePtr False s
    f (StructTy s typeArgs) = tyToCManglePtr False s ++ "__" ++ joinWithUnderscore (map (tyToCManglePtr True) typeArgs)
    f (ConcreteNameTy name) = mangle (intercalate "" (map unpack (splitOn (pack ".") (pack name))))
    f ModuleTy = err "modules"
    f TypeTy = err "types"
    f MacroTy = err "macros"
    f DynamicTy = err "dynamic functions"
    f StaticLifetimeTy = err "lifetimes"
    f InterfaceTy = err "interfaces"
    f Universe = err "universe"
    f (PointerTy _) = err "pointers"
    f (RefTy _ _) = err "references"
    err s = error ("Can't emit the type of " ++ s ++ ".")
