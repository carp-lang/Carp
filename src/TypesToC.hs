module TypesToC
  ( tyToC,
    tyToCLambdaFix,
    tyToCRawFunctionPtrFix,
  )
where

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
tyToCManglePtr _ IntTy = "int"
tyToCManglePtr _ BoolTy = "bool"
tyToCManglePtr _ FloatTy = "float"
tyToCManglePtr _ DoubleTy = "double"
tyToCManglePtr _ LongTy = "Long"
tyToCManglePtr _ ByteTy = "uint8_t"
tyToCManglePtr _ StringTy = "String"
tyToCManglePtr _ PatternTy = "Pattern"
tyToCManglePtr _ CharTy = "Char"
tyToCManglePtr _ UnitTy = "void"
tyToCManglePtr _ (VarTy x) = x
tyToCManglePtr _ (FuncTy argTys retTy _) = "Fn__" ++ joinWithUnderscore (map (tyToCManglePtr True) argTys) ++ "_" ++ tyToCManglePtr True retTy
tyToCManglePtr _ ModuleTy = error "Can't emit module type."
tyToCManglePtr b (PointerTy p) = tyToCManglePtr b p ++ (if b then mangle "*" else "*")
tyToCManglePtr b (RefTy r _) = tyToCManglePtr b r ++ (if b then mangle "*" else "*")
tyToCManglePtr _ (StructTy s []) = tyToCManglePtr False s
tyToCManglePtr _ (StructTy s typeArgs) = tyToCManglePtr False s ++ "__" ++ joinWithUnderscore (map (tyToCManglePtr True) typeArgs)
tyToCManglePtr _ (ConcreteNameTy name) = mangle name
tyToCManglePtr _ TypeTy = error "Can't emit the type of types."
tyToCManglePtr _ MacroTy = error "Can't emit the type of macros."
tyToCManglePtr _ DynamicTy = error "Can't emit the type of dynamic functions."
