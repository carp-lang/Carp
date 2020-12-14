module PrimitiveError where

import Obj
import Types
import TypeError

data PrimitiveError
  = ArgumentTypeError String String String XObj
  | ArgumentArityError XObj String [XObj] 
  | MissingInfo XObj
  | ForewardImplementsMeta
  | RegisterTypeError 
  | SymbolNotFoundError SymPath

data PrimitiveWarning 
  = NonExistentInterfaceWarning XObj
  | DefinitionTypeChangeWarning XObj Ty

instance Show PrimitiveError where
  show (ArgumentTypeError fun ty position actual) =
     "`" ++ fun ++ "` expected " ++ ty ++ " as its " ++ position
         ++ " argument, but got `"
         ++ pretty actual
         ++ "`"
  show (ArgumentArityError fun numberExpected args) =
    "`" ++ (show (getPath fun)) ++ "`" ++ "expected " ++ numberExpected 
        ++ " arguments " ++ ", but got " ++ show (length args)
  show (MissingInfo x) =
    "No information about object: " ++ pretty x
  show (ForewardImplementsMeta) = 
    "Can't set the `implements` meta on a global definition before it is declared."
  show (RegisterTypeError) =
    "I don't understand this usage of `register-type`.\n\n"
    ++ "Valid usages :\n"
    ++ "  (register-type Name)\n"
    ++ "  (register-type Name [field0 Type, ...])\n"
    ++ "  (register-type Name c-name)\n"
    ++ "  (register-type Name c-name [field0 Type, ...]"
  show (SymbolNotFoundError path) =
    "I can’t find the symbol `" ++ show path ++ "`"   

instance Show PrimitiveWarning where
  show (NonExistentInterfaceWarning x) =
    "The interface "
    ++ show (getPath x)
    ++ " is not defined."
    ++ " Did you define it using `definterface`?"
  show (DefinitionTypeChangeWarning annXObj previousType) = 
    "Definition at " ++ prettyInfoFromXObj annXObj ++ " changed type of '" ++ show (getPath annXObj)
    ++ "' from "
    ++ show previousType
    ++ " to "
    ++ show (forceTy annXObj)

toEvalError :: Context -> XObj -> PrimitiveError -> (Context, Either EvalError XObj)
toEvalError ctx xobj perr =
  evalError ctx (show perr) (xobjInfo xobj)
