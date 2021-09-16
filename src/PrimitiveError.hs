module PrimitiveError where

import Obj
import TypeError
import Types

data PrimitiveError
  = ArgumentTypeError String String String XObj
  | ArgumentArityError XObj String [XObj]
  | MissingInfo XObj
  | ForewardImplementsMeta
  | RegisterTypeError
  | SymbolNotFoundError SymPath
  | BadDeftypeMembers
  | QualifiedTypeMember [XObj]
  | InvalidTypeName XObj
  | InvalidTypeVariables XObj
  | MetaSetFailed XObj String
  | StructNotFound XObj
  | NonTypeInTypeEnv SymPath XObj
  | InvalidSumtypeCase XObj
  | TooManySumtypeCases

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
    "`" ++ show (getPath fun) ++ "`" ++ "expected " ++ numberExpected
      ++ " arguments "
      ++ ", but got "
      ++ show (length args)
  show (MissingInfo x) =
    "No information about object: " ++ pretty x
  show ForewardImplementsMeta =
    "Can't set the `implements` meta on a global definition before it is declared."
  show RegisterTypeError =
    "I don't understand this usage of `register-type`.\n\n"
      ++ "Valid usages :\n"
      ++ "  (register-type Name)\n"
      ++ "  (register-type Name [field0 Type, ...])\n"
      ++ "  (register-type Name c-name)\n"
      ++ "  (register-type Name c-name [field0 Type, ...]"
  show (SymbolNotFoundError path) =
    "I can’t find the symbol `" ++ show path ++ "`"
  show (BadDeftypeMembers) =
    "All fields must have a name and a type."
      ++ "Example:\n"
      ++ "```(deftype Name [field1 Type1, field2 Type2, field3 Type3])```\n"
  show (QualifiedTypeMember xobjs) =
    "Type members must be unqualified symbols, but got `"
      ++ concatMap pretty xobjs
      ++ "`"
  show (InvalidTypeName xobj) =
    ("Invalid name for type definition: " ++ pretty xobj)
  show (InvalidTypeVariables xobj) =
    ("Invalid type variables for type definition: " ++ pretty xobj)
  show (MetaSetFailed xobj e) =
    "`meta-set!` failed on `" ++ pretty xobj
      ++ "` "
      ++ show e
  show (StructNotFound xobj) =
    "Couldn't find a type named '" ++ (show (getPath xobj))
      ++ "' in the type environment."
  show (NonTypeInTypeEnv path xobj) =
    "Can't get members for: " ++ show path
      ++ " found a non-type in the type environment: "
      ++ (pretty xobj)
  show (PrimitiveError.InvalidSumtypeCase xobj) =
    "Can't get members for an invalid sumtype case: "
      ++ pretty xobj
  show TooManySumtypeCases =
    "Got too many sumtype cases (>128) for type"

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
