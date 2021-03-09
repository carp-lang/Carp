-- | Module EvalError defines errors that may occur during evaluation.
module EvalError
  ( EvaluationError (..),
    throwErr,
    defnInvalidArgs,
    loadInvalidArgs,
    loadOnceInvalidArgs,
    withInvalidArgs,
    setInvalidArgs,
  )
where

import Info
import Obj
import SymPath
import TypeError
import Util
import Types

--------------------------------------------------------------------------------
-- Data (Evaluation Errors)

-- | Errors that occur when evaluating forms.
-- Mostly called from the evaluator.
data EvaluationError
  = SymbolNotFound SymPath
  | PrivateBinding SymPath
  | -- If
    IfContainsNonBool XObj
  | IfMalformed XObj
  | -- Defn
    DefnContainsQualifiedArgs XObj
  | DefnIdentifierIsQualified XObj
  | DefnMalformed XObj
  | -- Def
    DefIdentifierIsQualified XObj
  | -- The
    TheMalformed XObj
  | -- Let
    LetUnevenForms XObj
  | LetMalformedIdentifiers [XObj]
  | -- Fn
    FnContainsQualifiedArgs XObj
  | -- Do
    DoMissingForms
  | -- Unknown Form
    UnknownForm XObj
  | -- Function/Macro Application
    MacroBadArgumentSplit [String]
  | -- Address
    AddressContainsNonSymbol XObj
  | -- While
    WhileContainsNonBool XObj
  | -- Defmodule
    ModuleRedefinition String
  | DefmoduleContainsNonSymbol XObj
  | DefmoduleNoArgs
  | LoadFileNotFound String
  | LoadGitFailure String String
  | LoadRecursiveLoad String
  | -- Defndynamic
    DefnDynamicInvalidName XObj
  | -- Set!
    SetVarNotFound XObj
  | SetInvalidVarName XObj
  | SetTypeMismatch SymPath Ty Ty
  | -- Static Call
    StaticCall XObj
  | -- Invalid Arguments
    InvalidArgs String [XObj]

-- Show instance for Evaluation Errors.
instance Show EvaluationError where
  show (SymbolNotFound path) = "Can't find symbol '" ++ show path ++ "'"
  show (PrivateBinding path) =
    "The binding: " ++ show path ++ " is private; it may only be used within the module that defines it."
  show (IfContainsNonBool cond) =
    "This `if` condition contains the non-boolean value `"
      ++ pretty cond
      ++ "`"
  show (IfMalformed xobj) =
    "I didn’t understand this `if`.\n\n Got:\n```\n" ++ pretty xobj
      ++ "\n```\n\nExpected the form:\n```\n(if cond then else)\n```\n"
  show (DefnContainsQualifiedArgs args) =
    "`defn` requires all arguments to be unqualified symbols, but it got `"
      ++ pretty args
      ++ "`"
  show (DefnIdentifierIsQualified name) =
    "`defn` identifiers must be unqualified symbols, but it got `"
      ++ pretty name
      ++ "`"
  show (DefnMalformed xobj) =
    "I didn’t understand the `defn` at " ++ prettyInfoFromXObj xobj
      ++ ":\n\n"
      ++ pretty xobj
      ++ "\n\nIs it valid? Every `defn` needs to follow the form `(defn name [arg] body)`."
  show (DefIdentifierIsQualified name) =
    "`def` identifiers must be unqualified symbols, but it got `"
      ++ pretty name
      ++ "`"
  show (TheMalformed xobj) =
    "I didn’t understand the `the` at " ++ prettyInfoFromXObj xobj
      ++ ":\n\n"
      ++ pretty xobj
      ++ "\n\nIs it valid? Every `the` needs to follow the form `(the type expression)`."
  show (LetUnevenForms xobj) = "Uneven number of forms in `let`: " ++ pretty xobj
  show (LetMalformedIdentifiers bindings) =
    "`let` identifiers must be symbols, but it got `"
      ++ joinWithSpace (map pretty bindings)
      ++ "`"
  show (FnContainsQualifiedArgs args) = "`fn` requires all arguments to be unqualified symbols, but it got `" ++ pretty args ++ "`"
  show DoMissingForms = "No forms in do"
  show (UnknownForm xobj) = "I did not understand the form `" ++ pretty xobj ++ "`"
  show (MacroBadArgumentSplit allParams) =
    "I didn’t understand this macro’s argument split, got `"
      ++ joinWith "," allParams
      ++ "`, but expected exactly one `:rest` separator."
  show (AddressContainsNonSymbol xobj) = "Can't get the address of non-symbol " ++ pretty xobj
  show (WhileContainsNonBool c) =
    "This `while` condition contains the non-boolean value '"
      ++ pretty c
      ++ "`"
  show (ModuleRedefinition name) = "Can't redefine '" ++ name ++ "' as module"
  show (DefmoduleContainsNonSymbol x) = "`defmodule` expects a symbol, got '" ++ pretty x ++ "' instead."
  show DefmoduleNoArgs = "`defmodule` requires at least a symbol, received none."
  show (LoadFileNotFound path) = " I can't find a file named: '" ++ path ++ "'" ++ "\n\nIf you tried loading an external package, try appending a version string (like `@master`)"
  show (LoadGitFailure path stderr) =
    "I can't find a file named: '" ++ path ++ "'"
      ++ "\n\nI tried interpreting the statement as a git import, but got: "
      ++ stderr
  show (LoadRecursiveLoad path) = "A file can't load itself: '" ++ path ++ "'"
  show (DefnDynamicInvalidName notName) = "`defndynamic` expected a name as first argument, but got " ++ pretty notName
  show (SetVarNotFound var) = "I couldn't find the variable " ++ pretty var ++ ", did you define it using `def` or `defdynamic`?"
  show (SetInvalidVarName var) = "`set!` expected a name as first argument, but got " ++ pretty var
  show (SetTypeMismatch path oldTy newTy) = "can't `set!` " ++ show path ++ " to a value of type " ++ show oldTy ++ ", " ++ show path ++ " has type " ++ show newTy
  show (StaticCall x) = "Unexpected static call in " ++ pretty x
  show (InvalidArgs expected actual) = expected ++ ", but got: " ++ joinWithComma (map pretty actual)

--------------------------------------------------------------------------------
-- Utilities

-- | Given a Showable error, turn it into an EvalError
-- TODO: Unify this and toEvalError and remove one of these functions.
throwErr :: Show a => a -> Context -> Maybe Info -> (Context, Either EvalError XObj)
throwErr err ctx info =
  evalError ctx (show err) info

--------------------------------------------------------------------------------
-- Invalid Argument Helpers

defnInvalidArgs :: [XObj] -> EvaluationError
defnInvalidArgs = InvalidArgs "Invalid args to `defn`, expected an array of symbols as an argument list"

loadInvalidArgs :: [XObj] -> EvaluationError
loadInvalidArgs = InvalidArgs  "Invalid args to `load`, expected (load str optional:fileFromRepo)"

loadOnceInvalidArgs :: [XObj] -> EvaluationError
loadOnceInvalidArgs = InvalidArgs "Invalid args to `load-once`, expected `(load-once str optional:fileFromRepo)`"

withInvalidArgs :: [XObj] -> EvaluationError
withInvalidArgs = InvalidArgs "Invalid arguments to `with`, expected a module name"

setInvalidArgs :: [XObj] -> EvaluationError
setInvalidArgs = InvalidArgs "Invalid arguments to `set!`, expected a symbol and a value"
