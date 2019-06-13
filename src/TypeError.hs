module TypeError where

import Data.Maybe (fromMaybe)

import Types
import Obj
import Constraints
import Util
import Lookup

data TypeError = SymbolMissingType XObj Env
               | DefnMissingType XObj
               | DefMissingType XObj
               | ExpressionMissingType XObj
               | SymbolNotDefined SymPath XObj Env
               | InvalidObj Obj XObj
               | CantUseDerefOutsideFunctionApplication XObj
               | NotAType XObj
               | WrongArgCount XObj Int Int
               | NotAFunction XObj
               | NoStatementsInDo XObj
               | TooManyFormsInBody XObj
               | NoFormsInBody XObj
               | LeadingColon XObj
               | UnificationFailed Constraint TypeMappings [Constraint]
               | CantDisambiguate XObj String Ty [(Ty, SymPath)]
               | CantDisambiguateInterfaceLookup XObj String Ty [(Ty, SymPath)]
               | SeveralExactMatches XObj String Ty [(Ty, SymPath)]
               | NoMatchingSignature XObj String Ty [(Ty, SymPath)]
               | HolesFound [(String, Ty)]
               | FailedToExpand XObj EvalError
               | NotAValidType XObj
               | FunctionsCantReturnRefTy XObj Ty
               | LetCantReturnRefTy XObj Ty
               | GettingReferenceToUnownedValue XObj
               | UsingUnownedValue XObj
               | UsingCapturedValue XObj
               | ArraysCannotContainRefs XObj
               | MainCanOnlyReturnUnitOrInt XObj Ty
               | MainCannotHaveArguments XObj Int
               | CannotConcretize XObj
               | TooManyAnnotateCalls XObj
               | CannotSet XObj
               | DoesNotMatchSignatureAnnotation XObj Ty -- Not used at the moment (but should?)
               | CannotMatch XObj
               | InvalidSumtypeCase XObj
               | InvalidMemberType Ty XObj
               | InvalidMemberTypeWhenConcretizing Ty XObj TypeError
               | NotAmongRegisteredTypes Ty XObj
               | UnevenMembers [XObj]
               | InvalidLetBinding [XObj] (XObj, XObj)
               | DuplicateBinding XObj
               | DefinitionsMustBeAtToplevel XObj

instance Show TypeError where
  show (SymbolMissingType xobj env) =
    "I couldn’t find a type for the symbol '" ++ getName xobj ++ "' at " ++
    prettyInfoFromXObj xobj ++ " in the environment:\n" ++
    prettyEnvironment env ++
    "\n\nIt might be too general. You could try adding a type hint using `the`."
  show (DefnMissingType xobj) =
    "I couldn’t find a type for the function definition '" ++ getName xobj ++
    "' at " ++ prettyInfoFromXObj xobj  ++
    ".\n\nIt might be too general. You could try adding a type hint using `the`."
  show (DefMissingType xobj) =
    "I couldn’t find a type for the variable definition '" ++ getName xobj ++
    "' at " ++ prettyInfoFromXObj xobj  ++
    ".\n\nIt might be too general. You could try adding a type hint using `the`."
  show (ExpressionMissingType xobj)=
    "I couldn’t find a type for the expression '" ++ pretty xobj ++ "' at " ++
    prettyInfoFromXObj xobj ++
    ".\n\nIt might be too general. You could try adding a type hint using `the`."
  show (SymbolNotDefined symPath@(SymPath p _) xobj env) =
    "I couldn’t find the symbol '" ++ show symPath ++ "' at " ++
    prettyInfoFromXObj xobj ++ ".\n\n" ++
    matches (keysInEnvEditDistance symPath env 3)
    where matches [] = "Maybe you forgot to define it?"
          matches x = "Maybe you wanted one of the following?\n    " ++ joinWith "\n    " (map (show . SymPath p) x)
  show (InvalidObj Defn xobj) =
    "I didn’t understand the function definition at " ++
    prettyInfoFromXObj xobj ++
    ".\n\nIs it valid?  Every `defn` needs to follow the form `(defn name [arg] body)`."
  show (CantUseDerefOutsideFunctionApplication xobj) =
    "I found a `deref` / `~` that isn’t inside a function application at " ++
    prettyInfoFromXObj xobj ++
    ".\n\nEvery usage of `~` must be inside a function application."
  show (InvalidObj If xobj) =
    "I didn’t understand the `if` statement at " ++ prettyInfoFromXObj xobj ++
    ".\n\nIs it valid? Every `if` needs to follow the form `(if cond iftrue iffalse)`."
  show (InvalidObj o xobj) =
    "I didn’t understand the form `" ++ show o ++ "` at " ++
    prettyInfoFromXObj xobj ++ ".\n\nIs it valid?"
  show (WrongArgCount xobj expected actual) =
    "You used the wrong number of arguments in '" ++ getName xobj ++ "' at " ++
    prettyInfoFromXObj xobj ++ ". I expected " ++ show expected ++
    ", but got " ++ show actual ++ "."
  show (NotAFunction xobj) =
    "You are trying to call the non-function `" ++ getName xobj ++ "` at " ++
    prettyInfoFromXObj xobj ++ "."
  show (NoStatementsInDo xobj) =
    "There are no expressions inside of the `do` statement at " ++
    prettyInfoFromXObj xobj ++
    ".\n\nAll instances of `do` need to have one or more expressions in it."
  show (TooManyFormsInBody xobj) =
    "There are too many expressions in the body of the form at " ++
    prettyInfoFromXObj xobj ++ ".\n\nTry wrapping them in a `do`."
  show (NoFormsInBody xobj) =
    "There are no expressions in the body body of the form at " ++
    prettyInfoFromXObj xobj ++
    ".\n\nI need exactly one body form. For multiple forms, try using `do`."
  show (UnificationFailed constraint@(Constraint a b aObj bObj ctx _) mappings constraints) =
    "I can’t match the types `" ++ show (recursiveLookupTy mappings a) ++
    "` and `" ++ show (recursiveLookupTy mappings b) ++ "`" ++ extra ++
    ".\n\n" ++
    --show aObj ++ "\nWITH\n" ++ show bObj ++ "\n\n" ++
    "  " ++ pretty aObj ++ " : " ++ showTypeFromXObj mappings aObj ++
    "\n  At " ++ prettyInfoFromXObj aObj ++ "" ++
    "\n\n" ++
    "  " ++ pretty bObj ++ " : " ++ showTypeFromXObj mappings bObj ++
    "\n  At " ++ prettyInfoFromXObj bObj ++ "\n"
    -- ++ "Constraint: " ++ show constraint ++ "\n\n"
    -- "All constraints:\n" ++ show constraints ++ "\n\n" ++
    -- "Mappings: \n" ++ show mappings ++ "\n\n"
    where extra = if ctx == aObj || ctx == bObj then "" else " within `" ++ snip (pretty ctx) ++ "`"
          snip s = if length s > 25
                    then take 15 s ++ " ... " ++ drop (length s - 5) s
                    else s
  show (CantDisambiguate xobj originalName theType options) =
    "I found an ambiguous symbol `" ++ originalName ++ "` of type `" ++
    show theType ++ "` at " ++ prettyInfoFromXObj xobj ++
    "\nPossibilities:\n    " ++
    joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (CantDisambiguateInterfaceLookup xobj name theType options) =
    "I found an ambiguous interface `" ++ name ++ "` of type `" ++
    show theType ++ "` at " ++ prettyInfoFromXObj xobj ++
    "\nPossibilities:\n    " ++
    joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (SeveralExactMatches xobj name theType options) =
    "There are several exact matches for the interface `" ++ name ++
    "` of type `" ++ show theType ++ "` at " ++ prettyInfoFromXObj xobj ++
    "\nPossibilities:\n    " ++
    joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (NoMatchingSignature xobj originalName theType options) =
    "I can’t find any implementation for the interface `" ++ originalName ++
    "` of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++
    ".\n\nNone of the possibilities have the correct signature:\n    " ++ joinWith
    "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (LeadingColon xobj) =
    "I found a symbol '" ++ pretty xobj ++ "' that starts with a colon at " ++
    prettyInfoFromXObj xobj ++ ".\n\nThis is disallowed."
  show (HolesFound holes) =
    "I found the following holes:\n\n    " ++
    joinWith "\n    " (map (\(name, t) -> name ++ " : " ++ show t) holes) ++
    "\n"
  show (FailedToExpand xobj err@EvalError{}) =
    "I failed to expand a macro at " ++ prettyInfoFromXObj xobj ++
    ".\n\nThe error message I got was: " ++ show err
  show (NotAValidType xobj) =
    pretty xobj ++ "is not a valid type at " ++ prettyInfoFromXObj xobj
  show (FunctionsCantReturnRefTy xobj t) =
    "Functions can’t return references. " ++ getName xobj ++ " : " ++ show t
    ++ " at " ++ prettyInfoFromXObj xobj ++
    "\n\nYou’ll have to copy the return value using `@`."
  show (LetCantReturnRefTy xobj t) =
    "`let` expressions can’t return references. " ++ pretty xobj ++ " : " ++
    show t ++ " at " ++ prettyInfoFromXObj xobj ++
    "\n\nYou’ll have to copy the return value using `@`."
  show (GettingReferenceToUnownedValue xobj) =
    "You’re referencing a given-away value `" ++ pretty xobj ++ "` at " ++ --"' (expression " ++ freshVar i ++ ") at " ++
    prettyInfoFromXObj xobj ++ "\n" ++ show xobj ++
    "\n\nYou’ll have to copy the value using `@`."
  show (UsingUnownedValue xobj) =
    "You’re using a given-away value `" ++ pretty xobj ++ "` at " ++
    prettyInfoFromXObj xobj ++ ".\n\nYou’ll have to copy the value using `@`."
  show (UsingCapturedValue xobj) =
    "You’re using a value `" ++ pretty xobj ++
    "` that was captured by a function at " ++ prettyInfoFromXObj xobj ++ "."
  show (ArraysCannotContainRefs xobj) =
    "Arrays can’t contain references: `" ++ pretty xobj ++ "` at " ++
    prettyInfoFromXObj xobj ++ ".\n\nYou’ll have to make a copy using `@`."
  show (MainCanOnlyReturnUnitOrInt xobj t) =
    "The main function can only return an `Int` or a unit type (`()`), but it got `" ++
    show t ++ "`."
  show (MainCannotHaveArguments xobj c) =
    "The main function may not receive arguments, but it got " ++ show c ++ "."
  show (CannotConcretize xobj) =
    "I’m unable to concretize the expression '" ++ pretty xobj ++ "' at " ++
    prettyInfoFromXObj xobj ++
    ".\n\nIt might be too general. You could try adding a type hint using `the`."
  show (TooManyAnnotateCalls xobj) =
    "There were too many annotation calls when annotating `" ++ pretty xobj ++
    "` at " ++ prettyInfoFromXObj xobj ++
    ".\n\n I deduced it was an infinite loop."
  show (NotAType xobj) =
    "I don’t understand the type '" ++ pretty xobj ++ "' at " ++
    prettyInfoFromXObj xobj ++ "\n\nIs it defined?"
  show (CannotSet xobj) =
    "I can’t `set!` the expression `" ++ pretty xobj ++ "` at " ++
    prettyInfoFromXObj xobj ++ ".\n\nOnly variables can be reset using `set!`."
  show (DoesNotMatchSignatureAnnotation xobj sigTy) =
    "The definition at " ++ prettyInfoFromXObj xobj ++
    " does not match its annotation provided to `sig` as `" ++ show sigTy ++
    "`, its actual type is `" ++ show (forceTy xobj) ++ "`."
  show (CannotMatch xobj) =
    "I can’t `match` `" ++ pretty xobj ++ "` at " ++ prettyInfoFromXObj xobj ++
    ".\n\nOnly sumtypes can be matched against."
  show (InvalidSumtypeCase xobj) =
    "I failed to read `" ++ pretty xobj ++ "` as a sumtype case at " ++
    prettyInfoFromXObj xobj ++
    ".\n\nSumtype cases look like this: `(Foo [Int typevar])`"
  show (InvalidMemberType t xobj) =
    "I can’t use the type `" ++ show t ++ "` as a member type at " ++
    prettyInfoFromXObj xobj ++
    ".\n\nIs it defined and captured in the head of the type definition?"
  show (InvalidMemberTypeWhenConcretizing t xobj err) =
    "I can’t use the concrete type `" ++ show t ++ "` at " ++ prettyInfoFromXObj xobj ++ ": " ++ show err
  show (NotAmongRegisteredTypes t xobj) =
    "I can’t find a definition for the type `" ++ show t ++ "` at " ++
    prettyInfoFromXObj xobj ++ ".\n\nWas it registered?"
  show (UnevenMembers xobjs) =
    "The number of members and types is uneven: `" ++
    joinWithComma (map pretty xobjs) ++ "` at " ++
    prettyInfoFromXObj (head xobjs) ++
    ".\n\nBecause they are pairs of names and their types, they need to be even.\nDid you forget a name or type?"
  show (InvalidLetBinding xobjs (sym, expr)) =
    "The binding `[" ++ pretty sym ++ " " ++ pretty expr ++ "]` is invalid at " ++
    prettyInfoFromXObj (head xobjs) ++ ". \n\n Binding names must be symbols."

  show (DuplicateBinding xobj) =
    "I encountered a duplicate binding `" ++ pretty xobj ++ "` inside the `let` at " ++ prettyInfoFromXObj xobj ++ "."
  show (DefinitionsMustBeAtToplevel xobj) =
    "I encountered a definition that was not at top level: `" ++ pretty xobj ++ "`"

machineReadableErrorStrings :: FilePathPrintLength -> TypeError -> [String]
machineReadableErrorStrings fppl err =
  case err of
    (UnificationFailed constraint@(Constraint a b aObj bObj _ _) mappings constraints) ->
      [machineReadableInfoFromXObj fppl aObj ++ " Inferred " ++ showTypeFromXObj mappings aObj ++ ", can't unify with " ++ show (recursiveLookupTy mappings b) ++ "."
      ,machineReadableInfoFromXObj fppl bObj ++ " Inferred " ++
       showTypeFromXObj mappings bObj ++ ", can't unify with " ++ show (recursiveLookupTy mappings a) ++ "."]

    (DefnMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Function definition '" ++ getName xobj ++ "' missing type."]
    (DefMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Variable definition '" ++ getName xobj ++ "' missing type."]
    (ExpressionMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Expression '" ++ pretty xobj ++ "' missing type."]
    (SymbolNotDefined symPath xobj _) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Trying to refer to an undefined symbol '" ++ show symPath ++ "'."]
    (SymbolMissingType xobj env) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Symbol '" ++ getName xobj ++ "' missing type."]
    (InvalidObj Defn xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Invalid function definition."]
    (InvalidObj If xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Invalid if-statement."]
    (InvalidObj o xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Invalid obj '" ++ show o ++ "'."]
    (CantUseDerefOutsideFunctionApplication xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't use 'deref' / '~' outside function application."]
    (WrongArgCount xobj expected actual) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Wrong argument count in call to '" ++ getName xobj ++ "' (expected " ++ show expected ++ ", received " ++ show actual ++ ")."]
    (NotAFunction xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Trying to call non-function '" ++ getName xobj ++ "'."]
    (NoStatementsInDo xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " The do-statement has no expressions inside of it."]
    (TooManyFormsInBody xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Too many expressions in body position."]
    (NoFormsInBody xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " No expressions in body position."]

    (CantDisambiguate xobj originalName theType options) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't disambiguate symbol '" ++ originalName ++ "' of type " ++ show theType ++
       "\nPossibilities:\n    " ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)]
    (CantDisambiguateInterfaceLookup xobj name theType options) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't disambiguate interface lookup symbol '" ++ name ++ "' of type " ++ show theType ++
       "\nPossibilities:\n    " ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)]
    (SeveralExactMatches xobj name theType options) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Several exact matches for interface lookup symbol '" ++ name ++ "' of type " ++ show theType ++ "\nPossibilities:\n    " ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)]
    (NoMatchingSignature xobj originalName theType options) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't find matching lookup for symbol '" ++ originalName ++ "' of type " ++ show theType ++
       "\nNone of the possibilities have the correct signature:\n    " ++ joinWith
       "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)]

    (LeadingColon xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Symbol '" ++ pretty xobj ++ "' starting with a colon (reserved for REPL shortcuts)."]

    -- (HolesFound holes) ->
    --   (map (\(name, t) -> machineReadableInfoFromXObj fppl xobj ++ " " ++ name ++ " : " ++ show t) holes)

    (FailedToExpand xobj (EvalError errorMessage _ _)) ->
      [machineReadableInfoFromXObj fppl xobj ++ "Failed to expand: " ++ errorMessage]

    -- TODO: Remove overlapping errors:
    (NotAValidType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Not a valid type: " ++ pretty xobj ++ "."]
    (NotAType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't understand the type '" ++ pretty xobj ++ "'."]

    (FunctionsCantReturnRefTy xobj t) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Functions can't return references. " ++ getName xobj ++ " : " ++ show t ++ "."]
    (LetCantReturnRefTy xobj t) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Let-expressions can't return references. '" ++ pretty xobj ++ "' : " ++ show t ++ "."]
    (GettingReferenceToUnownedValue xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Referencing a given-away value '" ++ pretty xobj ++ "'."]
    (UsingUnownedValue xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Using a given-away value '" ++ pretty xobj ++ "'."]
    (UsingCapturedValue xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Using a captured value '" ++ pretty xobj ++ "'."]
    (ArraysCannotContainRefs xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Arrays can't contain references: '" ++ pretty xobj ++ "'."]

    (MainCanOnlyReturnUnitOrInt xobj t) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Main function can only return Int or (), got " ++ show t ++ "."]
    (MainCannotHaveArguments xobj c) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Main function can not have arguments, got " ++ show c ++ "."]

    (TooManyAnnotateCalls xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Too many annotate calls (infinite loop) when annotating '" ++ pretty xobj ++ "'."]

  --    (InvalidMemberType msg) ->
 -- --   msg

    (CannotSet xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't set! '" ++ pretty xobj ++ "'."]
    (CannotConcretize xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Unable to concretize '" ++ pretty xobj ++ "'."]

    (DoesNotMatchSignatureAnnotation xobj sigTy) ->
      [machineReadableInfoFromXObj fppl xobj ++ "Definition does not match 'sig' annotation " ++ show sigTy ++ ", actual type is " ++ show (forceTy xobj)]

    (CannotMatch xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't match '" ++ pretty xobj ++ "'."]

    (InvalidSumtypeCase xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Failed to convert '" ++ pretty xobj ++ "' to a sumtype case."]

    (InvalidMemberType t xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't use '" ++ show t ++ "' as a type for a member variable."]
    (NotAmongRegisteredTypes t xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " The type '" ++ show t ++ "' isn't defined."]
    (UnevenMembers xobjs) ->
      [machineReadableInfoFromXObj fppl (head xobjs) ++ " Uneven nr of members / types: " ++ joinWithComma (map pretty xobjs)]
    (InvalidLetBinding xobjs (sym, expr)) ->
      [machineReadableInfoFromXObj fppl (head xobjs) ++ "Invalid let binding `" ++ pretty sym ++ pretty expr ++ "` at " ++ joinWithComma (map pretty xobjs)]
    (DuplicateBinding xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Duplicate binding `" ++ pretty xobj ++ "` inside `let`."]
    (DefinitionsMustBeAtToplevel xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Definition not at top level: `" ++ pretty xobj ++ "`"]

    _ ->
      [show err]

joinedMachineReadableErrorStrings :: FilePathPrintLength -> TypeError -> String
joinedMachineReadableErrorStrings fppl err = joinWith "\n\n" (machineReadableErrorStrings fppl err)

recursiveLookupTy :: TypeMappings -> Ty -> Ty
recursiveLookupTy mappings t = case t of
                                 (VarTy v) -> fromMaybe t (recursiveLookup mappings v)
                                 (RefTy r) -> RefTy (recursiveLookupTy mappings r)
                                 (PointerTy p) -> PointerTy (recursiveLookupTy mappings p)
                                 (StructTy n innerTys) -> StructTy n (map (recursiveLookupTy mappings) innerTys)
                                 (FuncTy argTys retTy) -> FuncTy (map (recursiveLookupTy mappings) argTys)
                                                                 (recursiveLookupTy mappings retTy)
                                 _ -> t

showTypeFromXObj :: TypeMappings -> XObj -> String
showTypeFromXObj mappings xobj =
  case ty xobj of
    Just t -> show (recursiveLookupTy mappings t)
    Nothing -> "Type missing"

-- | Print type errors correctly when running the compiler in 'Check' mode
makeEvalError :: Context -> Maybe TypeError.TypeError -> String -> Maybe Info -> Either EvalError a
makeEvalError ctx err msg info =
  let fppl = projectFilePathPrintLength (contextProj ctx)
  in case contextExecMode ctx of
       Check -> let messageWhenChecking = case err of
                                            Just okErr -> joinedMachineReadableErrorStrings fppl okErr
                                            Nothing ->
                                              case info of
                                                Just okInfo -> machineReadableInfo fppl okInfo ++ " " ++ msg
                                                Nothing -> msg
                in  Left (EvalError messageWhenChecking Nothing fppl) -- Passing no info to avoid appending it at the end in 'show' instance for EvalError
       _ ->  Left (EvalError msg info fppl)
