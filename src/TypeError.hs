module TypeError where

import Constraints
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Info
import qualified Map
import Obj
import Project
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import Types
import Util

data TypeError
  = SymbolMissingType XObj Env
  | DefnMissingType XObj
  | DefMissingType XObj
  | ExpressionMissingType XObj
  | SymbolNotDefined SymPath XObj Env
  | InvalidObj Obj XObj
  | InvalidObjExample Obj XObj String
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
  | CannotSetVariableFromLambda XObj XObj
  | DoesNotMatchSignatureAnnotation XObj Ty -- Not used at the moment (but should?)
  | CannotMatch XObj
  | InvalidSumtypeCase XObj
  | InvalidMemberType Ty XObj
  | InvalidMemberTypeWhenConcretizing Ty XObj TypeError
  | NotAmongRegisteredTypes Ty XObj
  | UnevenMembers [XObj]
  | DuplicatedMembers [XObj]
  | InvalidLetBinding [XObj] (XObj, XObj)
  | DuplicateBinding XObj
  | DefinitionsMustBeAtToplevel XObj
  | UsingDeadReference XObj String
  | UninhabitedConstructor Ty XObj Int Int
  | InconsistentKinds String [XObj]
  | FailedToAddLambdaStructToTyEnv SymPath XObj
  | FailedToInstantiateGenericType Ty
  | InvalidStructField XObj
  | FunctionLeaksCapture [String] XObj

instance Show TypeError where
  show (SymbolMissingType xobj env) =
    "I couldn’t find a type for the symbol '" ++ getName xobj ++ "' at "
      ++ prettyInfoFromXObj xobj
      ++ " in the environment:\n"
      ++ prettyEnvironment env
      ++ "\n\nIt might be too general. You could try adding a type hint using `the`."
  show (DefnMissingType xobj) =
    "I couldn’t find a type for the function definition '" ++ getName xobj
      ++ "' at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nIt might be too general. You could try adding a type hint using `the`."
  show (DefMissingType xobj) =
    "I couldn’t find a type for the variable definition '" ++ getName xobj
      ++ "' at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nIt might be too general. You could try adding a type hint using `the`."
  show (ExpressionMissingType xobj) =
    "I couldn’t find a type for the expression '" ++ pretty xobj ++ "' at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nIt might be too general. You could try adding a type hint using `the`."
  show (SymbolNotDefined symPath@(SymPath p _) xobj env) =
    "I couldn’t find the symbol '" ++ show symPath ++ "' at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\n"
      ++ matches (keysInEnvEditDistance symPath env 3)
    where
      matches [] = "Maybe you forgot to define it?"
      matches x = "Maybe you wanted one of the following?\n    " ++ joinWith "\n    " (map (show . SymPath p) x)
  show (InvalidObj (Defn _) xobj) =
    "I didn’t understand the function definition at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nIs it valid?  Every `defn` needs to follow the form `(defn name [arg] body)`."
  show (CantUseDerefOutsideFunctionApplication xobj) =
    "I found a `deref` / `~` that isn’t inside a function application at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nEvery usage of `~` must be inside a function application."
  show (InvalidObj If xobj) =
    "I didn’t understand the `if` statement at " ++ prettyInfoFromXObj xobj
      ++ ".\n\nIs it valid? Every `if` needs to follow the form `(if cond iftrue iffalse)`."
  show (InvalidObj (Mod env _) xobj) =
    let moduleName =
          case envModuleName env of
            Just name -> "the module '" ++ name ++ "'"
            Nothing -> "an unnamed module"
     in "I didn’t understand the form mentioning " ++ moduleName ++ " at " ++ prettyInfoFromXObj xobj
          ++ ".\n\nAre you using a module or type where a value is expected?"
  show (InvalidObj o xobj) =
    "I didn’t understand the form `" ++ prettyObj o ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nIs it valid?"
  show (InvalidObjExample o xobj example) =
    "I didn’t understand the form `" ++ prettyObj o ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nIs it valid? It needs to follow the form `"
      ++ example
      ++ "`."
  show (WrongArgCount xobj expected actual) =
    "You used the wrong number of arguments in '" ++ getName xobj ++ "' at "
      ++ prettyInfoFromXObj xobj
      ++ ". I expected "
      ++ show expected
      ++ ", but got "
      ++ show actual
      ++ "."
  show (NotAFunction xobj) =
    "You are trying to call the non-function `" ++ getName xobj ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "."
  show (NoStatementsInDo xobj) =
    "There are no expressions inside of the `do` statement at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nAll instances of `do` need to have one or more expressions in it."
  show (TooManyFormsInBody xobj) =
    "There are too many expressions in the body of the form at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nTry wrapping them in a `do`."
  show (NoFormsInBody xobj) =
    "There are no expressions in the body of the form at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nI need exactly one body form. For multiple forms, try using `do`."
  show (UnificationFailed (Constraint a b aObj bObj ctx _) mappings _) =
    "I can’t match the types `" ++ showTy a ++ "` and `" ++ showTy b ++ "`."
      ++ extra
      ++ showObj aObj
      ++ showObj bObj
    where
      -- ++ "Constraint: " ++ show constraint ++ "\n\n"
      -- "All constraints:\n" ++ show constraints ++ "\n\n" ++
      -- "Mappings: \n" ++ show mappings ++ "\n\n"
      extra = if ctx == aObj || ctx == bObj then "" else " within `" ++ snip (pretty ctx) ++ "`"
      snip s =
        if length s > 25
          then take 15 s ++ " ... " ++ drop (length s - 5) s
          else s
      beautifulTy = beautifyTy mappings . recursiveLookupTy mappings
      showTy = show . beautifulTy
      showObjTy = fromMaybe "Type missing" . fmap showTy . xobjTy
      showObj o =
        "\n\n  " ++ pretty o ++ " : " ++ showObjTy o
          ++ "\n  At "
          ++ prettyInfoFromXObj o
          ++ ""
  show (CantDisambiguate xobj originalName theType options) =
    "I found an ambiguous symbol `" ++ originalName ++ "` of type `"
      ++ show theType
      ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "\nPossibilities:\n    "
      ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (CantDisambiguateInterfaceLookup xobj name theType options) =
    "I found an ambiguous interface `" ++ name ++ "` of type `"
      ++ show theType
      ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "\nPossibilities:\n    "
      ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (SeveralExactMatches xobj name theType options) =
    "There are several exact matches for the interface `" ++ name
      ++ "` of type `"
      ++ show theType
      ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "\nPossibilities:\n    "
      ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (NoMatchingSignature xobj originalName theType options) =
    "I can’t find any implementation for the interface `" ++ originalName
      ++ "` of type "
      ++ show theType
      ++ " at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nNone of the possibilities have the correct signature:\n    "
      ++ joinWith
        "\n    "
        (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (LeadingColon xobj) =
    "I found a symbol '" ++ pretty xobj ++ "' that starts with a colon at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nThis is disallowed."
  show (HolesFound holes) =
    "I found the following holes:\n\n    "
      ++ joinWith "\n    " (map (\(name, t) -> name ++ " : " ++ show t) holes)
      ++ "\n"
  show (NotAValidType xobj) =
    pretty xobj ++ "is not a valid type at " ++ prettyInfoFromXObj xobj
  show (FunctionsCantReturnRefTy xobj t) =
    "Functions can’t return references. " ++ getName xobj ++ " : " ++ show t
      ++ " at "
      ++ prettyInfoFromXObj xobj
      ++ "\n\nYou’ll have to copy the return value using `@`."
  show (LetCantReturnRefTy xobj t) =
    "`let` expressions can’t return references. " ++ pretty xobj ++ " : "
      ++ show t
      ++ " at "
      ++ prettyInfoFromXObj xobj
      ++ "\n\nYou’ll have to copy the return value using `@`."
  show (GettingReferenceToUnownedValue xobj) =
    "You’re referencing a given-away value `" ++ pretty xobj ++ "` at "
      ++ prettyInfoFromXObj xobj --"' (expression " ++ freshVar i ++ ") at " ++
      ++ "\n"
      ++ show xobj
      ++ "\n\nYou’ll have to copy the value using `@`."
  show (UsingUnownedValue xobj) =
    "You’re using a given-away value `" ++ pretty xobj ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nYou’ll have to copy the value using `@`."
  show (UsingCapturedValue xobj) =
    "You’re using a value `" ++ pretty xobj
      ++ "` that was captured by a function at "
      ++ prettyInfoFromXObj xobj
      ++ "."
  show (ArraysCannotContainRefs xobj) =
    "Arrays can’t contain references: `" ++ pretty xobj ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nYou’ll have to make a copy using `@`."
  show (MainCanOnlyReturnUnitOrInt _ t) =
    "The main function can only return an `Int` or a unit type (`()`), but it got `"
      ++ show t
      ++ "`."
  show (MainCannotHaveArguments _ c) =
    "The main function may not receive arguments, but it got " ++ show c ++ "."
  show (CannotConcretize xobj) =
    "I’m unable to concretize the expression '" ++ pretty xobj ++ "' at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nIt might be too general. You could try adding a type hint using `the`."
  show (TooManyAnnotateCalls xobj) =
    "There were too many annotation calls when annotating `" ++ pretty xobj
      ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\n I deduced it was an infinite loop."
  show (NotAType xobj) =
    "I don’t understand the type '" ++ pretty xobj ++ "' at "
      ++ prettyInfoFromXObj xobj
      ++ "\n\nIs it defined?"
  show (CannotSet xobj) =
    "I can’t `set!` the expression `" ++ pretty xobj ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nOnly variables can be reset using `set!`."
  show (CannotSetVariableFromLambda variable _) =
    "I can’t `set!` the variable `" ++ pretty variable ++ "` at "
      ++ prettyInfoFromXObj variable
      ++ " because it's defined outside the lambda."
  show (DoesNotMatchSignatureAnnotation xobj sigTy) =
    "The definition at " ++ prettyInfoFromXObj xobj
      ++ " does not match its annotation provided to `sig` as `"
      ++ show sigTy
      ++ "`, its actual type is `"
      ++ show (forceTy xobj)
      ++ "`."
  show (CannotMatch xobj) =
    "I can’t `match` `" ++ pretty xobj ++ "` at " ++ prettyInfoFromXObj xobj
      ++ ".\n\nOnly sumtypes can be matched against."
  show (InvalidSumtypeCase xobj) =
    "I failed to read `" ++ pretty xobj ++ "` as a sumtype case at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nSumtype cases look like this: `(Foo [Int typevar])`"
  show (InvalidStructField xobj) =
    "I can't use " ++ pretty xobj ++ "as a struct field at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nStruct fields look like this: x Int, e.g. (deftype Point [x Int y Int])"
  show (InvalidMemberType t xobj) =
    "I can’t use the type `" ++ show t ++ "` as a member type at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nIs it defined and captured in the head of the type definition?"
  show (InvalidMemberTypeWhenConcretizing t xobj err) =
    "I can’t use the concrete type `" ++ show t ++ "` at " ++ prettyInfoFromXObj xobj ++ ": " ++ show err
  show (NotAmongRegisteredTypes t xobj) =
    "I can’t find a definition for the type `" ++ show t ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ ".\n\nWas it registered?"
  show (UnevenMembers xobjs) =
    "The number of members and types is uneven: `"
      ++ joinWithComma (map pretty xobjs)
      ++ "` at "
      ++ prettyInfoFromXObj (head xobjs)
      ++ ".\n\nBecause they are pairs of names and their types, they need to be even.\nDid you forget a name or type?"
  show (DuplicatedMembers xobjs) =
    "Duplicate members: `"
      ++ joinWithComma (map pretty xobjs)
      ++ "` at "
      ++ prettyInfoFromXObj (head xobjs)
  show (InvalidLetBinding xobjs (sym, expr)) =
    "The binding `[" ++ pretty sym ++ " " ++ pretty expr ++ "]` is invalid at "
      ++ prettyInfoFromXObj (head xobjs)
      ++ ". \n\n Binding names must be symbols."
  show (DuplicateBinding xobj) =
    "I encountered a duplicate binding `" ++ pretty xobj ++ "` inside the `let` at " ++ prettyInfoFromXObj xobj ++ "."
  show (DefinitionsMustBeAtToplevel xobj) =
    "I encountered a definition that was not at top level: `" ++ pretty xobj ++ "`"
  show (UsingDeadReference xobj dependsOn) =
    "The reference '" ++ pretty xobj ++ "' (depending on the variable '" ++ dependsOn ++ "') isn't alive at " ++ prettyInfoFromXObj xobj ++ "."
  show (UninhabitedConstructor ty xobj got wanted) =
    "Can't use a struct or sumtype constructor without arguments as a member type at " ++ prettyInfoFromXObj xobj ++ ". The type constructor " ++ show ty ++ " expects " ++ show wanted ++ " arguments but got " ++ show got
  show (InconsistentKinds varName xobjs) =
    " The type variable `" ++ varName ++ "` is used inconsistently: " ++ joinWithComma (map pretty (filter (doesTypeContainTyVarWithName varName . fromMaybe Universe . xobjToTy) xobjs)) ++ " Type variables must be applied to the same number of arguments."
  show (FailedToAddLambdaStructToTyEnv path xobj) =
    "Failed to add the lambda: " ++ show path ++ " represented by struct: "
      ++ pretty xobj
      ++ " to the type environment."
  show (FailedToInstantiateGenericType ty) =
    "I couldn't instantiate the generic type " ++ show ty
  show (FunctionLeaksCapture leaks xobj) = 
    "The function "  ++ pretty xobj ++ " gives away the captured variables: " 
    ++ joinWithComma leaks ++ ". Functions must keep ownership of variables captured from another environment."

machineReadableErrorStrings :: FilePathPrintLength -> TypeError -> [String]
machineReadableErrorStrings fppl err =
  case err of
    (UnificationFailed (Constraint a b aObj bObj _ _) mappings _) ->
      [ machineReadableInfoFromXObj fppl aObj ++ " Inferred " ++ showTypeFromXObj mappings aObj ++ ", can't unify with " ++ show (recursiveLookupTy mappings b) ++ ".",
        machineReadableInfoFromXObj fppl bObj ++ " Inferred " ++ showTypeFromXObj mappings bObj ++ ", can't unify with " ++ show (recursiveLookupTy mappings a) ++ "."
      ]
    (DefnMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Function definition '" ++ getName xobj ++ "' missing type."]
    (DefMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Variable definition '" ++ getName xobj ++ "' missing type."]
    (ExpressionMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Expression '" ++ pretty xobj ++ "' missing type."]
    (SymbolNotDefined symPath xobj _) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Trying to refer to an undefined symbol '" ++ show symPath ++ "'."]
    (SymbolMissingType xobj _) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Symbol '" ++ getName xobj ++ "' missing type."]
    (InvalidObj (Defn _) xobj) ->
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
      [ machineReadableInfoFromXObj fppl xobj ++ " Can't disambiguate symbol '" ++ originalName ++ "' of type " ++ show theType
          ++ "\nPossibilities:\n    "
          ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
      ]
    (CantDisambiguateInterfaceLookup xobj name theType options) ->
      [ machineReadableInfoFromXObj fppl xobj ++ " Can't disambiguate interface lookup symbol '" ++ name ++ "' of type " ++ show theType
          ++ "\nPossibilities:\n    "
          ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
      ]
    (SeveralExactMatches xobj name theType options) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Several exact matches for interface lookup symbol '" ++ name ++ "' of type " ++ show theType ++ "\nPossibilities:\n    " ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)]
    (NoMatchingSignature xobj originalName theType options) ->
      [ machineReadableInfoFromXObj fppl xobj ++ " Can't find matching lookup for symbol '" ++ originalName ++ "' of type " ++ show theType
          ++ "\nNone of the possibilities have the correct signature:\n    "
          ++ joinWith
            "\n    "
            (map (\(t, p) -> show p ++ " : " ++ show t) options)
      ]
    (LeadingColon xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Symbol '" ++ pretty xobj ++ "' starting with a colon (reserved for REPL shortcuts)."]
    -- (HolesFound holes) ->
    --   (map (\(name, t) -> machineReadableInfoFromXObj fppl xobj ++ " " ++ name ++ " : " ++ show t) holes)

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
    (CannotSet xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Can't set! '" ++ pretty xobj ++ "'."]
    (CannotSetVariableFromLambda variable _) ->
      [machineReadableInfoFromXObj fppl variable ++ " Can't set! '" ++ pretty variable ++ "' from inside of a lambda."]
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
    (UsingDeadReference xobj _) ->
      [machineReadableInfoFromXObj fppl xobj ++ " The reference '" ++ pretty xobj ++ "' isn't alive."]
    (UninhabitedConstructor ty xobj got wanted) ->
      [machineReadableInfoFromXObj fppl xobj ++ "Can't use a struct or sumtype constructor without arguments as a member type at " ++ prettyInfoFromXObj xobj ++ ". The type constructor " ++ show ty ++ " expects " ++ show wanted ++ " arguments but got " ++ show got]
    (InconsistentKinds varName xobjs) ->
      [machineReadableInfoFromXObj fppl (head xobjs) ++ " The type variable `" ++ varName ++ "` is used inconsistently: " ++ joinWithComma (map pretty (filter (doesTypeContainTyVarWithName varName . fromMaybe Universe . xobjToTy) xobjs)) ++ " Type variables must be applied to the same number of arguments."]
    (FailedToAddLambdaStructToTyEnv path xobj) ->
      [ machineReadableInfoFromXObj fppl xobj ++ "Failed to add the lambda: " ++ show path ++ " represented by struct: "
          ++ pretty xobj
          ++ " to the type environment."
      ]
    e@(FunctionLeaksCapture _ xobj) -> [machineReadableInfoFromXObj fppl xobj ++ show e] 
    _ ->
      [show err]

joinedMachineReadableErrorStrings :: FilePathPrintLength -> TypeError -> String
joinedMachineReadableErrorStrings fppl err = joinWith "\n\n" (machineReadableErrorStrings fppl err)

recursiveLookupTy :: TypeMappings -> Ty -> Ty
recursiveLookupTy mappings t = case t of
  (VarTy v) -> fromMaybe t (recursiveNameLookup mappings v)
  (RefTy r lt) -> RefTy (recursiveLookupTy mappings r) (recursiveLookupTy mappings lt)
  (PointerTy p) -> PointerTy (recursiveLookupTy mappings p)
  (StructTy n innerTys) -> StructTy n (map (recursiveLookupTy mappings) innerTys)
  (FuncTy argTys retTy ltTy) ->
    FuncTy
      (map (recursiveLookupTy mappings) argTys)
      (recursiveLookupTy mappings retTy)
      (recursiveLookupTy mappings ltTy)
  _ -> t

showTypeFromXObj :: TypeMappings -> XObj -> String
showTypeFromXObj mappings xobj =
  case xobjTy xobj of
    Just t -> show (recursiveLookupTy mappings t)
    Nothing -> "Type missing"

evalError :: Context -> String -> Maybe Info -> (Context, Either EvalError a)
evalError ctx = makeEvalError ctx Nothing

-- | Print type errors correctly when running the compiler in 'Check' mode
makeEvalError :: Context -> Maybe TypeError -> String -> Maybe Info -> (Context, Either EvalError a)
makeEvalError ctx err msg info =
  let fppl = projectFilePathPrintLength (contextProj ctx)
      history = contextHistory ctx
   in case contextExecMode ctx of
        Check ->
          let messageWhenChecking = case err of
                Just okErr -> joinedMachineReadableErrorStrings fppl okErr
                Nothing ->
                  case info of
                    Just okInfo -> machineReadableInfo fppl okInfo ++ " " ++ msg
                    Nothing -> msg
           in (ctx, Left (EvalError messageWhenChecking [] fppl Nothing)) -- Passing no history to avoid appending it at the end in 'show' instance for EvalError
        _ -> (ctx, Left (EvalError msg history fppl info))

-- | Converts a TypeError to a string, taking contextExecMode/fppl into account
typeErrorToString :: Context -> TypeError -> String
typeErrorToString ctx err =
  let fppl = projectFilePathPrintLength (contextProj ctx)
   in case contextExecMode ctx of
        Check -> joinedMachineReadableErrorStrings fppl err
        _ -> show err

keysInEnvEditDistance :: SymPath -> Env -> Int -> [String]
keysInEnvEditDistance (SymPath [] name) env distance =
  let candidates = Map.filterWithKey (\k _ -> levenshteinDistance defaultEditCosts k name < distance) (envBindings env)
   in Map.keys candidates
keysInEnvEditDistance path@(SymPath (p : ps) name) env distance =
  case Map.lookup p (envBindings env) of
    Just (Binder _ xobj) ->
      case xobj of
        (XObj (Mod modEnv _) _ _) -> keysInEnvEditDistance (SymPath ps name) modEnv distance
        _ -> []
    Nothing ->
      case envParent env of
        Just parent -> keysInEnvEditDistance path parent distance
        Nothing -> []

beautifyTy :: TypeMappings -> Ty -> Ty
beautifyTy mappings = f
  where
    f :: Ty -> Ty
    f (FuncTy argTys retTy lifetime) = FuncTy (f <$> argTys) (f retTy) (f lifetime)
    f (StructTy n typeArgs) = StructTy n (f <$> typeArgs)
    f (RefTy innerTy lifetime) = RefTy (f innerTy) (f lifetime)
    f (PointerTy innerTy) = PointerTy $ f innerTy
    f t@(VarTy n) = case Map.lookup n bmappings of
      Just nn -> VarTy nn
      Nothing -> t
    f t = t
    bmappings = beautification mappings
    beautification :: TypeMappings -> Map.Map String String
    beautification m =
      Map.fromList $ zip (map (\(VarTy name) -> name) tys) beautList
      where
        tys = nub $ concat $ typeVariablesInOrderOfAppearance <$> tys'
        tys' = snd <$> Map.assocs m
    beautList = [c : s | s <- "" : beautList, c <- ['a' .. 'z']]

typeVariablesInOrderOfAppearance :: Ty -> [Ty]
typeVariablesInOrderOfAppearance (FuncTy argTys retTy ltTy) =
  concatMap typeVariablesInOrderOfAppearance argTys ++ typeVariablesInOrderOfAppearance retTy ++ typeVariablesInOrderOfAppearance ltTy
typeVariablesInOrderOfAppearance (StructTy n typeArgs) =
  case n of
    t@(VarTy _) -> typeVariablesInOrderOfAppearance t ++ concatMap typeVariablesInOrderOfAppearance typeArgs
    _ -> concatMap typeVariablesInOrderOfAppearance typeArgs
typeVariablesInOrderOfAppearance (RefTy innerTy lifetimeTy) =
  typeVariablesInOrderOfAppearance innerTy ++ typeVariablesInOrderOfAppearance lifetimeTy
typeVariablesInOrderOfAppearance (PointerTy innerTy) =
  typeVariablesInOrderOfAppearance innerTy
typeVariablesInOrderOfAppearance t@(VarTy _) =
  [t]
typeVariablesInOrderOfAppearance _ =
  []
