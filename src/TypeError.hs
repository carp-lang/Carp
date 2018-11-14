module TypeError where

import Data.Maybe (fromMaybe)

import Types
import Obj
import Constraints
import Util

data TypeError = SymbolMissingType XObj Env
               | DefnMissingType XObj
               | DefMissingType XObj
               | ExpressionMissingType XObj
               | SymbolNotDefined SymPath XObj
               | InvalidObj Obj XObj
               | CantUseDerefOutsideFunctionApplication XObj
               | NotAType XObj
               | WrongArgCount XObj
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
               | InvalidMemberType String
               | CannotSet XObj
               | DoesNotMatchSignatureAnnotation XObj Ty -- Not used at the moment (but should?)

instance Show TypeError where
  show (SymbolMissingType xobj env) =
    "Symbol '" ++ getName xobj ++ "' missing type at " ++ prettyInfoFromXObj xobj ++ " in env:\n" ++ prettyEnvironment env
  show (DefnMissingType xobj) =
    "Function definition '" ++ getName xobj ++ "' missing type at " ++ prettyInfoFromXObj xobj  ++ "."
  show (DefMissingType xobj) =
    "Variable definition '" ++ getName xobj ++ "' missing type at " ++ prettyInfoFromXObj xobj  ++ "."
  show (ExpressionMissingType xobj)=
    "Expression '" ++ pretty xobj ++ "' missing type at " ++ prettyInfoFromXObj xobj ++ "."
  show (SymbolNotDefined symPath xobj) =
    "Trying to refer to an undefined symbol '" ++ show symPath ++ "' at " ++ prettyInfoFromXObj xobj ++ "."
  show (InvalidObj Defn xobj) =
    "Invalid function definition at " ++ prettyInfoFromXObj xobj ++ "."
  show (CantUseDerefOutsideFunctionApplication xobj) =
    "Can't use 'deref' / '~' outside function application at " ++ prettyInfoFromXObj xobj ++ "."
  show (InvalidObj If xobj) =
    "Invalid if-statement at " ++ prettyInfoFromXObj xobj ++ "."
  show (InvalidObj o xobj) =
    "Invalid obj '" ++ show o ++ "' at " ++ prettyInfoFromXObj xobj ++ "."
  show (WrongArgCount xobj) =
    "Wrong argument count in call to '" ++ getName xobj ++ "' at " ++ prettyInfoFromXObj xobj ++ "."
  show (NotAFunction xobj) =
    "Trying to call non-function '" ++ getName xobj ++ "' at " ++ prettyInfoFromXObj xobj ++ "."
  show (NoStatementsInDo xobj) =
    "The do-statement has no expressions inside of it at " ++ prettyInfoFromXObj xobj ++ "."
  show (TooManyFormsInBody xobj) =
    "Too many expressions in body position at " ++ prettyInfoFromXObj xobj ++ "."
  show (NoFormsInBody xobj) =
    "No expressions in body position at " ++ prettyInfoFromXObj xobj ++ "."
  show (UnificationFailed constraint@(Constraint a b aObj bObj _) mappings constraints) =
    "Can't unify " ++ show (recursiveLookupTy mappings a) ++ " with " ++ show (recursiveLookupTy mappings b) ++ "\n\n" ++
    --show aObj ++ "\nWITH\n" ++ show bObj ++ "\n\n" ++
    "  " ++ pretty aObj ++ " : " ++ showTypeFromXObj mappings aObj ++ "\n  At " ++ prettyInfoFromXObj aObj ++ "" ++
    "\n\n" ++
    "  " ++ pretty bObj ++ " : " ++ showTypeFromXObj mappings bObj ++ "\n  At " ++ prettyInfoFromXObj bObj ++ "\n"
    -- ++ "Constraint: " ++ show constraint ++ "\n\n"
    -- "All constraints:\n" ++ show constraints ++ "\n\n" ++
    -- "Mappings: \n" ++ show mappings ++ "\n\n"
  show (CantDisambiguate xobj originalName theType options) =
    "Can't disambiguate symbol '" ++ originalName ++ "' of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++
    "\nPossibilities:\n    " ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (CantDisambiguateInterfaceLookup xobj name theType options) =
    "Can't disambiguate interface lookup symbol '" ++ name ++ "' of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++
    "\nPossibilities:\n    " ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (SeveralExactMatches xobj name theType options) =
    "Several exact matches for interface lookup symbol '" ++ name ++ "' of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++
    "\nPossibilities:\n    " ++ joinWith "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (NoMatchingSignature xobj originalName theType options) =
    "Can't find matching lookup for symbol '" ++ originalName ++
    "' of type " ++ show theType ++ " at " ++ prettyInfoFromXObj xobj ++
    "\nNone of the possibilities have the correct signature:\n    " ++ joinWith
    "\n    " (map (\(t, p) -> show p ++ " : " ++ show t) options)
  show (LeadingColon xobj) =
    "Symbol '" ++ pretty xobj ++ "' starting with colon at " ++ prettyInfoFromXObj xobj ++ "."
  show (HolesFound holes) =
    "Holes found:\n\n    " ++ joinWith "\n    " (map (\(name, t) -> name ++ " : " ++ show t) holes) ++ "\n"
  show (FailedToExpand xobj (EvalError errorMessage)) =
    "Failed to expand at " ++ prettyInfoFromXObj xobj ++ ": " ++ errorMessage
  show (NotAValidType xobj) =
    "Not a valid type: " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj
  show (FunctionsCantReturnRefTy xobj t) =
    "Functions can't return references. " ++ getName xobj ++ " : " ++ show t ++ " at " ++ prettyInfoFromXObj xobj
  show (LetCantReturnRefTy xobj t) =
    "Let-expressions can't return references. '" ++ pretty xobj ++ "' : " ++ show t ++ " at " ++ prettyInfoFromXObj xobj
  show (GettingReferenceToUnownedValue xobj) =
    "Referencing a given-away value '" ++ pretty xobj ++ "' at " ++ --"' (expression " ++ freshVar i ++ ") at " ++
    prettyInfoFromXObj xobj ++ "\n" ++ show xobj
  show (UsingUnownedValue xobj) =
    "Using a given-away value '" ++ pretty xobj ++ "' at " ++ prettyInfoFromXObj xobj
  show (UsingCapturedValue xobj) =
    "Using a captured value '" ++ pretty xobj ++ "' at " ++ prettyInfoFromXObj xobj
  show (ArraysCannotContainRefs xobj) =
    "Arrays can't contain references: '" ++ pretty xobj ++ "' at " ++ prettyInfoFromXObj xobj
  show (MainCanOnlyReturnUnitOrInt xobj t) =
    "Main function can only return Int or (), got " ++ show t
  show (MainCannotHaveArguments xobj c) =
    "Main function can not have arguments, got " ++ show c
  show (CannotConcretize xobj) =
    "Unable to concretize '" ++ pretty xobj ++ "' at " ++ prettyInfoFromXObj xobj
  show (TooManyAnnotateCalls xobj) =
    "Too many annotate calls (infinite loop) when annotating '" ++ pretty xobj ++ "' at " ++ prettyInfoFromXObj xobj
  show (NotAType xobj) =
    "Can't understand the type '" ++ pretty xobj ++ "' at " ++ prettyInfoFromXObj xobj
  show (InvalidMemberType msg) =
    msg
  show (CannotSet xobj) =
    "Can't 'set!' " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj
  show (DoesNotMatchSignatureAnnotation xobj sigTy) =
    "Definition at " ++ prettyInfoFromXObj xobj ++ " does not match 'sig' annotation " ++ show sigTy ++ ", actual type is " ++ show (forceTy xobj)

machineReadableErrorStrings :: FilePathPrintLength -> TypeError -> [String]
machineReadableErrorStrings fppl err =
  case err of
    (UnificationFailed constraint@(Constraint a b aObj bObj _) mappings constraints) ->
      [machineReadableInfoFromXObj fppl aObj ++ " " ++ pretty aObj ++ " : " ++
       showTypeFromXObj mappings aObj ++ ", can't unify with " ++ show (recursiveLookupTy mappings b) ++ "."
      ,machineReadableInfoFromXObj fppl bObj ++ " " ++ pretty bObj ++ " : " ++
       showTypeFromXObj mappings bObj ++ ", can't unify with " ++ show (recursiveLookupTy mappings a) ++ "."]

    (DefnMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Function definition '" ++ getName xobj ++ "' missing type."]
    (DefMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Variable definition '" ++ getName xobj ++ "' missing type."]
    (ExpressionMissingType xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Expression '" ++ pretty xobj ++ "' missing type."]
    (SymbolNotDefined symPath xobj) ->
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
    (WrongArgCount xobj) ->
      [machineReadableInfoFromXObj fppl xobj ++ " Wrong argument count in call to '" ++ getName xobj ++ "'."]
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

    (FailedToExpand xobj (EvalError errorMessage)) ->
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

    _ ->
      [show err]

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
