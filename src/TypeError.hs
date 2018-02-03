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
               | ArraysCannotContainRefs XObj
               | MainCanOnlyReturnUnitOrInt Ty
               | MainCannotHaveArguments Int
               | CannotConcretize XObj
               | TooManyAnnotateCalls XObj
               | InvalidMemberType String
               | CannotSet XObj

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
    --show aObj ++ " WITH " ++ show bObj ++ "\n\n" ++
    "  " ++ pretty aObj ++ " : " ++ showTypeFromXObj aObj ++ "\n  At " ++ prettyInfoFromXObj aObj ++ "" ++
    "\n\n" ++
    "  " ++ pretty bObj ++ " : " ++ showTypeFromXObj bObj ++ "\n  At " ++ prettyInfoFromXObj bObj ++ "\n"
    -- ++ "Constraint: " ++ show constraint ++ "\n\n"
    -- "All constraints:\n" ++ show constraints ++ "\n\n" ++
    -- "Mappings: \n" ++ show mappings ++ "\n\n"
    where showTypeFromXObj :: XObj -> String
          showTypeFromXObj xobj = case ty xobj of
                                    Just t -> show (recursiveLookupTy mappings t)
                                    Nothing -> "Type missing"
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
  show (ArraysCannotContainRefs xobj) =
    "Arrays can't contain references: '" ++ pretty xobj ++ "' at " ++ prettyInfoFromXObj xobj
  show (MainCanOnlyReturnUnitOrInt t) =
    "Main function can only return Int or (), got " ++ show t
  show (MainCannotHaveArguments c) =
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

recursiveLookupTy :: TypeMappings -> Ty -> Ty
recursiveLookupTy mappings t = case t of
                                 (VarTy v) -> fromMaybe t (recursiveLookup mappings v)
                                 (RefTy r) -> RefTy (recursiveLookupTy mappings r)
                                 (PointerTy p) -> PointerTy (recursiveLookupTy mappings p)
                                 (StructTy n innerTys) -> StructTy n (map (recursiveLookupTy mappings) innerTys)
                                 (FuncTy argTys retTy) -> FuncTy (map (recursiveLookupTy mappings) argTys)
                                                                 (recursiveLookupTy mappings retTy)
                                 _ -> t
