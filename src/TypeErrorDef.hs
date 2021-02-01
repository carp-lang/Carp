module TypeErrorDef where

import Constraints
import Obj
import Types

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
