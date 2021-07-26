{-# LANGUAGE PatternSynonyms #-}

-- | Module Forms both defines valid forms in Carp and performs validation on
-- unchecked forms (xobjs).
--
-- It defines a number of pattern synonyms for ease of pattern matching.
module Forms
  ( validate,
    format,
    Malformed (GenericMalformed),
    pattern ArrPat,
    pattern StaticArrPat,
    pattern ListPat,
    pattern SymPat,
    pattern UnqualifiedSymPat,
    pattern DefPat,
    pattern DefnPat,
    pattern IfPat,
    pattern ThePat,
    pattern RefPat,
    pattern LetPat,
    pattern FnPat,
    pattern ClosurePat,
    pattern DynamicFnPat,
    pattern MacroPat,
    pattern CommandPat,
    pattern PrimitivePat,
    pattern AppPat,
    pattern WithPat,
    pattern DoPat,
    pattern WhilePat,
    pattern SetPat,
    pattern MultiSymPat,
    pattern InterfaceSymPat,
    pattern MatchPat,
    pattern InterfacePat,
  )
where

import Data.List (intercalate)
import Obj
import SymPath
import Types
import Util

--------------------------------------------------------------------------------
-- Data

-- Specialized constructors for each built-in language form.

-- | Error type representing a generic malformed expression.
data Malformed
  = InvalidIdentifier XObj Modifier
  | QualifiedIdentifier XObj Modifier
  | GenericMalformed XObj
  | InvalidArguments XObj Modifier
  | InvalidBody XObj Modifier
  | InvalidCondition XObj Modifier
  | InvalidType XObj Modifier
  | InvalidBindings XObj Modifier
  | UnevenForms XObj Int Modifier
  | InsufficientArguments XObj Int Int [XObj]
  | TooManyArguments XObj Int Int [XObj]
  | InvalidApplication XObj
  | DoMissingForms

instance Show Malformed where
  show (QualifiedIdentifier x modifier) =
    "I expected an unqualified symbol, but got: " ++ pretty x
      ++ formatModifier modifier
  show (InvalidIdentifier x modifier) =
    "I expected a symbol, but got: " ++ pretty x
      ++ formatModifier modifier
  show (InvalidArguments x modifier) =
    "`I expected an array of valid arguments, but got: " ++ pretty x
      ++ formatModifier modifier
  show (InvalidBody x modifier) =
    "I expected a valid definition body, but got: " ++ pretty x
      ++ formatModifier modifier
  show (InvalidCondition x modifier) =
    "I expected a boolean condition, but got: " ++ pretty x
      ++ formatModifier modifier
  show (InvalidType x modifier) =
    "I expected the name of a type, but got: " ++ pretty x
      ++ formatModifier modifier
  show (InvalidBindings bindings modifier) =
    "Expected an array of name-value binding pairs, but got: " ++ pretty bindings
      ++ formatModifier modifier
  show (UnevenForms forms len modifier) =
    "Expected an even number of forms, but got: " ++ pretty forms
      ++ "of length "
      ++ show len
      ++ " "
      ++ formatModifier modifier
  show (InsufficientArguments form lenExpected lenRecieved params) =
    let name = case form of
          (DynamicFnPat sym _ _) -> getName sym
          (MacroPat sym _ _) -> getName sym
          (CommandPat _ sym _) -> getName sym
          (PrimitivePat _ sym _) -> getName sym
          XObj Ref _ _ -> "ref"
          _ -> pretty form
     in name ++ " expected "
          ++ show lenExpected
          ++ " arguments but received only "
          ++ show lenRecieved
          ++ ".\n\nYou’ll have to provide "
          ++ intercalate ", " (map pretty (drop lenRecieved params))
          ++ " as well."
  show (TooManyArguments form lenExpected lenRecieved args) =
    let name = case form of
          (DynamicFnPat sym _ _) -> getName sym
          (MacroPat sym _ _) -> getName sym
          (CommandPat _ sym _) -> getName sym
          (PrimitivePat _ sym _) -> getName sym
          XObj Ref _ _ -> "ref"
          _ -> pretty form
     in name ++ " expected "
          ++ show lenExpected
          ++ " arguments but received "
          ++ show lenRecieved
          ++ ".\n\nThe arguments "
          ++ intercalate ", " (map pretty (drop lenExpected args))
          ++ " are not needed."
  show (InvalidApplication xobj) =
    "Expected a function or macro, but got: " ++ pretty xobj
  show (DoMissingForms) =
    "Expected one or more forms in a `do` form, but got none."
  show (GenericMalformed x) =
    "The form: " ++ pretty x ++ " is malformed"

-- | Specific errors for particular types of malformed expressions.
data Modifier
  = DefnQualifiedSyms XObj
  | DefnNonArrayArgs XObj
  | DefnNonSymArgs XObj
  | IfInvalidCondition XObj
  | WhileInvalidCondition XObj
  | TheInvalidType XObj
  | LetMalformedBinding XObj
  | LetUnevenForms XObj
  | LetNonArrayBindings XObj
  | FnQualifiedSyms XObj
  | FnNonArrayArgs XObj
  | FnNonSymArgs XObj
  | InvalidWith XObj
  | None

instance Show Modifier where
  show None = ""
  show (DefnQualifiedSyms arg) =
    "`defn` requires all of its arguments to be unqualified symbols, but the arugment: "
      ++ pretty arg
      ++ " is qualified"
  show (DefnNonArrayArgs args) =
    "`defn` requires an array of arugments, but it got: " ++ pretty args
  show (DefnNonSymArgs arg) =
    "`defn` requires an array of symbols as arguments, but the argument: "
      ++ pretty arg
      ++ " is not a symbol"
  show (IfInvalidCondition cond) =
    "`if` requires a condition that can be evaluated to a boolean, but it got: "
      ++ pretty cond
      ++ " which cannot resolve to a boolean value."
  show (WhileInvalidCondition cond) =
    "`while` requires a condition that can be evaluated to a boolean, but it got: "
      ++ pretty cond
      ++ " which cannot resolve to a boolean value."
  show (TheInvalidType t) =
    "`the` requires a valid type name, but it got: "
      ++ pretty t
      ++ " which is not a valid type name"
  show (LetMalformedBinding bind) =
    "`let` requires name-value binding pairs, but it got: " ++ pretty bind
      ++ " as a binding name, which is invalid. Binding names must be symbols"
  show (LetUnevenForms arr) =
    "`let` requires an even number of name-value bindings forms, but it got: " ++ pretty arr
  show (LetNonArrayBindings invalid) =
    "`let` requires an array of bindings, but it got: " ++ pretty invalid
  show (FnQualifiedSyms arg) =
    "`fn` requires all of its arguments to be unqualified symbols, but the arugment: "
      ++ pretty arg
      ++ " is qualified"
  show (FnNonArrayArgs args) =
    "`fn` requires an array of arugments, but it got: " ++ pretty args
  show (FnNonSymArgs arg) =
    "`fn` requires an array of symbols as arguments, but the argument: "
      ++ pretty arg
      ++ " is not a symbol"
  show (InvalidWith x) =
    "`with` requires a symbol as an arugment, but got: " ++ pretty x

formatModifier :: Modifier -> String
formatModifier None = ""
formatModifier m = "\n  - " ++ show m

-- | Format a malformed form error for printing.
format :: Malformed -> String
format e = "[ERROR] " ++ show e

--------------------------------------------------------------------------------
-- Validation functions

-- | Validate a given XObj to ensure it is a well formed expression of the language.
validate :: [XObj] -> Either Malformed [XObj]
validate xs =
  case xs of
    DefPat _ _ _ -> validateDef xs
    DefnPat _ _ _ _ -> validateDefn xs
    IfPat _ _ _ _ -> validateIf xs
    ThePat _ _ _ -> validateThe xs
    LetPat _ _ _ -> validateLet xs
    FnPat _ _ _ -> validateFn xs
    WithPat _ _ _ -> validateWith xs
    DoPat _ _ -> validateDo xs
    WhilePat _ _ _ -> validateWhile xs
    -- There are a number of application patterns (the "has static call patterns")
    -- that are formally caught at evaluation time.
    AppPat (ClosurePat _ _ _) _ -> validateApp xs
    AppPat (DynamicFnPat _ _ _) _ -> validateApp xs
    AppPat (MacroPat _ _ _) _ -> validateApp xs
    AppPat (CommandPat _ _ _) _ -> validateApp xs
    AppPat (PrimitivePat _ _ _) _ -> validateApp xs
    AppPat (XObj Ref _ _) _ -> validateApp xs
    _ -> Right xs

-- TODO: Complete validation of if currently relies on evaluating its condition
-- for truthiness But there is a class of list forms we can rule out purely
-- symbolically, e.g. `def`, etc..
validateIf :: [XObj] -> Either Malformed [XObj]
validateIf x@(IfPat _ (ListPat _) _ _) = Right x -- needs further evaluation
validateIf (IfPat _ invalid@(ArrPat _) _ _) = Left (InvalidCondition invalid (IfInvalidCondition invalid))
validateIf x@(IfPat _ cond _ _)
  | isSym cond = Right x -- needs further evaluation
  | isBool cond = Right x
  | otherwise = Left (InvalidCondition cond (IfInvalidCondition cond))
validateIf invalid = Left (GenericMalformed (XObj (Lst invalid) Nothing Nothing))

-- | Validation of (while cond body) expressions.
validateWhile :: [XObj] -> Either Malformed [XObj]
validateWhile x@(WhilePat _ (ListPat _) _) = Right x -- needs further evaluation
validateWhile (WhilePat _ invalid@(ArrPat _) _) = Left (InvalidCondition invalid (WhileInvalidCondition invalid))
validateWhile x@(WhilePat _ cond _)
  | isSym cond = Right x -- needs further evaluation
  | isBool cond = Right x
  | otherwise = Left (InvalidCondition cond (WhileInvalidCondition cond))
validateWhile invalid = Left (GenericMalformed (XObj (Lst invalid) Nothing Nothing))

-- | Validation of (def name value) expressions.
validateDef :: [XObj] -> Either Malformed [XObj]
validateDef x@(DefPat _ (UnqualifiedSymPat _) _) = Right x
validateDef (DefPat _ invalid@(SymPat _ _) _) = Left (QualifiedIdentifier invalid None)
validateDef (DefPat _ invalid _) = Left (InvalidIdentifier invalid None)
validateDef def = Left (GenericMalformed (XObj (Lst def) Nothing Nothing))

-- | Validation of (defn name [args] body) expressions.
validateDefn :: [XObj] -> Either Malformed [XObj]
validateDefn x@(DefnPat _ (UnqualifiedSymPat _) arr@(ArrPat args) _)
  | not (all isSym args) = Left (InvalidArguments arr (DefnNonSymArgs (head (remove isSym args))))
  | not (all isUnqualifiedSym args) =
    Left (InvalidArguments arr (DefnQualifiedSyms (head (remove isUnqualifiedSym args))))
  | otherwise = pure x
validateDefn (DefnPat _ (UnqualifiedSymPat _) invalid _) =
  Left (InvalidArguments invalid (DefnNonArrayArgs invalid))
validateDefn (DefnPat _ invalid@(SymPat _ _) _ _) = Left (QualifiedIdentifier invalid None)
validateDefn (DefnPat _ invalid _ _) = Left (InvalidIdentifier invalid None)
validateDefn defn = Left (GenericMalformed (XObj (Lst defn) Nothing Nothing))

-- | Validation of (the type body) expressions
validateThe :: [XObj] -> Either Malformed [XObj]
validateThe x@(ThePat _ t _) =
  case xobjToTy t of
    Nothing -> Left (InvalidType t (TheInvalidType t))
    Just _ -> Right x
validateThe the = Left (GenericMalformed (XObj (Lst the) Nothing Nothing))

-- | Validation of (let [bindings] body) expressions.
validateLet :: [XObj] -> Either Malformed [XObj]
validateLet x@(LetPat _ arr@(ArrPat binds) _)
  | odd (length binds) =
    Left (UnevenForms arr (length binds) (LetUnevenForms arr))
  | not (all isSym (evenIndices binds)) =
    Left (InvalidBindings arr (LetMalformedBinding (head (remove isSym (evenIndices binds)))))
  | otherwise = Right x
validateLet (LetPat _ invalid _) = Left (InvalidBindings invalid (LetNonArrayBindings invalid))
validateLet lett = Left (GenericMalformed (XObj (Lst lett) Nothing Nothing))

-- | Validation of (fn [args] body) expressions.
validateFn :: [XObj] -> Either Malformed [XObj]
validateFn x@(FnPat _ arr@(ArrPat args) _)
  | not (all isSym args) = Left (InvalidArguments arr (FnNonSymArgs (head (remove isSym args))))
  | not (all isUnqualifiedSym args) =
    Left (InvalidArguments arr (FnQualifiedSyms (head (remove isUnqualifiedSym args))))
  | otherwise = pure x
validateFn (FnPat _ invalid _) = Left (InvalidArguments invalid (FnNonArrayArgs invalid))
validateFn fn = Left (GenericMalformed (XObj (Lst fn) Nothing Nothing))

-- | Validation of (do body) expressions.
validateDo :: [XObj] -> Either Malformed [XObj]
validateDo x@(DoPat _ forms) =
  case forms of
    [] -> Left DoMissingForms
    _ -> Right x
validateDo doo = Left (GenericMalformed (XObj (Lst doo) Nothing Nothing))

-- | Validation of (function arguments) function applications.
validateApp :: [XObj] -> Either Malformed [XObj]
-- Special case for Refs
validateApp x@(AppPat f@(XObj Ref _ _) args) =
  checkAppArity f [(XObj (Sym (SymPath [] "x") Symbol) Nothing Nothing)] args >> Right x
validateApp x@(AppPat f@(ClosurePat params _ _) args) =
  checkAppArity f params args >> Right x
validateApp x@(AppPat f@(DynamicFnPat _ params _) args) =
  checkAppArity f params args >> Right x
validateApp x@(AppPat f@(MacroPat _ params _) args) =
  checkAppArity f params args >> Right x
validateApp x@(AppPat f@(CommandPat arity _ _) args) =
  case arity of
    (NullaryCommandFunction _) -> checkAppArity f p args >> Right x
    (UnaryCommandFunction _) -> checkAppArity f p args >> Right x
    (BinaryCommandFunction _) -> checkAppArity f p args >> Right x
    (TernaryCommandFunction _) -> checkAppArity f p args >> Right x
    (VariadicCommandFunction _) -> Right x
  where
    p = (\y -> XObj (Sym (SymPath [] y) Symbol) Nothing Nothing) <$> argnames
    argnames =
      case arity of
        NullaryCommandFunction _ -> []
        UnaryCommandFunction _ -> ["x"]
        BinaryCommandFunction _ -> ["x", "y"]
        TernaryCommandFunction _ -> ["x", "y", "z"]
        VariadicCommandFunction _ -> []
validateApp x@(AppPat f@(PrimitivePat arity _ _) args) =
  case arity of
    (NullaryPrimitive _) -> checkAppArity f p args >> Right x
    (UnaryPrimitive _) -> checkAppArity f p args >> Right x
    (BinaryPrimitive _) -> checkAppArity f p args >> Right x
    (TernaryPrimitive _) -> checkAppArity f p args >> Right x
    (QuaternaryPrimitive _) -> checkAppArity f p args >> Right x
    (VariadicPrimitive _) -> Right x
  where
    p = (\y -> XObj (Sym (SymPath [] y) Symbol) Nothing Nothing) <$> argnames
    argnames =
      case arity of
        NullaryPrimitive _ -> []
        UnaryPrimitive _ -> ["x"]
        BinaryPrimitive _ -> ["x", "y"]
        TernaryPrimitive _ -> ["x", "y", "z"]
        QuaternaryPrimitive _ -> ["x", "y", "z", "w"]
        VariadicPrimitive _ -> []
validateApp (AppPat invalid _) = Left (InvalidApplication invalid)
validateApp app = Left (GenericMalformed (XObj (Lst app) Nothing Nothing))

-- | Validation of (with module body) expressions
validateWith :: [XObj] -> Either Malformed [XObj]
validateWith x@(WithPat _ (SymPat _ _) _) = Right x
validateWith (WithPat _ invalid _) = Left (InvalidIdentifier invalid (InvalidWith invalid))
validateWith with = Left (GenericMalformed (XObj (Lst with) Nothing Nothing))

-- | Checks that the number of arguments passed to a function are correct.
checkAppArity :: XObj -> [XObj] -> [XObj] -> Either Malformed ()
checkAppArity xobj params args =
  let la = length args
      withRest = any ((":rest" ==) . getName) params
      lp = length params - (if withRest then 2 else 0)
   in if lp == la || (withRest && la >= lp)
        then Right ()
        else
          if la < lp
            then Left (InsufficientArguments xobj lp la params)
            else Left (TooManyArguments xobj lp la args)

--------------------------------------------------------------------------------
-- Pattern Synonyms

pattern ArrPat :: [XObj] -> XObj
pattern ArrPat members <- XObj (Arr members) _ _

pattern StaticArrPat :: [XObj] -> XObj
pattern StaticArrPat members <- XObj (StaticArr members) _ _

pattern ListPat :: [XObj] -> XObj
pattern ListPat members <- XObj (Lst members) _ _

pattern SymPat :: SymPath -> SymbolMode -> XObj
pattern SymPat path mode <- XObj (Sym path mode) _ _

pattern UnqualifiedSymPat :: SymPath -> XObj
pattern UnqualifiedSymPat path <- XObj (Sym path@(SymPath [] _) _) _ _

pattern DefPat :: XObj -> XObj -> XObj -> [XObj]
pattern DefPat def name value <- [def@(XObj Def _ _), name, value]

pattern DefnPat :: XObj -> XObj -> XObj -> XObj -> [XObj]
pattern DefnPat defn name args body <- [defn@(XObj (Defn _) _ _), name, args, body]

pattern IfPat :: XObj -> XObj -> XObj -> XObj -> [XObj]
pattern IfPat ifHead cond true false <- [ifHead@(XObj If _ _), cond, true, false]

pattern ThePat :: XObj -> XObj -> XObj -> [XObj]
pattern ThePat theHead t value <- [theHead@(XObj The _ _), t, value]

pattern RefPat :: XObj -> XObj -> [XObj]
pattern RefPat refHead value <- [refHead@(XObj Ref _ _), value]

pattern LetPat :: XObj -> XObj -> XObj -> [XObj]
pattern LetPat letHead bindings body <- [letHead@(XObj Let _ _), bindings, body]

pattern FnPat :: XObj -> XObj -> XObj -> [XObj]
pattern FnPat fnHead args body <- [fnHead@(XObj (Fn _ _) _ _), args, body]

pattern WithPat :: XObj -> XObj -> [XObj] -> [XObj]
pattern WithPat withHead sym forms <- (withHead@(XObj With _ _) : sym : forms)

pattern DoPat :: XObj -> [XObj] -> [XObj]
pattern DoPat doHead forms <- (doHead@(XObj Do _ _) : forms)

pattern WhilePat :: XObj -> XObj -> XObj -> [XObj]
pattern WhilePat whileHead cond body <- [whileHead@(XObj While _ _), cond, body]

pattern SetPat :: XObj -> XObj -> XObj -> [XObj]
pattern SetPat setBangHead iden value <- [setBangHead@(XObj SetBang _ _), iden, value]

pattern ClosurePat :: [XObj] -> XObj -> Context -> XObj
pattern ClosurePat params body ctx <- XObj (Closure (XObj (Lst [_, (ArrPat params), body]) _ _) (CCtx ctx)) _ _

pattern DynamicFnPat :: XObj -> [XObj] -> XObj -> XObj
pattern DynamicFnPat sym params body <- XObj (Lst [XObj Dynamic _ _, sym, (ArrPat params), body]) _ _

pattern MacroPat :: XObj -> [XObj] -> XObj -> XObj
pattern MacroPat sym params body <- XObj (Lst [XObj Macro _ _, sym, (ArrPat params), body]) _ _

pattern CommandPat :: CommandFunctionType -> XObj -> [XObj] -> XObj
pattern CommandPat arity sym params <- XObj (Lst [XObj (Command arity) _ _, sym, (ArrPat params)]) _ _

pattern PrimitivePat :: PrimitiveFunctionType -> XObj -> [XObj] -> XObj
pattern PrimitivePat arity sym params <- XObj (Lst [XObj (Primitive arity) _ _, sym, (ArrPat params)]) _ _

pattern AppPat :: XObj -> [XObj] -> [XObj]
pattern AppPat f args <- (f : args)

pattern InterfaceSymPat :: String -> XObj
pattern InterfaceSymPat name <- XObj (InterfaceSym name) _ _

pattern MultiSymPat :: String -> [SymPath] -> XObj
pattern MultiSymPat name candidates <- XObj (MultiSym name candidates) _ _

pattern MatchPat :: XObj -> XObj -> [XObj] -> [XObj]
pattern MatchPat match value stanzas <- (match@(XObj (Match _) _ _) : value : stanzas)

pattern InterfacePat :: Ty -> [SymPath] -> [XObj]
pattern InterfacePat ty paths <- [XObj (Interface ty paths) _ _, _]
