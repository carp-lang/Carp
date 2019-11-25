module Emit (toC,
             envToC,
             globalsToC,
             projectIncludesToC,
             envToDeclarations,
             checkForUnresolvedSymbols,
             ToCMode(..),
             wrapInInitFunction
            ) where

import Data.List (intercalate, sortOn)
import Control.Monad.State
import Control.Monad (when, zipWithM_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Debug.Trace

import Obj
import Types
import Util
import Template
import Scoring
import Lookup
import Concretize

addIndent :: Int -> String
addIndent n = replicate n ' '

indentAmount :: Int
indentAmount = 4

data ToCError = InvalidParameter XObj
              | InvalidList XObj
              | DontVisitObj XObj
              | CannotEmitUnit XObj
              | CannotEmitExternal XObj
              | CannotEmitModKeyword XObj
              | BinderIsMissingType Binder
              | UnresolvedMultiSymbol XObj
              | UnresolvedInterfaceSymbol XObj
              | UnresolvedGenericType XObj
              | CannotSet XObj

instance Show ToCError where
  show (InvalidParameter xobj) =
    "I encountered an invalid parameter `" ++ show (obj xobj) ++ "` at " ++
    prettyInfoFromXObj xobj ++ "."
  show (InvalidList xobj) =
    "I encountered an invalid list `" ++ show (obj xobj) ++ "` at " ++
    prettyInfoFromXObj xobj ++ "."
  show (DontVisitObj xobj) =
    "I can’t visit " ++ show (obj xobj) ++ " at " ++ prettyInfoFromXObj xobj ++
    "."
  show (CannotEmitUnit xobj) =
    "I can't emit code for the unit type `()` at" ++ prettyInfoFromXObj xobj ++
    "."
  show (CannotEmitExternal xobj) =
    "I can’t emit code for the external function/variable `" ++
    show (obj xobj) ++ "` at " ++ prettyInfoFromXObj xobj ++ "."
  show (CannotEmitModKeyword xobj) =
    "I can’t emit code for the module `" ++ show (obj xobj) ++ "` at " ++
    prettyInfoFromXObj xobj ++ "."
  show (BinderIsMissingType b) =
    "I encountered a binder `" ++ show b ++ "` that is missing its type."
  show (UnresolvedMultiSymbol xobj@(XObj (MultiSym symName symPaths) _ _)) =
    "I found an ambiguous symbol `" ++ symName ++
    "` at " ++ prettyInfoFromXObj xobj ++ "\n\nPossibilities:\n  " ++
    joinWith "\n  " (map show symPaths) ++
    "\n\nAll possibilities have the correct type."
  show (UnresolvedInterfaceSymbol xobj@(XObj (InterfaceSym symName) _ _)) =
    "I found an interface `" ++ symName ++
    "` that is unresolved in the context at " ++ prettyInfoFromXObj xobj
  show (UnresolvedGenericType xobj@(XObj _ _ (Just t))) =
    "I found an unresolved generic type `" ++ show t ++
    "` for the expression `" ++ show xobj ++ "` at " ++ prettyInfoFromXObj xobj
  show (CannotSet xobj) =
    "I can’t emit code for setting `" ++ pretty xobj ++ "` at " ++
    prettyInfoFromXObj xobj ++ "\n\nOnly variables can be reset using `set!`."

data ToCMode = Functions | Globals | All deriving Show

newtype EmitterState = EmitterState { emitterSrc :: String }

appendToSrc :: String -> State EmitterState ()
appendToSrc moreSrc = modify (\s -> s { emitterSrc = emitterSrc s ++ moreSrc })

toC :: ToCMode -> Binder -> String
toC toCMode (Binder meta root) = emitterSrc (execState (visit startingIndent root) (EmitterState ""))
  where startingIndent = case toCMode of
                           Functions -> 0
                           Globals -> 4
                           All -> 0
        visit :: Int -> XObj -> State EmitterState String
        visit indent xobj =
          case obj xobj of
            Lst _   -> visitList indent xobj
            Arr _   -> visitArray indent xobj
            Num IntTy num -> return (show (round num :: Int))
            Num LongTy num -> return (show (round num :: Int) ++ "l")
            Num FloatTy num -> return (show num ++ "f")
            Num DoubleTy num -> return (show num)
            Num _ _ -> error "Can't emit invalid number type."
            Bol b -> return (if b then "true" else "false")
            Str _ -> visitString indent xobj
            Pattern _ -> visitString indent xobj
            Chr c -> return $ case c of
                                '\t' -> "'\\t'"
                                '\n' -> "'\\n'"
                                '\\' -> "'\\\\'"
                                x -> ['\'', x, '\'']
            Sym _ _ -> visitSymbol indent xobj
            (Defn _) -> error (show (DontVisitObj xobj))
            Def -> error (show (DontVisitObj xobj))
            Let -> error (show (DontVisitObj xobj))
            If -> error (show (DontVisitObj xobj))
            Break -> error (show (DontVisitObj xobj))
            While -> error (show (DontVisitObj xobj))
            Do -> error (show (DontVisitObj xobj))
            e@(Deftype _) -> error (show (DontVisitObj xobj))
            e@(DefSumtype _) -> error (show (DontVisitObj xobj))
            Mod _ -> error (show (CannotEmitModKeyword xobj))
            External _ -> error (show (CannotEmitExternal xobj))
            ExternalType -> error (show (DontVisitObj xobj))
            e@(Command _) -> error (show (DontVisitObj xobj))
            e@(Deftemplate _) ->  error (show (DontVisitObj xobj))
            e@(Instantiate _) ->  error (show (DontVisitObj xobj))
            e@(Defalias _) -> error (show (DontVisitObj xobj))
            e@(MultiSym _ _) -> error (show (DontVisitObj xobj))
            e@(InterfaceSym _) -> error (show (DontVisitObj xobj))
            Address -> error (show (DontVisitObj xobj))
            SetBang -> error (show (DontVisitObj xobj))
            Macro -> error (show (DontVisitObj xobj))
            Dynamic -> error (show (DontVisitObj xobj))
            DefDynamic -> error (show (DontVisitObj xobj))
            The -> error (show (DontVisitObj xobj))
            Ref -> error (show (DontVisitObj xobj))
            Deref -> error (show (DontVisitObj xobj))
            e@(Interface _ _) -> error (show (DontVisitObj xobj))

        visitStr' indent str i =
          -- | This will allocate a new string every time the code runs:
          -- do let var = freshVar i
          --    appendToSrc (addIndent indent ++ "String " ++ var ++ " = strdup(\"" ++ str ++ "\");\n")
          --    return var
          -- | This will use the statically allocated string in the C binary (can't be freed):
          do let var = freshVar i
                 varRef = freshVar i ++ "_ref";
             appendToSrc (addIndent indent ++ "static String " ++ var ++ " = \"" ++ escapeString str ++ "\";\n")
             appendToSrc (addIndent indent ++ "String *" ++ varRef ++ " = &" ++ var ++ ";\n")
             return varRef
        visitString indent (XObj (Str str) (Just i) _) = visitStr' indent str i
        visitString indent (XObj (Pattern str) (Just i) _) = visitStr' indent str i
        visitString _ _ = error "Not a string."
        escaper '\"' acc = "\\\"" ++ acc
        escaper '\n' acc = "\\n" ++ acc
        escaper x acc = x : acc
        escapeString = foldr escaper ""

        visitSymbol :: Int -> XObj -> State EmitterState String
        visitSymbol _ xobj@(XObj (Sym _ (LookupGlobalOverride overrideWithName)) _ t) =
          return overrideWithName
        visitSymbol indent xobj@(XObj sym@(Sym path lookupMode) (Just i) t) =
          let Just t' = t
          in if isTypeGeneric t'
             then error ("Can't emit symbol of generic type: " ++
                         show path ++ " : " ++ show t' ++ " at " ++ prettyInfoFromXObj xobj)
             else if isFunctionType t' && not (isLookupLocal lookupMode) && not (isGlobalVariableLookup lookupMode)
                  then do let var = freshVar i
                          appendToSrc (addIndent indent ++ "Lambda " ++ var ++ " = { .callback = " ++ pathToC path ++ ", .env = NULL, .delete = NULL, .copy = NULL }; //" ++ show sym ++ "\n")
                          return var
                  else case lookupMode of
                         LookupLocal Capture -> return ("_env->" ++ pathToC path)
                         _ -> return (pathToC path)

        visitSymbol _ xobj@(XObj (Sym path _) Nothing _) = error ("Symbol missing info: " ++ show xobj)
        visitSymbol _ _ = error "Not a symbol."

        visitList :: Int -> XObj -> State EmitterState String
        visitList indent (XObj (Lst xobjs) (Just i) t) =
          case xobjs of
            -- Defn
            [XObj (Defn _) _ _, XObj (Sym path@(SymPath _ name) _) _ _, XObj (Arr argList) _ _, body] ->
              case toCMode of
                Globals ->
                  return ""
                _ ->
                  do let innerIndent = indent + indentAmount
                         Just (FuncTy _ _ retTy) = t
                         defnDecl = defnToDeclaration meta path argList retTy
                     appendToSrc (defnDecl ++ " {\n")
                     when (name == "main") $
                       appendToSrc (addIndent innerIndent ++ "carp_init_globals(argc, argv);\n")
                     ret <- visit innerIndent body
                     delete innerIndent i
                     when (retTy /= UnitTy) $
                       appendToSrc (addIndent innerIndent ++ "return " ++ ret ++ ";\n")
                     appendToSrc "}\n\n"
                     return ""

            -- Fn / λ
            [XObj (Fn name set) _ _, XObj (Arr argList) _ _, body] ->
              do let retVar = freshVar i
                     capturedVars = Set.toList set
                     Just callback = name
                     callbackMangled = pathToC callback
                     needEnv = not (null capturedVars)
                     lambdaEnvTypeName = callbackMangled ++ "_env" -- The name of the struct is the callback name with suffix '_env'.
                     lambdaEnvType = StructTy lambdaEnvTypeName []
                     lambdaEnvName = freshVar i ++ "_env"
                 appendToSrc (addIndent indent ++ "// This lambda captures " ++
                              show (length capturedVars) ++ " variables: " ++
                              joinWithComma (map getName capturedVars) ++ "\n")
                 when needEnv $
                   do appendToSrc (addIndent indent ++ tyToC lambdaEnvType ++ " *" ++ lambdaEnvName ++
                                   " = CARP_MALLOC(sizeof(" ++ tyToC lambdaEnvType ++ "));\n")
                      mapM_ (\(XObj (Sym path _) _ _) ->
                               appendToSrc (addIndent indent ++ lambdaEnvName ++ "->" ++
                                            pathToC path ++ " = " ++ pathToC path ++ ";\n"))
                        capturedVars
                 appendToSrc (addIndent indent ++ "Lambda " ++ retVar ++ " = {\n")
                 appendToSrc (addIndent indent ++ "  .callback = " ++ callbackMangled ++ ",\n")
                 appendToSrc (addIndent indent ++ "  .env = " ++ (if needEnv then lambdaEnvName else "NULL")  ++ ",\n")
                 appendToSrc (addIndent indent ++ "  .delete = " ++ (if needEnv then "" ++ lambdaEnvTypeName ++ "_delete" else "NULL")  ++ ",\n")
                 appendToSrc (addIndent indent ++ "  .copy = " ++ (if needEnv then "" ++ lambdaEnvTypeName ++ "_copy" else "NULL")  ++ "\n")
                 appendToSrc (addIndent indent ++ "};\n")
                 return retVar

            -- Def
            [XObj Def _ _, XObj (Sym path _) _ _, expr] ->
              case toCMode of
                Functions ->
                  return ""
                _ ->
                  do appendToSrc (addIndent indent ++ "{\n")
                     let innerIndent = indent + indentAmount
                     ret <- visit innerIndent expr
                     appendToSrc (addIndent innerIndent ++ pathToC path ++ " = " ++ ret ++ ";\n")
                     delete innerIndent i
                     appendToSrc (addIndent indent ++ "}\n")
                     return ""

            -- Let
            [XObj Let _ _, XObj (Arr bindings) _ _, body] ->
              let indent' = indent + indentAmount
              in  do let Just bodyTy = ty body
                         isNotVoid = bodyTy /= UnitTy
                         letBodyRet = freshVar i
                     when isNotVoid $ -- Must be declared outside the scope
                       appendToSrc (addIndent indent ++ tyToCLambdaFix bodyTy ++ " " ++ letBodyRet ++ ";\n")
                     appendToSrc (addIndent indent ++ "/* let */ {\n")
                     let letBindingToC (XObj (Sym (SymPath _ symName) _) _ _) expr =
                           do ret <- visit indent' expr
                              let Just bindingTy = ty expr
                              when (bindingTy /= UnitTy) $
                                appendToSrc (addIndent indent' ++ tyToCLambdaFix bindingTy ++ " " ++ mangle symName ++ " = " ++ ret ++ ";\n")
                         letBindingToC _ _ = error "Invalid binding."
                     mapM_ (uncurry letBindingToC) (pairwise bindings)
                     ret <- visit indent' body
                     when isNotVoid $
                       appendToSrc (addIndent indent' ++ letBodyRet ++ " = " ++ ret ++ ";\n")
                     delete indent' i
                     appendToSrc (addIndent indent ++ "}\n")
                     return letBodyRet

            -- If
            [XObj If _ _, expr, ifTrue, ifFalse] ->
              let indent' = indent + indentAmount
              in  do let isNotVoid = ty ifTrue /= Just UnitTy
                         ifRetVar = freshVar i
                     when isNotVoid $
                       let Just ifT = ty ifTrue
                       in  appendToSrc (addIndent indent ++ tyToCLambdaFix ifT ++ " " ++ ifRetVar ++ ";\n")
                     exprVar <- visit indent expr
                     appendToSrc (addIndent indent ++ "if (" ++ exprVar ++ ") {\n")
                     trueVar <- visit indent' ifTrue
                     let Just ifTrueInfo = info ifTrue
                     delete indent' ifTrueInfo
                     when isNotVoid $
                       appendToSrc (addIndent indent' ++ ifRetVar ++ " = " ++ trueVar ++ ";\n")
                     appendToSrc (addIndent indent ++ "} else {\n")
                     falseVar <- visit indent' ifFalse
                     let Just ifFalseInfo = info ifFalse
                     delete indent' ifFalseInfo
                     when isNotVoid $
                       appendToSrc (addIndent indent' ++ ifRetVar ++ " = " ++ falseVar ++ ";\n")
                     appendToSrc (addIndent indent ++ "}\n")
                     return ifRetVar

            -- Match
            XObj Match _ _ : expr@(XObj _ (Just exprInfo) (Just exprTy)) : rest ->
              let indent' = indent + indentAmount
                  retVar = freshVar i
                  isNotVoid = t /= Just UnitTy
                  sumTypeAsPath = SymPath [] (show exprTy)

                  tempVarToAvoidClash = freshVar exprInfo ++ "_temp";

                  emitCaseMatcher :: String -> XObj -> Integer -> State EmitterState ()
                  emitCaseMatcher caseName (XObj (Sym path _) i t) index =
                    let Just tt = t
                    in  appendToSrc (addIndent indent' ++ tyToCLambdaFix tt ++ " " ++
                                     pathToC path ++ " = " ++ tempVarToAvoidClash ++ "." ++ mangle caseName ++
                                     ".member" ++ show index ++ ";\n")

                  emitCase :: String -> Bool -> (XObj, XObj) -> State EmitterState ()
                  emitCase exprVar isFirst (caseLhs@(XObj (Lst (XObj (Sym firstPath@(SymPath _ caseName@(firstLetter : _)) _) _ _ : caseMatchers)) caseLhsInfo _), caseExpr) =
                    -- A list of things, beginning with a tag
                    do appendToSrc (addIndent indent)
                       unless isFirst (appendToSrc "else ")
                       -- HACK! The function 'removeSuffix' ignores the type specialisation of the tag name and just uses the base name
                       -- A better idea is to not specialise the names, which happens when calling 'concretize' on the lhs
                       -- This requires a bunch of extra machinery though, so this will do for now...
                       appendToSrc ("if(" ++ exprVar ++ "._tag == " ++ tagName exprTy (removeSuffix caseName) ++ ") {\n")
                       appendToSrc (addIndent indent' ++ tyToCLambdaFix exprTy ++ " " ++
                                    tempVarToAvoidClash ++ " = " ++ exprVar ++ ";\n")
                       zipWithM_ (emitCaseMatcher (removeSuffix caseName)) caseMatchers [0..]
                       caseExprRetVal <- visit indent' caseExpr
                       when isNotVoid $
                         appendToSrc (addIndent indent' ++ retVar ++ " = " ++ caseExprRetVal ++ ";\n")
                       let Just caseLhsInfo' = caseLhsInfo
                       delete indent' caseLhsInfo'
                       appendToSrc (addIndent indent ++ "}\n")
                  emitCase exprVar isFirst (XObj (Sym firstPath _) caseLhsInfo _, caseExpr) =
                    -- Single variable
                    do appendToSrc (addIndent indent)
                       unless isFirst (appendToSrc "else ")
                       appendToSrc "if(true) {\n"
                       appendToSrc (addIndent indent' ++ tyToCLambdaFix exprTy ++ " " ++
                                    tempVarToAvoidClash ++ " = " ++ exprVar ++ ";\n")
                       appendToSrc (addIndent indent' ++ tyToCLambdaFix exprTy ++ " " ++
                                    pathToC firstPath ++ " = " ++ tempVarToAvoidClash ++ ";\n") -- Store the whole expr in a variable
                       caseExprRetVal <- visit indent' caseExpr
                       when isNotVoid $
                         appendToSrc (addIndent indent' ++ retVar ++ " = " ++ caseExprRetVal ++ ";\n")
                       let Just caseLhsInfo' = caseLhsInfo
                       delete indent' caseLhsInfo'
                       appendToSrc (addIndent indent ++ "}\n")

              in  do exprVar <- visit indent expr
                     when isNotVoid $
                       let Just tt = t
                       in  appendToSrc (addIndent indent ++ tyToCLambdaFix tt ++ " " ++ retVar ++ ";\n")
                     zipWithM_ (emitCase exprVar) (True : repeat False) (pairwise rest)
                     appendToSrc (addIndent indent ++ "else {\n")
                     appendToSrc (addIndent indent ++ "  // This will not be needed with static exhaustiveness checking in 'match' expressions:\n")
                     appendToSrc (addIndent indent ++ "  fprintf(stderr, \"Unhandled case in 'match' expression at " ++ quoteBackslashes (prettyInfo i) ++ "\\n\");\n")
                     appendToSrc (addIndent indent ++ "  exit(1);\n")
                     appendToSrc (addIndent indent ++ "}\n")
                     return retVar
              where quoteBackslashes [] = []
                    quoteBackslashes ('\\':r) = "\\\\" ++ quoteBackslashes r
                    quoteBackslashes (x:r) = x : quoteBackslashes r

            XObj Match _ _ : _ ->
              error "Fell through match."

            -- While
            [XObj While _ _, expr, body] ->
              let indent' = indent + indentAmount
                  Just exprTy = ty expr
                  conditionVar = freshVar i
                  Just exprInfo = info expr
              in  do exprRetVar <- visitWhileExpression indent
                     appendToSrc (addIndent indent ++ tyToCLambdaFix exprTy ++ " " ++ conditionVar ++ " = " ++ exprRetVar ++ ";\n")
                     delete indent exprInfo
                     appendToSrc (addIndent indent ++ "while (" ++ conditionVar ++ ") {\n")
                     _ <- visit indent' body
                     exprRetVar' <- visitWhileExpression indent'
                     delete indent' i
                     appendToSrc (addIndent indent' ++ conditionVar ++ " = " ++ exprRetVar' ++ ";\n")
                     appendToSrc (addIndent indent ++ "}\n")
                     return ""

                       where visitWhileExpression :: Int -> State EmitterState String
                             visitWhileExpression ind =
                               do s <- get
                                  let (exprRetVar, exprResultState) = runState (visit ind expr) (EmitterState "")
                                      exprSrc = emitterSrc exprResultState
                                  modify (\x -> x { emitterSrc = emitterSrc s ++ exprSrc
                                                  })
                                  return exprRetVar

            -- Do
            XObj Do _ _ : expressions ->
              do let lastExpr = last expressions
                     retVar = freshVar i
                 mapM_ (visit indent) (init expressions)
                 let (Just lastTy) = ty lastExpr
                 if lastTy == UnitTy
                   then do _ <- visit indent lastExpr
                           return ""
                   else do lastRet <- visit indent lastExpr
                           appendToSrc (addIndent indent ++ tyToCLambdaFix lastTy ++ " " ++ retVar ++ " = " ++ lastRet ++ ";\n")
                           return retVar

            -- Address
            [XObj Address _ _, value] ->
              do valueVar <- visit indent value
                 return ("&" ++ valueVar)

            -- Set!
            [XObj SetBang _ _, variable, value] ->
              do valueVar <- visit indent value
                 let properVariableName =
                       case variable of
                         (XObj (Lst (XObj (Sym (SymPath _ "copy") _) _ _ : symObj@(XObj (Sym sym _) _ _) : _)) _ _) -> "*" ++ pathToC sym
                         (XObj (Sym sym _) _ _) -> pathToC sym
                         v -> error (show (CannotSet variable))
                     Just varInfo = info variable
                 --appendToSrc (addIndent indent ++ "// " ++ show (length (infoDelete varInfo)) ++ " deleters for " ++ properVariableName ++ ":\n")
                 delete indent varInfo
                 appendToSrc (addIndent indent ++ properVariableName ++ " = " ++ valueVar ++ "; "
                              ++ " // " ++ show (fromMaybe (VarTy "?") (ty variable)) ++ " = " ++ show (fromMaybe (VarTy "?") (ty value))
                              ++ "\n")
                 return ""

            -- The
            [XObj The _ _, _, value] ->
              do var <- visit indent value
                 let Just t' = t
                     fresh = mangle (freshVar i)
                 appendToSrc (addIndent indent ++ tyToCLambdaFix t' ++ " " ++ fresh ++ " = " ++ var ++ "; // From the 'the' function.\n")
                 return fresh

            -- Ref
            [XObj Ref _ _, value] ->
              do var <- visit indent value
                 let Just t' = t
                     fresh = mangle (freshVar i)
                 if isNumericLiteral value
                   then do let literal = freshVar i ++ "_lit";
                               Just literalTy = ty value
                           appendToSrc (addIndent indent ++ "static " ++ tyToCLambdaFix literalTy ++ " " ++ literal ++ " = " ++ var ++ ";\n")
                           appendToSrc (addIndent indent ++ tyToCLambdaFix t' ++ " " ++ fresh ++ " = &" ++ literal ++ "; // ref\n")
                   else appendToSrc (addIndent indent ++ tyToCLambdaFix t' ++ " " ++ fresh ++ " = &" ++ var ++ "; // ref\n")
                 return fresh

            -- Deref
            [XObj Deref _ _, value] ->
              do x <- visit indent value
                 return ("(*" ++ x ++ ")")

            -- Deftype
            XObj (Deftype _) _ _ : XObj (Sym _ _) _ _ : _ ->
              return ""

            -- DefSumtype
            XObj (DefSumtype _) _ _ : XObj (Sym _ _) _ _ : _ ->
              return ""

            -- Template
            [XObj (Deftemplate _) _ _, XObj (Sym _ _) _ _] ->
              return ""

            [XObj (Instantiate template) _ _, XObj (Sym path _) _ _] ->
              case toCMode of
                Globals ->
                  return ""
                _ ->
                  do let Just t' = t
                     appendToSrc (templateToC template path t')
                     return ""

            -- Alias
            XObj (Defalias _) _ _ : _ ->
              return ""

            -- External
            XObj (External _) _ _ : _ ->
              return ""

            -- Macro
            XObj Macro _ _ : _ ->
              return ""

            -- Dynamic
            XObj Dynamic _ _ : _ ->
              return ""

            -- DefDynamic
            XObj DefDynamic _ _ : _ ->
              return ""

            -- Command
            XObj (Command _) _ _ : _ ->
              return ""

            -- Interface
            XObj (Interface _ _) _ _ : _ ->
              return ""

            -- Break
            [XObj Break _ _] -> do
              appendToSrc (addIndent indent ++ "break;\n")
              return ""

            -- Function application (functions with overridden names)
            func@(XObj (Sym _ (LookupGlobalOverride overriddenName)) _ _) : args ->
              do argListAsC <- createArgList indent True args -- The 'True' means "unwrap lambdas" which is always the case for functions with overriden names (they are external)
                 let funcTy = case ty func of
                               Just actualType -> actualType
                               _ -> error ("No type on func " ++ show func)
                     FuncTy _ argTys retTy = funcTy
                     callFunction = overriddenName ++ "(" ++ argListAsC ++ ");\n"
                 if retTy == UnitTy
                   then do appendToSrc (addIndent indent ++ callFunction)
                           return ""
                   else do let varName = freshVar i
                           appendToSrc (addIndent indent ++ tyToCLambdaFix retTy ++ " " ++ varName ++ " = " ++ callFunction)
                           return varName

            -- Function application (global symbols that are functions -- lambdas stored in def:s need to be called like locals, see below)
            func@(XObj (Sym path (LookupGlobal mode AFunction)) _ _) : args ->
              do argListAsC <- createArgList indent (mode == ExternalCode) args
                 let Just (FuncTy _ _ retTy) = ty func
                     funcToCall = pathToC path
                 if retTy == UnitTy
                   then do appendToSrc (addIndent indent ++ funcToCall ++ "(" ++ argListAsC ++ ");\n")
                           return ""
                   else do let varName = freshVar i
                           appendToSrc (addIndent indent ++ tyToCLambdaFix retTy ++ " " ++ varName ++ " = " ++ funcToCall ++ "(" ++ argListAsC ++ ");\n")
                           return varName

            -- Function application (on local symbols and global defs containing lambdas)
            func : args ->
              do funcToCall <- visit indent func
                 let unwrapLambdas = case func of
                                       XObj (Sym _ (LookupGlobal ExternalCode _)) _ _ -> True
                                       _ -> False
                 argListAsC <- createArgList indent unwrapLambdas args
                 let funcTy = case ty func of
                               Just actualType -> actualType
                               _ -> error ("No type on func " ++ show func)
                     FuncTy _ argTys retTy = funcTy
                     castToFn =
                       if unwrapLambdas
                       then tyToCLambdaFix retTy ++ "(*)(" ++ joinWithComma (map tyToCRawFunctionPtrFix argTys) ++ ")"
                       else tyToCLambdaFix retTy ++ "(*)(" ++ joinWithComma (map tyToCLambdaFix argTys) ++ ")"
                     castToFnWithEnv =
                       if unwrapLambdas
                       then tyToCLambdaFix retTy ++ "(*)(" ++ joinWithComma (map tyToCRawFunctionPtrFix (StructTy "LambdaEnv" [] : argTys)) ++ ")"
                       else tyToCLambdaFix retTy ++ "(*)(" ++ joinWithComma (map tyToCLambdaFix (StructTy "LambdaEnv" [] : argTys)) ++ ")"
                     callLambda = funcToCall ++ ".env ? ((" ++ castToFnWithEnv ++ ")" ++ funcToCall ++ ".callback)" ++ "(" ++ funcToCall ++ ".env" ++ (if null args then "" else ", ") ++ argListAsC ++ ") : ((" ++ castToFn ++ ")" ++ funcToCall ++ ".callback)(" ++ argListAsC ++ ");\n"
                 if retTy == UnitTy
                   then do appendToSrc (addIndent indent ++ callLambda)
                           return ""
                   else do let varName = freshVar i
                           appendToSrc (addIndent indent ++ tyToCLambdaFix retTy ++ " " ++ varName ++ " = " ++ callLambda)
                           return varName

            -- Empty list
            [] -> do appendToSrc (addIndent indent ++ "/* () */\n")
                     return ""

        visitList _ xobj@(XObj (Lst _) Nothing Nothing) = error ("List is missing info and type! " ++ show xobj)
        visitList _ xobj@(XObj (Lst _) Nothing (Just _)) = error ("List is missing info! " ++ show xobj)
        visitList _ xobj = error ("Must visit list! " ++ show xobj)

        createArgList :: Int -> Bool -> [XObj] -> State EmitterState String
        createArgList indent unwrapLambdas args =
          do argStrings <- mapM (visit indent) args
             let argTypes = map forceTy args
             return $ intercalate ", " $ if unwrapLambdas
                                         then zipWith unwrapLambda argStrings argTypes
                                         else argStrings

        unwrapLambda :: String -> Ty -> String
        unwrapLambda variableName ty =
          if isFunctionType ty
          then variableName ++ ".callback"
          else variableName

        visitArray :: Int -> XObj -> State EmitterState String
        visitArray indent (XObj (Arr xobjs) (Just i) t) =
          do let arrayVar = freshVar i
                 len = length xobjs
                 Just (StructTy "Array" [innerTy]) = t
             appendToSrc (addIndent indent ++ "Array " ++ arrayVar ++
                          " = { .len = " ++ show len ++ "," ++
                          " .capacity = " ++ show len ++ "," ++
                          " .data = CARP_MALLOC(sizeof(" ++ tyToCLambdaFix innerTy ++ ") * " ++ show len ++ ") };\n")
             zipWithM_ (visitArrayElement indent arrayVar innerTy) [0..] xobjs
             return arrayVar

        visitArray _ _ = error "Must visit array!"

        visitArrayElement :: Int -> String -> Ty -> Int -> XObj -> State EmitterState ()
        visitArrayElement indent arrayVar innerTy index xobj =
          do visited <- visit indent xobj
             appendToSrc (addIndent indent ++ "((" ++ tyToCLambdaFix innerTy ++ "*)" ++ arrayVar ++
                          ".data)[" ++ show index ++ "] = " ++ visited ++ ";\n")
             return ()

delete :: Int -> Info -> State EmitterState ()
delete indent i = mapM_ deleterToC (infoDelete i)
  where deleterToC :: Deleter -> State EmitterState ()
        deleterToC FakeDeleter {} =
          return ()
        deleterToC PrimDeleter {} =
          return ()
        deleterToC RefDeleter {} =
          return ()
        deleterToC deleter@ProperDeleter{} =
          appendToSrc $ addIndent indent ++ "" ++ pathToC (deleterPath deleter) ++ "(" ++ mangle (deleterVariable deleter) ++ ");\n"

defnToDeclaration :: MetaData -> SymPath -> [XObj] -> Ty -> String
defnToDeclaration meta path@(SymPath _ name) argList retTy =
  let (XObj (Lst annotations) _ _) = fromMaybe emptyList (Map.lookup "annotations" (getMeta meta))
      annotationsStr = joinWith " " (map strToC annotations)
      sep = if not (null annotationsStr) then " " else ""
  in annotationsStr ++ sep ++
    if name == "main"
      then "int main(int argc, char** argv)"
      else let retTyAsC = tyToCLambdaFix retTy
               paramsAsC = paramListToC argList
               annotations = meta
           in (retTyAsC ++ " " ++ pathToC path ++ "(" ++ paramsAsC ++ ")")
  where strToC (XObj (Str s) _ _) = s
        strToC xobj = pretty xobj

templateToC :: Template -> SymPath -> Ty -> String
templateToC template path actualTy =
  let mappings = unifySignatures (templateSignature template) actualTy
      declaration = templateDeclaration template actualTy
      definition = templateDefinition template actualTy
      tokens = concatMap (concretizeTypesInToken mappings (pathToC path) declaration) definition
  in  concatMap show tokens ++ "\n"

templateToDeclaration :: Template -> SymPath -> Ty -> String
templateToDeclaration template path actualTy =
  let mappings = unifySignatures (templateSignature template) actualTy
      e = error "Can't refer to declaration in declaration."
      declaration = templateDeclaration template actualTy
      tokens = concatMap (concretizeTypesInToken mappings (pathToC path) e) declaration
  in  concatMap show tokens ++ ";\n"

memberToDecl :: Int -> (XObj, XObj) -> State EmitterState ()
memberToDecl indent (memberName, memberType) =
  case xobjToTy memberType of
    -- Handle function pointers as members specially to allow members that are functions referring to the struct itself.
    Just t  -> appendToSrc (addIndent indent ++ tyToCLambdaFix t ++ " " ++ mangle (getName memberName) ++ ";\n")
    Nothing -> error ("Invalid memberType: " ++ show memberType)

defStructToDeclaration :: Ty -> SymPath -> [XObj] -> String
defStructToDeclaration structTy@(StructTy typeName typeVariables) path rest =
  let indent' = indentAmount

      typedefCaseToMemberDecl :: XObj -> State EmitterState [()]
      typedefCaseToMemberDecl (XObj (Arr members) _ _) = mapM (memberToDecl indent') (pairwise members)
      typedefCaseToMemberDecl _ = error "Invalid case in typedef."

      -- Note: the names of types are not namespaced
      visit = do appendToSrc "typedef struct {\n"
                 mapM_ typedefCaseToMemberDecl rest
                 appendToSrc ("} " ++ tyToC structTy ++ ";\n")

  in if isTypeGeneric structTy
     then "" -- ("// " ++ show structTy ++ "\n")
     else emitterSrc (execState visit (EmitterState ""))

defSumtypeToDeclaration sumTy@(StructTy typeName typeVariables) path rest =
  let indent = indentAmount

      visit = do appendToSrc "typedef struct {\n"
                 appendToSrc (addIndent indent ++ "union {\n")
                 mapM_ (emitSumtypeCase indent) rest
                 appendToSrc (addIndent indent ++ "};\n")
                 appendToSrc (addIndent indent ++ "char _tag;\n")
                 appendToSrc ("} " ++ tyToC sumTy ++ ";\n")
                 --appendToSrc ("// " ++ show typeVariables ++ "\n")
                 mapM_ emitSumtypeCaseTagDefinition (zip [0..] rest)

      emitSumtypeCase :: Int -> XObj -> State EmitterState ()
      emitSumtypeCase indent (XObj (Lst [XObj (Sym (SymPath [] caseName) _) _ _, XObj (Arr []) _ _]) _ _) =
        appendToSrc (addIndent indent ++ "// " ++ caseName ++ "\n")
      emitSumtypeCase indent xobj@(XObj (Lst [XObj (Sym (SymPath [] caseName) _) _ _, XObj (Arr memberTys) _ _]) _ _) =
        do appendToSrc (addIndent indent ++ "struct {\n")
           let members = zipWith (\anonName tyXObj -> (anonName, tyXObj)) anonMemberSymbols memberTys
           mapM_ (memberToDecl (indent + indentAmount)) members
           appendToSrc (addIndent indent ++ "} " ++ caseName ++ ";\n")
      emitSumtypeCase indent xobj@(XObj (Sym (SymPath [] caseName) _) _ _) =
        appendToSrc (addIndent indent ++ "// " ++ caseName ++ "\n")

      emitSumtypeCaseTagDefinition :: (Int, XObj) -> State EmitterState ()
      emitSumtypeCaseTagDefinition (tagIndex, xobj@(XObj (Lst [XObj (Sym (SymPath [] caseName) _) _ _, _]) _ _)) =
        appendToSrc ("#define " ++ tagName sumTy caseName ++ " " ++ show tagIndex ++ "\n")
      emitSumtypeCaseTagDefinition (tagIndex, xobj@(XObj (Sym (SymPath [] caseName) _) _ _)) =
        appendToSrc ("#define " ++ tagName sumTy caseName ++ " " ++ show tagIndex ++ "\n")

  in if isTypeGeneric sumTy
     then ""
     else emitterSrc (execState visit (EmitterState ""))

defaliasToDeclaration :: Ty -> SymPath -> String
defaliasToDeclaration t path =
  case t of
    (FuncTy _ argTys retTy) -> "typedef " ++ tyToCLambdaFix retTy ++ "(*" ++ pathToC path ++ ")(" ++
                               intercalate ", " (map tyToCLambdaFix argTys) ++ ");\n"
    _ ->  "typedef " ++ tyToC t ++ " " ++ pathToC path ++ ";\n"

toDeclaration :: Binder -> String
toDeclaration (Binder meta xobj@(XObj (Lst xobjs) _ t)) =
  case xobjs of
    [XObj (Defn _) _ _, XObj (Sym path _) _ _, XObj (Arr argList) _ _, _] ->
      let (Just (FuncTy _ _ retTy)) = t
      in  defnToDeclaration meta path argList retTy ++ ";\n"
    [XObj Def _ _, XObj (Sym path _) _ _, _] ->
      let Just t' = t
      in "" ++ tyToCLambdaFix t' ++ " " ++ pathToC path ++ ";\n"
    XObj (Deftype t) _ _ : XObj (Sym path _) _ _ : rest ->
      defStructToDeclaration t path rest
    XObj (DefSumtype t) _ _ : XObj (Sym path _) _ _ : rest ->
      defSumtypeToDeclaration t path rest
    XObj (Deftemplate _) _ _ : _ ->
      ""
    XObj Macro _ _ : _ ->
      ""
    XObj Dynamic _ _ : _ ->
      ""
    XObj DefDynamic _ _ : _ ->
      ""
    [XObj (Instantiate template) _ _, XObj (Sym path _) _ _] ->
      let Just t' = t
      in templateToDeclaration template path t'
    [XObj (Defalias aliasTy) _ _, XObj (Sym path _) _ _] ->
      defaliasToDeclaration aliasTy path
    [XObj (Interface _ _) _ _, _] ->
      ""
    XObj (External _) _ _ : _ ->
      ""
    XObj ExternalType _ _ : _ ->
      ""
    XObj (Command _) _ _ : _ ->
      ""
    _ -> error ("Internal compiler error: Can't emit other kinds of definitions: " ++ show xobj)
toDeclaration _ = error "Missing case."

paramListToC :: [XObj] -> String
paramListToC xobjs = intercalate ", " (map getParam (filter notUnit xobjs))
  where getParam :: XObj -> String
        getParam (XObj (Sym (SymPath _ name) _) _ (Just t)) = tyToCLambdaFix t ++ " " ++ mangle name
        getParam invalid = error (show (InvalidParameter invalid))
        notUnit (XObj _ _ (Just UnitTy)) = False
        notUnit _ = True

projectIncludesToC :: Project -> String
projectIncludesToC proj = intercalate "\n" (map includerToC includes) ++ "\n\n"
  where includerToC (SystemInclude file) = "#include <" ++ file ++ ">"
        includerToC (RelativeInclude file) = "#include \"" ++ file ++ "\""
        includes = projectIncludes proj

binderToC :: ToCMode -> Binder -> Either ToCError String
binderToC toCMode binder =
  let xobj = binderXObj binder
  in  case xobj of
        XObj (External _) _ _ -> Right ""
        XObj ExternalType _ _ -> Right ""
        XObj (Command _) _ _ -> Right ""
        XObj (Mod env) _ _ -> envToC env toCMode
        _ -> case ty xobj of
               Just t -> if isTypeGeneric t
                         then Right ""
                         else do checkForUnresolvedSymbols xobj
                                 return (toC toCMode binder)
               Nothing -> Left (BinderIsMissingType binder)

binderToDeclaration :: TypeEnv -> Binder -> Either ToCError String
binderToDeclaration typeEnv binder =
  let xobj = binderXObj binder
  in  case xobj of
        XObj (Mod env) _ _ -> envToDeclarations typeEnv env
        _ -> case ty xobj of
               Just t -> if isTypeGeneric t then Right "" else Right (toDeclaration binder ++ "")
               Nothing -> Left (BinderIsMissingType binder)

envToC :: Env -> ToCMode -> Either ToCError String
envToC env toCMode =
  let binders = Map.toList (envBindings env)
  in  do okCodes <- mapM (binderToC toCMode . snd) binders
         return (concat okCodes)

globalsToC :: Env -> Either ToCError String
globalsToC globalEnv =
  let allGlobalBinders = findAllGlobalVariables globalEnv
  in  do okCodes <- mapM (\(score, binder) ->
                            fmap (\s -> if s == "" then "" else ("\n    // Depth " ++ show score ++ "\n") ++ s)
                           (binderToC Globals binder))
                         (sortGlobalVariableBinders globalEnv allGlobalBinders)
         return (concat okCodes)

envToDeclarations :: TypeEnv -> Env -> Either ToCError String
envToDeclarations typeEnv env =
  let bindersWithScore = sortDeclarationBinders typeEnv (map snd (Map.toList (envBindings env)))
  in  do okDecls <- mapM (\(score, binder) ->
                            fmap (\s -> if s == "" then "" else ("\n// Depth " ++ show score ++ "\n") ++ s)
                            (binderToDeclaration typeEnv binder))
                         bindersWithScore
         return (concat okDecls)

-- debugScorePair :: (Int, Binder) -> (Int, Binder)
-- debugScorePair (s,b) = trace ("Scored binder: " ++ show b ++ ", score: " ++ show s) (s,b)

sortDeclarationBinders :: TypeEnv -> [Binder] -> [(Int, Binder)]
sortDeclarationBinders typeEnv binders =
  --trace ("\nSORTED: " ++ (show (sortOn fst (map (scoreBinder typeEnv) binders))))
  sortOn fst (map (scoreTypeBinder typeEnv) binders)

sortGlobalVariableBinders :: Env -> [Binder] -> [(Int, Binder)]
sortGlobalVariableBinders globalEnv binders =
  sortOn fst (map (scoreValueBinder globalEnv Set.empty) binders)

checkForUnresolvedSymbols :: XObj -> Either ToCError ()
checkForUnresolvedSymbols = visit
  where
    visit :: XObj -> Either ToCError ()
    visit xobj =
      case ty xobj of
        Nothing -> visitXObj
        Just t -> if isTypeGeneric t
                  then Left (UnresolvedGenericType xobj)
                  else visitXObj
      where
        visitXObj =
          case obj xobj of
            (Lst _) -> visitList xobj
            (Arr _) -> visitArray xobj
            (MultiSym _ _) -> Left (UnresolvedMultiSymbol xobj)
            (InterfaceSym _) -> Left (UnresolvedInterfaceSymbol xobj)
            _ -> return ()

    visitList :: XObj -> Either ToCError ()
    visitList (XObj (Lst xobjs) i t) =
      case mapM visit xobjs of
        Left e -> Left e
        Right _ -> return ()
    visitList _ = error "The function 'visitList' only accepts XObjs with lists in them."

    visitArray :: XObj -> Either ToCError ()
    visitArray (XObj (Arr xobjs) i t) =
      case mapM visit xobjs of
        Left e -> Left e
        Right _ -> return ()
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."

wrapInInitFunction :: Bool -> String -> String
wrapInInitFunction with_core src =
  "void carp_init_globals(int argc, char** argv) {\n" ++
  (if with_core
    then "  System_args.len = argc;\n  System_args.data = argv;\n"
    else "")
  ++ src ++
  "}"

isNumericLiteral :: XObj -> Bool
isNumericLiteral (XObj (Num _ _) _ _) = True
isNumericLiteral (XObj (Bol _) _ _) = True
isNumericLiteral (XObj (Chr _) _ _) = True
isNumericLiteral _ = False

removeSuffix :: String -> String
removeSuffix [] = []
removeSuffix [c] = [c]
removeSuffix ('_' : '_' : cs) = []
removeSuffix (c:cs) = c : removeSuffix cs
