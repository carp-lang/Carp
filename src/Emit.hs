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
              | DontVisitObj Obj
              | CannotEmitUnit
              | CannotEmitExternal
              | CannotEmitModKeyword
              | BinderIsMissingType Binder
              | UnresolvedMultiSymbol XObj
              | UnresolvedInterfaceSymbol XObj
              | UnresolvedGenericType XObj
              | CannotSet XObj

instance Show ToCError where
  show (InvalidParameter xobj) = "Invalid parameter: " ++ show (obj xobj)
  show (InvalidList xobj) = "Invalid list: " ++ show (obj xobj)
  show (DontVisitObj o) = "Don't visit " ++ show o ++ " (internal compiler error)."
  show CannotEmitUnit = "Can't emit code for empty list: ()"
  show CannotEmitExternal = "Can't emit code for external function/variable."
  show CannotEmitModKeyword = "Can't emit code for Mod."
  show (BinderIsMissingType b) = "Binder is missing type: " ++ show b
  show (UnresolvedMultiSymbol xobj@(XObj (MultiSym symName symPaths) _ _)) =
    "Found ambiguous symbol '" ++ symName ++
    "' (alternatives are " ++ joinWithComma (map show symPaths) ++ ")" ++
    " at " ++ prettyInfoFromXObj xobj
  show (UnresolvedInterfaceSymbol xobj@(XObj (InterfaceSym symName) _ _)) =
    "Found unresolved use of interface '" ++ symName ++ "'" ++
    " at " ++ prettyInfoFromXObj xobj
  show (UnresolvedGenericType xobj@(XObj _ _ (Just t))) =
    "Found unresolved generic type '" ++ show t ++ "' at " ++ prettyInfoFromXObj xobj
  show (CannotSet xobj) = "Can't emit code for setting " ++ pretty xobj ++ " at " ++ prettyInfoFromXObj xobj

data ToCMode = Functions | Globals | All deriving Show

data EmitterState = EmitterState { emitterSrc :: String }

appendToSrc :: String -> State EmitterState ()
appendToSrc moreSrc = modify (\s -> s { emitterSrc = emitterSrc s ++ moreSrc })

toC :: ToCMode -> XObj -> String
toC toCMode root = emitterSrc (execState (visit startingIndent root) (EmitterState ""))
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
                                x -> ['\'', x, '\'']
            Sym _ _ -> visitSymbol indent xobj
            Defn -> error (show (DontVisitObj Defn))
            Def -> error (show (DontVisitObj Def))
            Let -> error (show (DontVisitObj Let))
            If -> error (show (DontVisitObj If))
            Break -> error (show (DontVisitObj Break))
            While -> error (show (DontVisitObj While))
            Do -> error (show (DontVisitObj Do))
            e@(Typ _) -> error (show (DontVisitObj e))
            Mod _ -> error (show CannotEmitModKeyword)
            External _ -> error (show CannotEmitExternal)
            ExternalType -> error (show (DontVisitObj ExternalType))
            e@(Command _) -> error (show (DontVisitObj e))
            e@(Deftemplate _) ->  error (show (DontVisitObj e))
            e@(Instantiate _) ->  error (show (DontVisitObj e))
            e@(Defalias _) -> error (show (DontVisitObj e))
            e@(MultiSym _ _) -> error (show (DontVisitObj e))
            e@(InterfaceSym _) -> error (show (DontVisitObj e))
            Address -> error (show (DontVisitObj Address))
            SetBang -> error (show (DontVisitObj SetBang))
            Macro -> error (show (DontVisitObj Macro))
            Dynamic -> error (show (DontVisitObj Dynamic))
            The -> error (show (DontVisitObj The))
            Ref -> error (show (DontVisitObj Ref))
            e@(Interface _ _) -> error (show (DontVisitObj e))

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
        escapeString [] = ""
        escapeString ('\"':xs) = "\\\"" ++ escapeString xs
        escapeString (x:xs) = x : escapeString xs

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
            [XObj Defn _ _, XObj (Sym path@(SymPath _ name) _) _ _, XObj (Arr argList) _ _, body] ->
              case toCMode of
                Globals ->
                  return ""
                _ ->
                  do let innerIndent = indent + indentAmount
                         Just (FuncTy _ retTy) = t
                         defnDecl = defnToDeclaration path argList retTy
                     if (name == "main")
                       then appendToSrc "int main(int argc, char** argv) {\n"
                       else appendToSrc (defnDecl ++ " {\n")
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
                 -- appendToSrc (addIndent indent ++ "// This lambda captures " ++
                 --              show (length capturedVars) ++ " variables: " ++
                 --              joinWithComma (map getName capturedVars) ++ "\n")
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
                     _ <- mapM (uncurry letBindingToC) (pairwise bindings)
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
                 _ <- mapM (visit indent) (init expressions)
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
                              ++ " // " ++ (show (fromMaybe (VarTy "?") (ty variable))) ++ " = " ++ (show (fromMaybe (VarTy "?") (ty value)))
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

            -- Deftype
            XObj (Typ _) _ _ : XObj (Sym _ _) _ _ : _ ->
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
                     FuncTy argTys retTy = funcTy
                     callFunction = overriddenName ++ "(" ++ argListAsC ++ ");\n"
                 if retTy == UnitTy
                   then do appendToSrc (addIndent indent ++ callFunction)
                           return ""
                   else do let varName = freshVar i
                           appendToSrc (addIndent indent ++ tyToCLambdaFix retTy ++ " " ++ varName ++ " = " ++ callFunction)
                           return varName

            -- Function application (global symbols that are functions -- lambdas stored in def:s need to be called like locals, see below)
            func@(XObj (Sym path (LookupGlobal mode AFunction)) _ _) : args ->
              do argListAsC <- (createArgList indent (mode == ExternalCode)) args
                 let Just (FuncTy _ retTy) = ty func
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
                     FuncTy argTys retTy = funcTy
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
        deleterToC deleter@ProperDeleter{} =
          appendToSrc $ addIndent indent ++ "" ++ pathToC (deleterPath deleter) ++ "(" ++ mangle (deleterVariable deleter) ++ ");\n"

defnToDeclaration :: SymPath -> [XObj] -> Ty -> String
defnToDeclaration path@(SymPath _ name) argList retTy =
  if name == "main"
    then "int main(int argc, char** argv)"
    else let retTyAsC = tyToCLambdaFix retTy
             paramsAsC = paramListToC argList
         in (retTyAsC ++ " " ++ pathToC path ++ "(" ++ paramsAsC ++ ")")

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

deftypeToDeclaration :: Ty -> SymPath -> [XObj] -> String
deftypeToDeclaration structTy@(StructTy typeName typeVariables) path rest =
  let indent' = indentAmount

      typedefCaseToMemberDecl :: XObj -> State EmitterState [()]
      typedefCaseToMemberDecl (XObj (Arr members) _ _) = mapM memberToDecl (pairwise members)
      typedefCaseToMemberDecl _ = error "Invalid case in typedef."

      memberToDecl :: (XObj, XObj) -> State EmitterState ()
      memberToDecl (memberName, memberType) =
        case xobjToTy memberType of
          -- Handle function pointers as members specially to allow members that are functions referring to the struct itself.
          Just (FuncTy _ _) -> appendToSrc (addIndent indent' ++ "Lambda " ++ mangle (getName memberName) ++ ";\n") -- TODO: Can remove this case now and rely on tyToCLambdaFix instead?
          Just t  -> appendToSrc (addIndent indent' ++ tyToC t ++ " " ++ mangle (getName memberName) ++ ";\n")
          Nothing -> error ("Invalid memberType: " ++ show memberType)

      -- Note: the names of types are not namespaced
      visit = do appendToSrc "typedef struct {\n"
                 _ <- mapM typedefCaseToMemberDecl rest
                 appendToSrc ("} " ++ tyToC structTy ++ ";\n")

  in if isTypeGeneric structTy
     then "" -- ("// " ++ show structTy ++ "\n")
     else emitterSrc (execState visit (EmitterState ""))

defaliasToDeclaration :: Ty -> SymPath -> String
defaliasToDeclaration t path =
  case t of
    (FuncTy argTys retTy) -> "typedef " ++ tyToCLambdaFix retTy ++ "(*" ++ pathToC path ++ ")(" ++
                             intercalate ", " (map tyToCLambdaFix argTys) ++ ");\n"
    _ ->  "typedef " ++ tyToC t ++ " " ++ pathToC path ++ ";\n"

toDeclaration :: XObj -> String
toDeclaration xobj@(XObj (Lst xobjs) _ t) =
  case xobjs of
    [XObj Defn _ _, XObj (Sym path _) _ _, XObj (Arr argList) _ _, _] ->
      let (Just (FuncTy _ retTy)) = t
      in  defnToDeclaration path argList retTy ++ ";\n"
    [XObj Def _ _, XObj (Sym path _) _ _, _] ->
      let Just t' = t
      in "" ++ tyToCLambdaFix t' ++ " " ++ pathToC path ++ ";\n"
    XObj (Typ t) _ _ : XObj (Sym path _) _ _ : rest ->
      deftypeToDeclaration t path rest
    XObj (Deftemplate _) _ _ : _ ->
      ""
    XObj Macro _ _ : _ ->
      ""
    XObj Dynamic _ _ : _ ->
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
paramListToC xobjs = intercalate ", " (map getParam xobjs)
  where getParam :: XObj -> String
        getParam (XObj (Sym (SymPath _ name) _) _ (Just t)) = tyToCLambdaFix t ++ " " ++ mangle name
        getParam invalid = error (show (InvalidParameter invalid))

projectIncludesToC :: Project -> String
projectIncludesToC proj = intercalate "\n" (map includerToC (projectIncludes proj)) ++ "\n\n"
  where includerToC (SystemInclude file) = "#include <" ++ file ++ ">"
        includerToC (LocalInclude file) = "#include \"" ++ file ++ "\""

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
                                 return (toC toCMode xobj)
               Nothing -> Left (BinderIsMissingType binder)

binderToDeclaration :: TypeEnv -> Binder -> Either ToCError String
binderToDeclaration typeEnv binder =
  let xobj = binderXObj binder
  in  case xobj of
        XObj (Mod env) _ _ -> envToDeclarations typeEnv env
        _ -> case ty xobj of
               Just t -> if isTypeGeneric t then Right "" else Right (toDeclaration xobj ++ "")
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
