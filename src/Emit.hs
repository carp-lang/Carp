module Emit (toC, envToC, projectIncludesToC, envToDeclarations, checkForUnresolvedSymbols) where

import Data.List (intercalate, sortOn)
import Control.Monad.State
import Control.Monad (when, zipWithM_)
import qualified Data.Map as Map
import Debug.Trace

import Obj
import Types
import Util
import Template

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

data EmitterState = EmitterState { emitterSrc :: String }

appendToSrc :: String -> State EmitterState ()
appendToSrc moreSrc = modify (\s -> s { emitterSrc = emitterSrc s ++ moreSrc })

toC :: XObj -> String
toC root = emitterSrc (execState (visit 0 root) (EmitterState ""))
  where visit :: Int -> XObj -> State EmitterState String
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
            Chr c -> return ['\'', c, '\'']
            Sym _ -> visitSymbol xobj
            Defn -> error (show (DontVisitObj Defn))
            Def -> error (show (DontVisitObj Def))
            Let -> error (show (DontVisitObj Let))
            If -> error (show (DontVisitObj If))
            While -> error (show (DontVisitObj While))
            Do -> error (show (DontVisitObj Do))
            Typ -> error (show (DontVisitObj Typ))
            Mod _ -> error (show CannotEmitModKeyword)
            External -> error (show CannotEmitExternal)
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

        visitString indent (XObj (Str str) (Just i) _) =
          -- | This will allocate a new string every time the code runs:
          -- do let var = freshVar i
          --    appendToSrc (addIndent indent ++ "string " ++ var ++ " = strdup(\"" ++ str ++ "\");\n")
          --    return var
          -- | This will use the statically allocated string in the C binary (can't be freed):
          do let var = freshVar i
                 varRef = freshVar i ++ "_ref";
             appendToSrc (addIndent indent ++ "string " ++ var ++ " = string_from_cstr(\"" ++ escapeString str ++ "\");\n")
             appendToSrc (addIndent indent ++ "string *" ++ varRef ++ " = &" ++ var ++ ";\n")
             return varRef
        visitString _ _ = error "Not a string."
        escapeString [] = ""
        escapeString ('\"':xs) = "\\\"" ++ escapeString xs
        escapeString (x:xs) = x : escapeString xs

        visitSymbol :: XObj -> State EmitterState String
        visitSymbol xobj@(XObj (Sym path) _ t) = let Just t' = t
                                                 in if typeIsGeneric t'
                                                    then error ("Can't emit symbol of generic type: " ++
                                                                show path ++ " : " ++ show t' ++ " at " ++ prettyInfoFromXObj xobj)
                                                    else return (pathToC path)
        visitSymbol _ = error "Not a symbol."

        visitList :: Int -> XObj -> State EmitterState String
        visitList indent (XObj (Lst xobjs) (Just i) t) =
          case xobjs of
            -- Defn
            [XObj Defn _ _, XObj (Sym path) _ _, XObj (Arr argList) _ _, body] ->
              do let innerIndent = indent + indentAmount
                     Just (FuncTy _ retTy) = t
                     defnDecl = defnToDeclaration path argList retTy
                 appendToSrc (defnDecl ++ " {\n")
                 ret <- visit innerIndent body
                 delete innerIndent i
                 when (retTy /= UnitTy) $
                   appendToSrc (addIndent innerIndent ++ "return " ++ ret ++ ";\n")
                 appendToSrc "}\n\n"
                 return ""

            -- Def
            [XObj Def _ _, XObj (Sym path) _ _, expr] ->
              do ret <- visit 0 expr
                 let Just t' = t
                 appendToSrc ("" ++ tyToC t' ++ " " ++ pathToC path ++ " = " ++ ret ++ ";\n")
                 return ""

            -- Let
            [XObj Let _ _, XObj (Arr bindings) _ _, body] ->
              let indent' = indent + indentAmount
              in  do let Just bodyTy = ty body
                         isNotVoid = bodyTy /= UnitTy
                         letBodyRet = freshVar i
                     when isNotVoid $ -- Must be declared outside the scope
                       appendToSrc (addIndent indent ++ tyToC bodyTy ++ " " ++ letBodyRet ++ ";\n")
                     appendToSrc (addIndent indent ++ "/* let */ {\n")
                     let letBindingToC (XObj (Sym (SymPath _ symName)) _ _) expr =
                           do ret <- visit indent' expr
                              let Just bindingTy = ty expr
                              when (bindingTy /= UnitTy) $
                                appendToSrc (addIndent indent' ++ tyToC bindingTy ++ " " ++ mangle symName ++ " = " ++ ret ++ ";\n")
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
                       in  appendToSrc (addIndent indent ++ tyToC ifT ++ " " ++ ifRetVar ++ ";\n")
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
              in  do exprRetVar <- visitWhileExpression indent
                     appendToSrc (addIndent indent ++ tyToC exprTy ++ " " ++ conditionVar ++ " = " ++ exprRetVar ++ ";\n")
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
                           appendToSrc (addIndent indent ++ tyToC lastTy ++ " " ++ retVar ++ " = " ++ lastRet ++ ";\n")
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
                         (XObj (Lst (XObj Ref _ _ : symObj@(XObj (Sym sym) _ _) : _)) _ _) -> pathToC sym
                         _ -> error "How to handle setting of refs?"
                     Just varInfo = info variable
                 delete indent varInfo
                 appendToSrc (addIndent indent ++ properVariableName ++ " = " ++ valueVar ++ ";\n")
                 return ""

            -- The
            [XObj The _ _, _, value] ->
              do var <- visit indent value
                 let Just t' = t
                     fresh = mangle (freshVar i)
                 appendToSrc (addIndent indent ++ tyToC t' ++ " " ++ fresh ++ " = " ++ var ++ "; // From the 'the' function.\n")
                 return fresh

            -- Ref
            [XObj Ref _ _, value] ->
              do var <- visit indent value
                 let Just t' = t
                     fresh = mangle (freshVar i)
                 appendToSrc (addIndent indent ++ tyToC t' ++ " " ++ fresh ++ " = &" ++ var ++ "; // ref\n")
                 return fresh

            -- Deftype
            XObj Typ _ _ : XObj (Sym _) _ _ : _ ->
              return ""

            -- Template
            [XObj (Deftemplate _) _ _, XObj (Sym _) _ _] ->
              return ""

            [XObj (Instantiate template) _ _, XObj (Sym path) _ _] ->
              do let Just t' = t
                 appendToSrc (templateToC template path t')
                 return ""

            -- Alias
            XObj (Defalias _) _ _ : _ ->
              return ""

            -- External
            XObj External _ _ : _ ->
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

            -- Function application
            func : args ->
              do funcToCall <- visit indent func
                 argListAsC <- createArgList indent args
                 let Just (FuncTy _ retTy) = ty func
                 if retTy == UnitTy
                   then do appendToSrc (addIndent indent ++ funcToCall ++ "(" ++ argListAsC ++ ");\n")
                           return ""
                   else do let varName = freshVar i
                           appendToSrc (addIndent indent ++ tyToC retTy ++ " " ++ varName ++ " = " ++ funcToCall ++ "(" ++ argListAsC ++ ");\n")
                           return varName

            -- Empty list
            [] -> do appendToSrc (addIndent indent ++ "/* () */\n")
                     return ""
        visitList _ xobj = error ("Must visit list! " ++ show xobj)

        createArgList :: Int -> [XObj] -> State EmitterState String
        createArgList indent args = do argStrings <- mapM (visit indent) args
                                       return (intercalate ", " argStrings)

        visitArray :: Int -> XObj -> State EmitterState String
        visitArray indent (XObj (Arr xobjs) (Just i) t) =
          do let arrayVar = freshVar i
                 len = length xobjs
                 Just (StructTy "Array" [innerTy]) = t
             appendToSrc (addIndent indent ++ "Array " ++ arrayVar ++
                          " = { .len = " ++ show len ++ "," ++
                          " .data = CARP_MALLOC(sizeof(" ++ tyToC innerTy ++ ") * " ++ show len ++ ") };\n")
             zipWithM_ (visitArrayElement indent arrayVar innerTy) [0..] xobjs
             return arrayVar

        visitArray _ _ = error "Must visit array!"

        visitArrayElement :: Int -> String -> Ty -> Int -> XObj -> State EmitterState ()
        visitArrayElement indent arrayVar innerTy index xobj =
          do visited <- visit indent xobj
             appendToSrc (addIndent indent ++ "((" ++ tyToC innerTy ++ "*)" ++ arrayVar ++
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
  let retTyAsC = tyToC $ if name == "main"
                         then IntTy
                         else retTy
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

deftypeToDeclaration :: SymPath -> [XObj] -> String
deftypeToDeclaration path rest =
  let indent' = indentAmount
      (SymPath _ typeName) = path
      --p = (PointerTy (StructTy typeName []))

      typedefCaseToMemberDecl :: XObj -> State EmitterState [()]
      typedefCaseToMemberDecl (XObj (Arr members) _ _) = mapM memberToDecl (pairwise members)
      typedefCaseToMemberDecl _ = error "Invalid case in typedef."

      memberToDecl :: (XObj, XObj) -> State EmitterState ()
      memberToDecl (memberName, memberType) =
        case xobjToTy memberType of
          Just t  -> appendToSrc (addIndent indent' ++ tyToC t ++ " " ++ mangle (getName memberName) ++ ";\n")
          Nothing -> error ("Invalid memberType: " ++ show memberType)

      -- Note: the names of types are not namespaced
      visit = do appendToSrc "typedef struct {\n"
                 _ <- mapM typedefCaseToMemberDecl rest
                 appendToSrc ("} " ++ typeName ++ ";\n")

  in emitterSrc (execState visit (EmitterState ""))

defaliasToDeclaration :: Ty -> SymPath -> String
defaliasToDeclaration t path =
  case t of
    (FuncTy argTys retTy) -> "typedef " ++ tyToC retTy ++ "(*" ++ pathToC path ++ ")(" ++
                             intercalate ", " (map tyToC argTys) ++ ");\n"
    _ ->  "typedef " ++ tyToC t ++ " " ++ pathToC path ++ ";\n"

toDeclaration :: XObj -> String
toDeclaration xobj@(XObj (Lst xobjs) _ t) =
  case xobjs of
    [XObj Defn _ _, XObj (Sym path) _ _, XObj (Arr argList) _ _, _] ->
      let (Just (FuncTy _ retTy)) = t
      in  defnToDeclaration path argList retTy ++ ";\n"
    [XObj Def _ _, XObj (Sym path) _ _, _] ->
      let Just t' = t
      in "" ++ tyToC t' ++ " " ++ pathToC path ++ ";\n"
    XObj Typ _ _ : XObj (Sym path) _ _ : rest ->
      deftypeToDeclaration path rest
    XObj (Deftemplate _) _ _ : _ ->
      ""
    XObj Macro _ _ : _ ->
      ""
    XObj Dynamic _ _ : _ ->
      ""
    [XObj (Instantiate template) _ _, XObj (Sym path) _ _] ->
      let Just t' = t
      in templateToDeclaration template path t'
    [XObj (Defalias aliasTy) _ _, XObj (Sym path) _ _] ->
      defaliasToDeclaration aliasTy path
    [XObj (Interface _ _) _ _, _] ->
      ""
    XObj External _ _ : _ ->
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
        getParam (XObj (Sym (SymPath _ name)) _ (Just t)) = tyToC t ++ " " ++ mangle name
        getParam invalid = error (show (InvalidParameter invalid))

projectIncludesToC :: Project -> String
projectIncludesToC proj = intercalate "\n" (map includerToC (projectIncludes proj)) ++ "\n\n"
  where includerToC (SystemInclude file) = "#include <" ++ file ++ ">"
        includerToC (LocalInclude file) = "#include \"" ++ file ++ "\""

binderToC :: Binder -> Either ToCError String
binderToC binder = let xobj = binderXObj binder
                   in  case xobj of
                         XObj External _ _ -> Right ""
                         XObj ExternalType _ _ -> Right ""
                         XObj (Command _) _ _ -> Right ""
                         XObj (Mod env) _ _ -> envToC env
                         _ -> case ty xobj of
                                Just t -> if typeIsGeneric t
                                          then Right ""
                                          else do checkForUnresolvedSymbols xobj
                                                  return (toC xobj)
                                Nothing -> Left (BinderIsMissingType binder)

binderToDeclaration :: TypeEnv -> Binder -> Either ToCError String
binderToDeclaration typeEnv binder =
  let xobj = binderXObj binder
  in  case xobj of
        XObj (Mod env) _ _ -> envToDeclarations typeEnv env
        _ -> case ty xobj of
               Just t -> if typeIsGeneric t then Right "" else Right (toDeclaration xobj ++ "")
               Nothing -> Left (BinderIsMissingType binder)

envToC :: Env -> Either ToCError String
envToC env = let binders = map snd (Map.toList (envBindings env))
             in  do okCodes <- mapM binderToC binders
                    return (concat okCodes)

envToDeclarations :: TypeEnv -> Env -> Either ToCError String
envToDeclarations typeEnv env =
  let bindersWithScore = sortDeclarationBinders typeEnv (map snd (Map.toList (envBindings env)))
  in  do okDecls <- mapM (\(score, binder) ->
                            -- | Uncomment this line to emit the score of each binding:
                            -- fmap (\s -> if s == "" then "" else ("\n// Depth " ++ show score ++ "\n") ++ s)
                            (binderToDeclaration typeEnv binder))
                         bindersWithScore
         return (concat okDecls)

-- debugScorePair :: (Int, Binder) -> (Int, Binder)
-- debugScorePair (s,b) = trace ("Scored binder: " ++ show b ++ ", score: " ++ show s) (s,b)

sortDeclarationBinders :: TypeEnv -> [Binder] -> [(Int, Binder)]
sortDeclarationBinders typeEnv binders =
  --trace ("\nSORTED: " ++ (show (sortOn fst (map (scoreBinder typeEnv) binders))))
  sortOn fst (map (scoreBinder typeEnv) binders)

checkForUnresolvedSymbols :: XObj -> Either ToCError ()
checkForUnresolvedSymbols = visit
  where
    visit :: XObj -> Either ToCError ()
    visit xobj =
      case ty xobj of
        Nothing -> visitXObj
        Just t -> if typeIsGeneric t
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
