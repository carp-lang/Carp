module Emit
  ( toC,
    envToC,
    globalsToC,
    projectIncludesToC,
    projectPreprocToC,
    envToDeclarations,
    checkForUnresolvedSymbols,
    ToCMode (..),
    wrapInInitFunction,
    typeEnvToDeclarations,
  )
where

import Control.Monad.State
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (intercalate, isPrefixOf, sortOn)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Env
import Info
import qualified Map
import qualified Meta
import Obj
import Path (takeFileName)
import Project
import Scoring
import qualified Set
import Template
import TypePredicates
import Types
import TypesToC
import Util

addIndent :: Int -> String
addIndent n = replicate n ' '

indentAmount :: Int
indentAmount = 4

data ToCError
  = InvalidParameter XObj
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
    "I encountered an invalid parameter `" ++ show (xobjObj xobj) ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "."
  show (InvalidList xobj) =
    "I encountered an invalid list `" ++ show (xobjObj xobj) ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "."
  show (DontVisitObj xobj) =
    "I can’t visit " ++ show (xobjObj xobj) ++ " at " ++ prettyInfoFromXObj xobj
      ++ "."
  show (CannotEmitUnit xobj) =
    "I can't emit code for the unit type `()` at " ++ prettyInfoFromXObj xobj
      ++ "."
  show (CannotEmitExternal xobj) =
    "I can’t emit code for the external function/variable `"
      ++ show (xobjObj xobj)
      ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "."
  show (CannotEmitModKeyword xobj) =
    "I can’t emit code for the module `" ++ show (xobjObj xobj) ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "."
  show (BinderIsMissingType b) =
    "I encountered a binder `" ++ show b ++ "` that is missing its type."
  show (UnresolvedMultiSymbol xobj@(XObj (MultiSym symName symPaths) _ _)) =
    "I found an ambiguous symbol `" ++ symName
      ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "\n\nPossibilities:\n  "
      ++ joinWith "\n  " (map show symPaths)
      ++ "\n\nAll possibilities have the correct type."
  show (UnresolvedMultiSymbol _) = error "show unresolvedmultisymbol"
  show (UnresolvedInterfaceSymbol xobj@(XObj (InterfaceSym symName) _ _)) =
    "I found an interface `" ++ symName
      ++ "` that is unresolved in the context at "
      ++ prettyInfoFromXObj xobj
  show (UnresolvedInterfaceSymbol _) = error "show unresolvedinterfacesymbol"
  show (UnresolvedGenericType xobj@(XObj _ _ (Just t))) =
    "I found an unresolved generic type `" ++ show t
      ++ "` for the expression `"
      ++ pretty xobj
      ++ "` at "
      ++ prettyInfoFromXObj xobj
  show (UnresolvedGenericType _) = error "show unresolvedgenerictype"
  show (CannotSet xobj) =
    "I can’t emit code for setting `" ++ pretty xobj ++ "` at "
      ++ prettyInfoFromXObj xobj
      ++ "\n\nOnly variables can be reset using `set!`."

data ToCMode = Functions | Globals | All deriving (Show)

newtype EmitterState = EmitterState {emitterBuilder :: TB.Builder}

emptyEmitterState :: EmitterState
emptyEmitterState = EmitterState mempty

renderEmitterState :: EmitterState -> String
renderEmitterState = TL.unpack . TB.toLazyText . emitterBuilder

appendToSrc :: String -> State EmitterState ()
appendToSrc moreSrc = modify (\s -> s {emitterBuilder = emitterBuilder s <> TB.fromString moreSrc})

toC :: ToCMode -> Bool -> [(SymPath, Binder)] -> Binder -> String
toC toCMode emitLines mutualGroup (Binder meta root) = renderEmitterState (execState (visit startingIndent root) emptyEmitterState)
  where
    startingIndent = case toCMode of
      Functions -> 0
      Globals -> 4
      All -> 0
    emitLineDir :: XObj -> State EmitterState ()
    emitLineDir xobj = when emitLines $
      case xobjInfo xobj of
        Just i
          | infoLine i > 0 && infoFile i /= "dummy-file" ->
            appendToSrc ("#line " ++ show (infoLine i) ++ " \"" ++ infoFile i ++ "\"\n")
        _ -> pure ()
    emitLineDirInfo :: Info -> State EmitterState ()
    emitLineDirInfo i =
      when emitLines $
        when (infoLine i > 0 && infoFile i /= "dummy-file") $
          appendToSrc ("#line " ++ show (infoLine i) ++ " \"" ++ infoFile i ++ "\"\n")
    visit :: Int -> XObj -> State EmitterState String
    visit indent xobj =
      let dontVisit = error (show (DontVisitObj xobj))
       in case xobjObj xobj of
            Lst _ -> visitList indent xobj
            Arr _ -> visitArray indent xobj
            StaticArr _ -> visitStaticArray indent xobj
            Num IntTy num -> pure (show num)
            Num LongTy num -> pure (show num ++ "l")
            Num ByteTy num -> pure (show num)
            Num FloatTy num -> pure (show num ++ "f")
            Num DoubleTy num -> pure (show num)
            Num _ _ -> error "Can't emit invalid number type."
            Bol b -> pure (if b then "true" else "false")
            Str _ -> visitString indent xobj
            Pattern _ -> visitString indent xobj
            Chr c -> pure $ case c of
              '\t' -> "'\\t'"
              '\n' -> "'\\n'"
              '\\' -> "'\\\\'"
              x -> show (ord x) ++ "/*" ++ show x ++ "*/" -- ['U', '\'', x, '\'']
            Closure _ _ -> pure ""
            VMClosure _ _ _ _ -> pure ""
            Sym _ _ -> visitSymbol indent xobj
            Mod _ _ -> error (show (CannotEmitModKeyword xobj))
            External _ -> error (show (CannotEmitExternal xobj))
            (Defn _) -> dontVisit
            Def -> dontVisit
            Let -> dontVisit
            If -> dontVisit
            Break -> dontVisit
            While -> dontVisit
            Do -> dontVisit
            (Deftype _) -> dontVisit
            (DefSumtype _) -> dontVisit
            ExternalType _ -> dontVisit
            (Command _) -> dontVisit
            (Primitive _) -> dontVisit
            (Deftemplate _) -> dontVisit
            (Instantiate _) -> dontVisit
            (Defalias _) -> dontVisit
            (MultiSym _ _) -> dontVisit
            (InterfaceSym _) -> dontVisit
            SetBang -> dontVisit
            Macro -> dontVisit
            Dynamic -> dontVisit
            DefDynamic -> dontVisit
            The -> dontVisit
            Ref -> dontVisit
            Deref -> dontVisit
            (Interface _ _) -> dontVisit
            (Dict _) -> dontVisit
            (Fn _ _) -> dontVisit
            LocalDef -> dontVisit
            (Match _) -> dontVisit
            With -> dontVisit
            MetaStub -> dontVisit
            C c -> pure c
    visitStr' indent str i shouldEscape =
      -- This will allocate a new string every time the code runs:
      -- do let var = freshVar i
      --    appendToSrc (addIndent indent ++ "String " ++ var ++ " = strdup(\"" ++ str ++ "\");\n")
      --    pure var
      -- This will use the statically allocated string in the C binary (can't be freed):
      do
        let var = freshVar i
            varRef = freshVar i ++ "_ref"
        appendToSrc (addIndent indent ++ "static String " ++ var ++ " = \"" ++ (if shouldEscape then escapeString str else str) ++ "\";\n")
        appendToSrc (addIndent indent ++ "String *" ++ varRef ++ " = &" ++ var ++ ";\n")
        pure varRef
    visitString indent (XObj (Str str) (Just i) _) = visitStr' indent str i True
    visitString indent (XObj (Pattern str) (Just i) _) = visitStr' indent str i False
    visitString _ _ = error "Not a string."
    escaper '\"' acc = "\\\"" ++ acc
    escaper '\\' acc = "\\\\" ++ acc
    escaper '\n' acc = "\\n" ++ acc
    escaper '\a' acc = "\\a" ++ acc
    escaper '\b' acc = "\\b" ++ acc
    escaper '\f' acc = "\\f" ++ acc
    escaper '\r' acc = "\\r" ++ acc
    escaper '\t' acc = "\\t" ++ acc
    escaper '\v' acc = "\\v" ++ acc
    escaper x acc = x : acc
    escapeString = foldr escaper ""
    visitSymbol :: Int -> XObj -> State EmitterState String
    visitSymbol _ (XObj (Sym _ (LookupGlobalOverride overrideWithName)) _ _) =
      pure overrideWithName
    visitSymbol indent xobj@(XObj sym@(Sym path lookupMode) (Just i) ty) =
      let t = fromMaybe (error "emit: symbol has no type") $ ty
       in if isTypeGeneric t
            then
              error
                ( "Can't emit symbol of generic type: "
                    ++ show path
                    ++ " : "
                    ++ show t
                    ++ " at "
                    ++ prettyInfoFromXObj xobj
                )
            else
              if isFunctionType t && not (isLookupLocal lookupMode) && not (isGlobalVariableLookup lookupMode)
                then do
                  let var = freshVar i
                  appendToSrc (addIndent indent ++ "Lambda " ++ var ++ " = { .callback = (void*)" ++ pathToC path ++ ", .env = NULL, .delete = NULL, .copy = NULL }; //" ++ show sym ++ "\n")
                  pure var
                else pure $ case lookupMode of
                  LookupLocal (Capture _) -> "_env->" ++ pathToC path
                  _ -> pathToC path
    visitSymbol _ xobj@(XObj (Sym _ _) Nothing _) = error ("Symbol missing info: " ++ show xobj)
    visitSymbol _ _ = error "Not a symbol."
    visitList :: Int -> XObj -> State EmitterState String
    visitList indent (XObj (Lst xobjs) (Just info) ty) =
      case xobjs of
        -- Defn
        [XObj (Defn _) _ _, XObj (Sym path@(SymPath _ name) _) _ _, XObj (Arr argList) _ _, body] ->
          case toCMode of
            Globals ->
              pure ""
            _
              | not (null mutualGroup),
                pathToC path == pathToC (fst (head mutualGroup)) ->
                -- First member of mutual group: emit merged dispatch + wrappers
                emitMutualGroup indent mutualGroup
            _
              | not (null mutualGroup) ->
                -- Non-first member of mutual group: skip (already emitted)
                pure ""
            _ ->
              do
                let innerIndent = indent + indentAmount
                    retTy = case ty of
                      Just (FuncTy _ rt _) -> rt
                      _ -> error "emit: defn has no return type"
                    defnDecl = defnToDeclaration meta path argList retTy
                    isMain = name == "main"
                    params = [(mangle n, forceTy p) | p@(XObj (Sym (SymPath _ n) _) _ _) <- argList, not (isUnit (forceTy p))]
                    canTCO = not isMain && hasSelfTailCalls body && isSafeForTCO argList
                emitLineDirInfo info
                appendToSrc (defnDecl ++ " {\n")
                when isMain $
                  appendToSrc (addIndent innerIndent ++ "carp_init_globals(argc, argv);\n")
                if canTCO
                  then do
                    let tcoIndent = innerIndent + indentAmount
                    appendToSrc (addIndent innerIndent ++ "while(1) {\n")
                    emitLineDir body
                    visitTCO tcoIndent params retTy (infoDelete info) Nothing body
                    appendToSrc (addIndent innerIndent ++ "}\n")
                  else do
                    emitLineDir body
                    ret <- visit innerIndent body
                    delete innerIndent (infoDelete info)
                    case retTy of
                      UnitTy -> when isMain $ appendToSrc (addIndent innerIndent ++ "return 0;\n")
                      _ -> appendToSrc (addIndent innerIndent ++ "return " ++ ret ++ ";\n")
                appendToSrc "}\n\n"
                pure ""
        -- Fn / λ
        [XObj (Fn name set) _ _, XObj (Arr _) _ _, _] ->
          do
            let retVar = freshVar info
                capturedVars = Set.toList set
                callback = fromMaybe (SymPath [] "") name
                callbackMangled = pathToC callback
                needEnv = not (null capturedVars)
                lambdaEnvTypeName = (SymPath [] (callbackMangled ++ "_ty")) -- The name of the struct is the callback name with suffix '_ty'.
                lambdaEnvType = StructTy (ConcreteNameTy lambdaEnvTypeName) []
                lambdaEnvName = freshVar info ++ "_env"
            appendToSrc
              ( addIndent indent ++ "// This lambda captures "
                  ++ show (length capturedVars)
                  ++ " variables: "
                  ++ joinWithComma (map getName capturedVars)
                  ++ "\n"
              )
            when needEnv $
              do
                appendToSrc
                  ( addIndent indent ++ tyToC lambdaEnvType ++ " *" ++ lambdaEnvName
                      ++ " = CARP_MALLOC(sizeof("
                      ++ tyToC lambdaEnvType
                      ++ "));\n"
                  )
                mapM_
                  ( \xobj ->
                      case xobj of
                        (XObj (Sym path lookupMode) _ _) ->
                          appendToSrc
                            ( addIndent indent ++ lambdaEnvName ++ "->"
                                ++ pathToC path
                                ++ " = "
                                ++ ( case lookupMode of
                                       LookupLocal (Capture _) -> "_env->" ++ pathToC path
                                       _ -> pathToC path
                                   )
                                ++ ";\n"
                            )
                        _ -> appendToSrc ""
                  )
                  (remove (isUnit . forceTy) capturedVars)
            appendToSrc (addIndent indent ++ "Lambda " ++ retVar ++ " = {\n")
            appendToSrc (addIndent indent ++ "  .callback = (void*)" ++ callbackMangled ++ ",\n")
            appendToSrc (addIndent indent ++ "  .env = " ++ (if needEnv then lambdaEnvName else "NULL") ++ ",\n")
            appendToSrc (addIndent indent ++ "  .delete = (void*)" ++ (if needEnv then "" ++ show lambdaEnvTypeName ++ "_delete" else "NULL") ++ ",\n")
            appendToSrc (addIndent indent ++ "  .copy = (void*)" ++ (if needEnv then "" ++ show lambdaEnvTypeName ++ "_copy" else "NULL") ++ "\n")
            appendToSrc (addIndent indent ++ "};\n")
            pure retVar
        -- Def
        [XObj Def _ _, XObj (Sym path _) _ _, expr] ->
          case toCMode of
            Functions ->
              pure ""
            _ ->
              do
                appendToSrc (addIndent indent ++ "{\n")
                let innerIndent = indent + indentAmount
                    cname = (Meta.getString (Meta.getCompilerKey Meta.CNAME) meta)
                    fullname = if (null cname) then pathToC path else cname
                ret <- visit innerIndent expr
                when (ret /= "") $ appendToSrc (addIndent innerIndent ++ fullname ++ " = " ++ ret ++ ";\n")
                delete innerIndent (infoDelete info)
                appendToSrc (addIndent indent ++ "}\n")
                pure ""
        -- Let
        [XObj Let _ _, XObj (Arr bindings) _ _, body] ->
          let indent' = indent + indentAmount
           in do
                let bodyTy = fromMaybe (error "emit: let body has no type") $ xobjTy body
                    isNotVoid = bodyTy /= UnitTy
                    letBodyRet = freshVar info
                when isNotVoid $ -- Must be declared outside the scope
                  appendToSrc (addIndent indent ++ tyToCLambdaFix bodyTy ++ " " ++ letBodyRet ++ ";\n")
                appendToSrc (addIndent indent ++ "/* let */ {\n")
                let letBindingToC (XObj (Sym (SymPath _ symName) _) _ _) expr =
                      do
                        ret <- visit indent' expr
                        let bindingTy = fromMaybe (error "emit: let binding value has no type") $ xobjTy expr
                        unless (isUnit bindingTy) $
                          appendToSrc (addIndent indent' ++ tyToCLambdaFix bindingTy ++ " " ++ mangle symName ++ " = " ++ ret ++ ";\n")
                    letBindingToC _ _ = error "Invalid binding."
                mapM_ (uncurry letBindingToC) (pairwise bindings)
                emitLineDir body
                ret <- visit indent' body
                when isNotVoid $
                  appendToSrc (addIndent indent' ++ letBodyRet ++ " = " ++ ret ++ ";\n")
                delete indent' (infoDelete info)
                appendToSrc (addIndent indent ++ "}\n")
                pure letBodyRet
        -- If
        [XObj If _ _, expr, ifTrue, ifFalse] ->
          let indent' = indent + indentAmount
           in do
                let isNotVoid = xobjTy ifTrue /= Just UnitTy
                    ifRetVar = freshVar info
                when isNotVoid $
                  let ifT = fromMaybe (error "emit: if true branch has no type") $ xobjTy ifTrue
                   in appendToSrc (addIndent indent ++ tyToCLambdaFix ifT ++ " " ++ ifRetVar ++ ";\n")
                exprVar <- visit indent expr
                appendToSrc (addIndent indent ++ "if (" ++ exprVar ++ ") {\n")
                emitLineDir ifTrue
                trueVar <- visit indent' ifTrue
                delete indent' (infoDelete (infoOrUnknown $ xobjInfo ifTrue))
                when isNotVoid $
                  appendToSrc (addIndent indent' ++ ifRetVar ++ " = " ++ trueVar ++ ";\n")
                appendToSrc (addIndent indent ++ "} else {\n")
                emitLineDir ifFalse
                falseVar <- visit indent' ifFalse
                delete indent' (infoDelete (infoOrUnknown $ xobjInfo ifFalse))
                when isNotVoid $
                  appendToSrc (addIndent indent' ++ ifRetVar ++ " = " ++ falseVar ++ ";\n")
                appendToSrc (addIndent indent ++ "}\n")
                pure ifRetVar
        -- Match
        XObj (Match matchMode) _ _ : expr@(XObj _ (Just exprInfo) (Just exprTyNotFixed)) : rest ->
          let indent' = indent + indentAmount
              retVar = freshVar info
              isNotVoid = ty /= Just UnitTy
              exprTy = exprTyNotFixed
              tagCondition :: String -> String -> Ty -> XObj -> [String]
              tagCondition var periodOrArrow caseTy ((XObj (Lst (XObj (Sym (SymPath _ caseName) _) _ _ : caseMatchers)) _ _)) =
                -- HACK! The function 'removeSuffix' ignores the type specialisation of the tag name and just uses the base name
                -- A better idea is to not specialise the names, which happens when calling 'concretize' on the lhs
                -- This requires a bunch of extra machinery though, so this will do for now...

                (var ++ periodOrArrow ++ "_tag == " ++ tagName caseTy (removeSuffix caseName)) :
                concat (zipWith (\c i -> tagCondition (var ++ periodOrArrow ++ "u." ++ removeSuffix caseName ++ ".member" ++ show i) "." (forceTy c) c) unitless ([0 ..] :: [Int]))
                where
                  unitless = remove (isUnit . forceTy) caseMatchers
              tagCondition _ _ _ _ =
                []
              --error ("tagCondition fell through: " ++ show x)

              tempVarToAvoidClash = freshVar exprInfo ++ "_temp"
              emitCaseMatcher :: (String, String) -> String -> XObj -> Integer -> State EmitterState ()
              emitCaseMatcher (periodOrArrow, ampersandOrNot) caseName (XObj (Sym path _) _ t) index =
                let tt = fromMaybe (error "emit: case matcher has no type") $ t
                 in appendToSrc
                      ( addIndent indent' ++ tyToCLambdaFix tt ++ " " ++ pathToC path ++ " = "
                          ++ ampersandOrNot
                          ++ tempVarToAvoidClash
                          ++ periodOrArrow
                          ++ "u."
                          ++ mangle caseName
                          ++ ".member"
                          ++ show index
                          ++ ";\n"
                      )
              emitCaseMatcher periodOrArrow caseName (XObj (Lst (XObj (Sym (SymPath _ innerCaseName) _) _ _ : xs)) _ _) index =
                zipWithM_ (\x i -> emitCaseMatcher periodOrArrow (caseName ++ ".member" ++ show i ++ ".u." ++ removeSuffix innerCaseName) x index) xs ([0 ..] :: [Int])
              emitCaseMatcher _ _ xobj _ =
                error ("Failed to emit case matcher for: " ++ pretty xobj)
              removeOuterRefTyIfMatchRef :: Ty -> Ty
              removeOuterRefTyIfMatchRef t =
                case matchMode of
                  MatchValue -> t
                  MatchRef ->
                    case t of
                      RefTy inner _ -> inner
                      _ -> error ("Failed to remove outer ref on type " ++ show t)
              emitCase :: String -> Bool -> (XObj, XObj) -> State EmitterState ()
              emitCase _ _ (caseLhs@(XObj (Lst (XObj Ref _ _ : _)) _ _), _) =
                error ("Can't emit case matchers for refs: " ++ pretty caseLhs)
              emitCase exprVar isFirst (caseLhs@(XObj (Lst (XObj (Sym (SymPath _ caseName@(_ : _)) _) _ _ : caseMatchers)) caseLhsInfo _), caseExpr) =
                -- A list of things, beginning with a tag
                do
                  appendToSrc (addIndent indent)
                  unless isFirst (appendToSrc "else ")
                  let refModifications =
                        case matchMode of
                          MatchValue -> (".", "")
                          MatchRef -> ("->", "&")
                  appendToSrc ("if(" ++ joinWith " && " (tagCondition exprVar (fst refModifications) (removeOuterRefTyIfMatchRef exprTy) caseLhs) ++ ") {\n")
                  appendToSrc (addIndent indent' ++ tyToCLambdaFix exprTy ++ " " ++ tempVarToAvoidClash ++ " = " ++ exprVar ++ ";\n")
                  zipWithM_ (emitCaseMatcher refModifications (removeSuffix caseName)) (remove (isUnit . forceTy) caseMatchers) [0 ..]
                  appendToSrc (addIndent indent' ++ "// Case expr:\n")
                  emitCaseEnd caseLhsInfo caseExpr
              emitCase exprVar isFirst (XObj (Sym firstPath _) caseLhsInfo _, caseExpr) =
                -- Single variable
                do
                  appendToSrc (addIndent indent)
                  unless isFirst (appendToSrc "else ")
                  appendToSrc "if(true) {\n"
                  appendToSrc (addIndent indent' ++ tyToCLambdaFix exprTy ++ " " ++ tempVarToAvoidClash ++ " = " ++ exprVar ++ ";\n")
                  appendToSrc
                    ( addIndent indent' ++ tyToCLambdaFix exprTy ++ " "
                        ++ pathToC firstPath
                        ++ " = "
                        ++ tempVarToAvoidClash
                        ++ ";\n" -- Store the whole expr in a variable
                    )
                  emitCaseEnd caseLhsInfo caseExpr
              emitCase _ _ x =
                error ("Fell through: " ++ show x)
              emitCaseEnd caseLhsInfo caseExpr = do
                emitLineDir caseExpr
                caseExprRetVal <- visit indent' caseExpr
                when isNotVoid $
                  appendToSrc (addIndent indent' ++ retVar ++ " = " ++ caseExprRetVal ++ ";\n")
                delete indent' (infoDelete (infoOrUnknown caseLhsInfo))
                appendToSrc (addIndent indent ++ "}\n")
           in do
                exprVar <- visit indent expr
                when isNotVoid $
                  let t = fromMaybe (error "emit: match expression has no type") $ ty
                   in appendToSrc (addIndent indent ++ tyToCLambdaFix t ++ " " ++ retVar ++ ";\n")
                zipWithM_ (emitCase exprVar) (True : repeat False) (pairwise rest)
                appendToSrc (addIndent indent ++ "else UNHANDLED(\"" ++ takeFileName (infoFile info) ++ "\", " ++ show (infoLine info) ++ ");\n")
                pure retVar
        XObj (Match _) _ _ : _ ->
          error "Fell through match."
        -- While
        [XObj While _ _, expr, body] ->
          let indent' = indent + indentAmount
              exprTy = fromMaybe (error "emit: called on while expression has no type") $ xobjTy expr
              conditionVar = freshVar info
              exprInfo = infoOrUnknown $ xobjInfo expr
           in do
                exprRetVar <- visitWhileExpression indent
                appendToSrc (addIndent indent ++ tyToCLambdaFix exprTy ++ " " ++ conditionVar ++ " = " ++ exprRetVar ++ ";\n")
                delete indent (infoDelete exprInfo)
                appendToSrc (addIndent indent ++ "while (" ++ conditionVar ++ ") {\n")
                emitLineDir body
                _ <- visit indent' body
                exprRetVar' <- visitWhileExpression indent'
                delete indent' (infoDelete info)
                appendToSrc (addIndent indent' ++ conditionVar ++ " = " ++ exprRetVar' ++ ";\n")
                appendToSrc (addIndent indent ++ "}\n")
                pure ""
          where
            visitWhileExpression :: Int -> State EmitterState String
            visitWhileExpression ind =
              do
                s <- get
                let (exprRetVar, exprResultState) = runState (visit ind expr) emptyEmitterState
                    exprBuilder = emitterBuilder exprResultState
                modify
                  ( \x ->
                      x
                        { emitterBuilder = emitterBuilder s <> exprBuilder
                        }
                  )
                pure exprRetVar
        -- Do
        XObj Do _ _ : expressions ->
          do
            let lastExpr = last expressions
                retVar = freshVar info
            mapM_ (visit indent) (init expressions)
            let lastTy = fromMaybe (error "emit: final expression in do has no type") $ xobjTy lastExpr
            if lastTy == UnitTy
              then do
                _ <- visit indent lastExpr
                pure ""
              else do
                lastRet <- visit indent lastExpr
                appendToSrc (addIndent indent ++ tyToCLambdaFix lastTy ++ " " ++ retVar ++ " = " ++ lastRet ++ ";\n")
                pure retVar
        -- Set!
        [XObj SetBang _ _, variable, value] ->
          do
            valueVar <- visit indent value
            let properVariableName =
                  case variable of
                    (XObj (Lst (XObj (Sym (SymPath _ "copy") _) _ _ : (XObj (Sym sym _) _ _) : _)) _ _) -> "*" ++ pathToC sym
                    (XObj (Sym sym _) _ _) -> pathToC sym
                    _ -> error (show (CannotSet variable))
                varInfo = infoOrUnknown $ xobjInfo variable
            --appendToSrc (addIndent indent ++ "// " ++ show (length (infoDelete varInfo)) ++ " deleters for " ++ properVariableName ++ ":\n")
            delete indent (infoDelete varInfo)
            appendToSrc
              ( addIndent indent ++ properVariableName ++ " = " ++ valueVar ++ "; "
                  ++ " // "
                  ++ show (fromMaybe (VarTy "?") (xobjTy variable))
                  ++ " = "
                  ++ show (fromMaybe (VarTy "?") (xobjTy value))
                  ++ "\n"
              )
            pure ""
        -- The
        [XObj The _ _, _, value] ->
          do
            var <- visit indent value
            let t = fromMaybe (error "emit: called emit on the with no type") ty
                fresh = mangle (freshVar info)
            unless
              (isUnit t)
              (appendToSrc (addIndent indent ++ tyToCLambdaFix t ++ " " ++ fresh ++ " = " ++ var ++ "; // From the 'the' function.\n"))
            pure fresh
        -- Ref
        [XObj Ref _ _, value] ->
          do
            var <- visit indent value
            let t = case ty of
                  Just typ -> typ
                  _ -> error "emit: called emit on ref with no type"
                fresh = mangle (freshVar info)
            case t of
              (RefTy UnitTy _) -> appendToSrc ""
              _ ->
                if isNumericLiteral value
                  then do
                    let literal = freshVar info ++ "_lit"
                        literalTy = case xobjTy value of
                          Just typ -> typ
                          _ -> error "called emit on ref without value type"
                    appendToSrc (addIndent indent ++ "static " ++ tyToCLambdaFix literalTy ++ " " ++ literal ++ " = " ++ var ++ ";\n")
                    appendToSrc (addIndent indent ++ tyToCLambdaFix t ++ " " ++ fresh ++ " = &" ++ literal ++ "; // ref\n")
                  else appendToSrc (addIndent indent ++ tyToCLambdaFix t ++ " " ++ fresh ++ " = &" ++ var ++ "; // ref\n")
            pure fresh
        -- Deref
        [XObj Deref _ _, value] ->
          do
            x <- visit indent value
            pure ("(*" ++ x ++ ")")
        -- Deftype
        XObj (Deftype _) _ _ : XObj (Sym _ _) _ _ : _ ->
          pure ""
        -- DefSumtype
        XObj (DefSumtype _) _ _ : XObj (Sym _ _) _ _ : _ ->
          pure ""
        -- Template
        [XObj (Deftemplate _) _ _, XObj (Sym _ _) _ _] ->
          pure ""
        [XObj (Instantiate template) _ _, XObj (Sym path _) _ _] ->
          case toCMode of
            Globals ->
              pure ""
            _ ->
              do
                case ty of
                  Just t ->
                    do
                      appendToSrc (templateToC template path t)
                      pure ""
                  _ -> pure "" -- called with no type
                  -- Alias
        XObj (Defalias _) _ _ : _ ->
          pure ""
        -- External
        XObj (External _) _ _ : _ ->
          pure ""
        -- Macro
        XObj Macro _ _ : _ ->
          pure ""
        -- Dynamic
        XObj Dynamic _ _ : _ ->
          pure ""
        -- DefDynamic
        XObj DefDynamic _ _ : _ ->
          pure ""
        -- Command
        XObj (Command _) _ _ : _ ->
          pure ""
        -- Primitive
        XObj (Primitive _) _ _ : _ ->
          pure ""
        -- Interface
        XObj (Interface _ _) _ _ : _ ->
          pure ""
        -- Break
        [XObj Break minfo _] -> do
          case minfo of
            Just i -> delete indent (infoDelete i)
            Nothing -> return ()
          appendToSrc (addIndent indent ++ "break;\n")
          appendToSrc (addIndent indent ++ "// Unreachable:\n")
          pure ""
        -- Function application (functions with overridden names)
        func@(XObj (Sym _ (LookupGlobalOverride overriddenName)) _ _) : args ->
          do
            argListAsC <- createArgList indent True args -- The 'True' means "unwrap lambdas" which is always the case for functions with overriden names (they are external)
            let retTy = case xobjTy func of
                  Just (FuncTy _ rt _) -> rt
                  _ -> error ("No type on func " ++ show func)
                callFunction = overriddenName ++ "(" ++ argListAsC ++ ");\n"
            if isUnit retTy
              then do
                appendToSrc (addIndent indent ++ callFunction)
                pure ""
              else do
                let varName = freshVar info
                appendToSrc (addIndent indent ++ tyToCLambdaFix retTy ++ " " ++ varName ++ " = " ++ callFunction)
                pure varName
        -- Function application (global symbols that are functions -- lambdas stored in def:s need to be called like locals, see below)
        func@(XObj (Sym path (LookupGlobal mode AFunction)) _ _) : args ->
          do
            argListAsC <- createArgList indent (mode == ExternalCode) args
            let retTy = case xobjTy func of
                  Just (FuncTy _ rt _) -> rt
                  _ -> error "failed to emit function application for non-function type."
                funcToCall = pathToC path
            if isUnit retTy
              then do
                appendToSrc (addIndent indent ++ funcToCall ++ "(" ++ argListAsC ++ ");\n")
                pure ""
              else do
                let varName = freshVar info
                appendToSrc (addIndent indent ++ tyToCLambdaFix retTy ++ " " ++ varName ++ " = " ++ funcToCall ++ "(" ++ argListAsC ++ ");\n")
                pure varName
        -- Function application (on local symbols and global defs containing lambdas)
        func : args ->
          do
            funcToCall <- visit indent func
            let unwrapLambdas = case func of
                  XObj (Sym _ (LookupGlobal ExternalCode _)) _ _ -> True
                  _ -> False
            argListAsC <- createArgList indent unwrapLambdas args
            let (argTys, retTy) = case xobjTy func of
                  Just (FuncTy at rt _) -> (at, rt)
                  _ -> error ("No type on func " ++ show func)
                voidless = remove isUnit argTys
                castToFn =
                  if unwrapLambdas
                    then tyToCLambdaFix retTy ++ "(*)(" ++ joinWithComma (map tyToCRawFunctionPtrFix voidless) ++ ")"
                    else tyToCLambdaFix retTy ++ "(*)(" ++ joinWithComma (map tyToCLambdaFix voidless) ++ ")"
                castToFnWithEnv =
                  if unwrapLambdas
                    then tyToCLambdaFix retTy ++ "(*)(" ++ joinWithComma (map tyToCRawFunctionPtrFix (StructTy (ConcreteNameTy (SymPath [] "LambdaEnv")) [] : voidless)) ++ ")"
                    else tyToCLambdaFix retTy ++ "(*)(" ++ joinWithComma (map tyToCLambdaFix (StructTy (ConcreteNameTy (SymPath [] "LambdaEnv")) [] : voidless)) ++ ")"
                callLambda = funcToCall ++ ".env ? ((" ++ castToFnWithEnv ++ ")" ++ funcToCall ++ ".callback)" ++ "(" ++ funcToCall ++ ".env" ++ (if null argListAsC then "" else ", ") ++ argListAsC ++ ") : ((" ++ castToFn ++ ")" ++ funcToCall ++ ".callback)(" ++ argListAsC ++ ");\n"
            if isUnit retTy
              then do
                appendToSrc (addIndent indent ++ callLambda)
                pure ""
              else do
                let varName = freshVar info
                appendToSrc (addIndent indent ++ tyToCLambdaFix retTy ++ " " ++ varName ++ " = " ++ callLambda)
                pure varName
        -- Empty list
        [] -> do
          appendToSrc (addIndent indent ++ "/* () */\n")
          pure ""
    visitList _ xobj@(XObj (Lst _) Nothing Nothing) = error ("List is missing info and type! " ++ show xobj)
    visitList _ xobj@(XObj (Lst _) Nothing (Just _)) = error ("List is missing info! " ++ show xobj)
    visitList _ xobj = error ("Must visit list! " ++ show xobj)
    -- TCO: detect self-recursive calls in tail position
    hasSelfTailCalls :: XObj -> Bool
    hasSelfTailCalls (XObj (Lst xs) _ _) = case xs of
      XObj (Sym _ LookupRecursive) _ _ : _ -> True
      [XObj If _ _, _, ifTrue, ifFalse] -> hasSelfTailCalls ifTrue || hasSelfTailCalls ifFalse
      XObj Do _ _ : exprs@(_ : _) -> hasSelfTailCalls (last exprs)
      [XObj Let _ _, XObj (Arr _) _ _, body'] -> hasSelfTailCalls body'
      [XObj The _ _, _, value] -> hasSelfTailCalls value
      _ -> False
    hasSelfTailCalls _ = False
    isSafeForTCO :: [XObj] -> Bool
    isSafeForTCO = all (not . hasRef . forceTy)
      where
        hasRef (RefTy _ _) = True
        hasRef (FuncTy ats rt _) = any hasRef ats || hasRef rt
        hasRef (StructTy _ tys) = any hasRef tys
        hasRef (PointerTy p) = hasRef p
        hasRef _ = False
    visitTCO :: Int -> [(String, Ty)] -> Ty -> Set.Set Deleter -> Maybe (String, Map.Map String Int) -> XObj -> State EmitterState ()
    visitTCO indent' params retTy accDels mutualInfo xobj = case xobj of
      XObj (Lst (func : args)) _ _ | isTCOCall func -> do
        argVars <- mapM (visit indent') (remove (isUnit . forceTy) args)
        mapM_ (visit indent') (filter (isUnit . forceTy) args)
        zipWithM_ (\(p, t) v -> appendToSrc (addIndent indent' ++ tyToCLambdaFix t ++ " __tco_" ++ p ++ " = " ++ v ++ ";\n")) params argVars
        delete indent' accDels
        mapM_ (\(p, _) -> appendToSrc (addIndent indent' ++ p ++ " = __tco_" ++ p ++ ";\n")) params
        case mutualInfo of
          Just (tagVar, tagMap) | not (isSelfCall func) ->
            case func of
              XObj (Sym path _) _ _ -> case Map.lookup (pathToC path) tagMap of
                Just tag -> appendToSrc (addIndent indent' ++ tagVar ++ " = " ++ show tag ++ ";\n")
                _ -> pure ()
              _ -> pure ()
          _ -> pure ()
        appendToSrc (addIndent indent' ++ "continue;\n")
      XObj (Lst [XObj If _ _, expr, ifTrue, ifFalse]) _ _ -> do
        exprVar <- visit indent' expr
        appendToSrc (addIndent indent' ++ "if (" ++ exprVar ++ ") {\n")
        emitLineDir ifTrue
        visitTCO (indent' + indentAmount) params retTy (accDels `Set.union` infoDelete (infoOrUnknown $ xobjInfo ifTrue)) mutualInfo ifTrue
        appendToSrc (addIndent indent' ++ "} else {\n")
        emitLineDir ifFalse
        visitTCO (indent' + indentAmount) params retTy (accDels `Set.union` infoDelete (infoOrUnknown $ xobjInfo ifFalse)) mutualInfo ifFalse
        appendToSrc (addIndent indent' ++ "}\n")
      XObj (Lst (XObj Do _ _ : exprs@(_ : _))) _ _ -> do
        mapM_ (visit indent') (init exprs)
        visitTCO indent' params retTy accDels mutualInfo (last exprs)
      XObj (Lst [XObj Let _ _, XObj (Arr bindings) _ _, body']) minfo _ -> do
        let indent'' = indent' + indentAmount
        appendToSrc (addIndent indent' ++ "/* let */ {\n")
        let emitBinding (XObj (Sym (SymPath _ n) _) _ _) e = do
              ret <- visit indent'' e
              let bt = fromMaybe (error "emit: let binding has no type") $ xobjTy e
              unless (isUnit bt) $ appendToSrc (addIndent indent'' ++ tyToCLambdaFix bt ++ " " ++ mangle n ++ " = " ++ ret ++ ";\n")
            emitBinding _ _ = error "Invalid binding."
        mapM_ (uncurry emitBinding) (pairwise bindings)
        emitLineDir body'
        visitTCO indent'' params retTy (accDels `Set.union` maybe Set.empty infoDelete minfo) mutualInfo body'
        appendToSrc (addIndent indent' ++ "}\n")
      XObj (Lst [XObj The _ _, _, value]) _ _ ->
        visitTCO indent' params retTy accDels mutualInfo value
      _ -> do
        ret <- visit indent' xobj
        delete indent' accDels
        case retTy of
          UnitTy -> appendToSrc (addIndent indent' ++ "return;\n")
          _ -> appendToSrc (addIndent indent' ++ "return " ++ ret ++ ";\n")
      where
        isSelfCall (XObj (Sym _ LookupRecursive) _ _) = True
        isSelfCall _ = False
        isTCOCall (XObj (Sym _ LookupRecursive) _ _) = True
        isTCOCall (XObj (Sym path (LookupGlobal CarpLand AFunction)) _ _) =
          case mutualInfo of
            Just (_, tagMap) -> Map.member (pathToC path) tagMap
            Nothing -> False
        isTCOCall _ = False
    emitMutualGroup :: Int -> [(SymPath, Binder)] -> State EmitterState String
    emitMutualGroup indent group =
      let members = [(p, m, al, b, rt) | (p, Binder m (XObj (Lst [XObj (Defn _) _ _, _, XObj (Arr al) _ _, b]) _ (Just (FuncTy _ rt _)))) <- group]
          (_, _, firstArgs, _, retTy) = head members
          params = [(mangle n, forceTy p) | p@(XObj (Sym (SymPath _ n) _) _ _) <- firstArgs, not (isUnit (forceTy p))]
          paramsC = joinWithComma [tyToCLambdaFix t ++ " " ++ n | (n, t) <- params]
          tagMap = Map.fromList (zip (map (pathToC . fst5) members) [0 ..])
          mergedName = "__mutual_" ++ intercalate "_" (map (pathToC . fst5) members)
          retTyC = tyToCLambdaFix retTy
          tagParam = if null paramsC then "int __tag" else "int __tag, " ++ paramsC
          innerIndent = indent + indentAmount
          switchIndent = innerIndent + indentAmount
          caseIndent = switchIndent + indentAmount
          bodyIndent = caseIndent + indentAmount
          fst5 (a, _, _, _, _) = a
       in do
            -- Merged dispatch function
            appendToSrc ("static " ++ retTyC ++ " " ++ mergedName ++ "(" ++ tagParam ++ ") {\n")
            appendToSrc (addIndent innerIndent ++ "while(1) {\n")
            appendToSrc (addIndent switchIndent ++ "switch(__tag) {\n")
            zipWithM_
              ( \tag (_, _, argList, body, rt) -> do
                  let fnInfo = case binderXObj (snd (group !! tag)) of
                        XObj _ (Just i) _ -> i
                        _ -> Info 0 0 "" Set.empty (-1)
                      memberParams = [(mangle n, forceTy p) | p@(XObj (Sym (SymPath _ n) _) _ _) <- argList, not (isUnit (forceTy p))]
                  appendToSrc (addIndent caseIndent ++ "case " ++ show tag ++ ": {\n")
                  -- Emit aliases when param names differ from canonical
                  zipWithM_
                    ( \(cn, ct) (mn, _) ->
                        when (cn /= mn) $
                          appendToSrc (addIndent bodyIndent ++ tyToCLambdaFix ct ++ " " ++ mn ++ " = " ++ cn ++ ";\n")
                    )
                    params
                    memberParams
                  visitTCO bodyIndent params rt (infoDelete fnInfo) (Just ("__tag", tagMap)) body
                  appendToSrc (addIndent caseIndent ++ "}\n")
              )
              [(0 :: Int) ..]
              members
            appendToSrc (addIndent switchIndent ++ "}\n")
            appendToSrc (addIndent innerIndent ++ "}\n")
            appendToSrc "}\n\n"
            -- Wrapper functions
            mapM_
              ( \(tag, (path, m, argList, _, _)) -> do
                  let decl = defnToDeclaration m path argList retTy
                      mParams = [mangle n | p@(XObj (Sym (SymPath _ n) _) _ _) <- argList, not (isUnit (forceTy p))]
                      callArgs = if null mParams then show tag else show tag ++ ", " ++ joinWithComma mParams
                  appendToSrc (decl ++ " {\n")
                  appendToSrc (addIndent innerIndent ++ "return " ++ mergedName ++ "(" ++ callArgs ++ ");\n")
                  appendToSrc "}\n\n"
              )
              (zip [(0 :: Int) ..] members)
            pure ""
    createArgList :: Int -> Bool -> [XObj] -> State EmitterState String
    createArgList indent unwrapLambdas args =
      do
        argStrings <- mapM (visit indent) (remove (isUnit . forceTy) args)
        let argTypes = map forceTy args
            unitless = remove isUnit argTypes
            -- Run side effects
            sideEffects = mapM (visit indent) (filter (isUnit . forceTy) args) <&> intercalate ";\n"
            unwrapped =
              joinWithComma $
                if unwrapLambdas
                  then zipWith unwrapLambda argStrings unitless
                  else argStrings
        sideEffects >> pure unwrapped
    unwrapLambda :: String -> Ty -> String
    unwrapLambda variableName ty =
      if isFunctionType ty
        then variableName ++ ".callback"
        else variableName
    visitArray :: Int -> XObj -> State EmitterState String
    visitArray indent (XObj (Arr xobjs) (Just i) t) =
      do
        let arrayVar = freshVar i
            len = length xobjs
        case t of
          Just (StructTy (ConcreteNameTy (SymPath [] "Array")) [innerTy]) ->
            do
              appendToSrc
                ( addIndent indent ++ "Array " ++ arrayVar
                    ++ " = { .len = "
                    ++ show len
                    ++ ","
                    ++ " .capacity = "
                    ++ show len
                    ++ ","
                    ++ " .data = CARP_MALLOC(sizeof("
                    ++ tyToCLambdaFix innerTy
                    ++ ") * "
                    ++ show len
                    ++ ") };\n"
                )
              zipWithM_ (visitArrayElement indent arrayVar innerTy) [0 ..] xobjs
              pure arrayVar
          _ -> pure "" -- called with non-array type
    visitArray _ _ = error "Must visit array!"
    visitArrayElement :: Int -> String -> Ty -> Int -> XObj -> State EmitterState ()
    visitArrayElement indent arrayVar innerTy index xobj =
      do
        visited <- visit indent xobj
        appendToSrc
          ( case innerTy of
              UnitTy -> "/* () */"
              _ ->
                addIndent indent ++ "((" ++ tyToCLambdaFix innerTy ++ "*)" ++ arrayVar
                  ++ ".data)["
                  ++ show index
                  ++ "] = "
                  ++ visited
                  ++ ";\n"
          )
        pure ()
    visitStaticArray :: Int -> XObj -> State EmitterState String
    visitStaticArray indent (XObj (StaticArr xobjs) (Just i) t) =
      do
        let arrayVar = freshVar i
            retVar = arrayVar ++ "_retref"
            arrayDataVar = arrayVar ++ "_data"
            len = length xobjs
        case t of
          Just tt@(RefTy (StructTy (ConcreteNameTy (SymPath [] "StaticArray")) [innerTy]) _) ->
            do
              appendToSrc (addIndent indent ++ tyToCLambdaFix innerTy ++ " " ++ arrayDataVar ++ "[" ++ show len ++ "];\n")
              appendToSrc
                ( addIndent indent ++ "Array " ++ arrayVar
                    ++ " = { .len = "
                    ++ show len
                    ++ ","
                    ++ " /* .capacity = DOES NOT MATTER, STACK ALLOCATED ARRAY, */"
                    ++ " .data = "
                    ++ arrayDataVar
                    ++ " };\n"
                )
              zipWithM_ (visitStaticArrayElement indent arrayDataVar innerTy) [0 ..] xobjs
              appendToSrc (addIndent indent ++ tyToCLambdaFix tt ++ " " ++ retVar ++ " = &" ++ arrayVar ++ ";\n")
              pure retVar
          _ -> pure "" -- called with non-array type
    visitStaticArray _ _ = error "Must visit static array!"
    visitStaticArrayElement :: Int -> String -> Ty -> Int -> XObj -> State EmitterState ()
    visitStaticArrayElement indent arrayDataVar _ index xobj =
      do
        visited <- visit indent xobj
        appendToSrc (addIndent indent ++ arrayDataVar ++ "[" ++ show index ++ "] = " ++ visited ++ ";\n")
        pure ()

delete :: Int -> Set.Set Deleter -> State EmitterState ()
delete indent dels = mapM_ deleterToC dels
  where
    deleterToC :: Deleter -> State EmitterState ()
    deleterToC FakeDeleter {} =
      pure ()
    deleterToC PrimDeleter {} =
      pure ()
    deleterToC RefDeleter {} =
      pure ()
    deleterToC deleter@ProperDeleter {} = do
      let v = mangle (deleterVariable deleter)
      case dropPath deleter of
        Just path ->
          appendToSrc $ addIndent indent ++ "" ++ pathToC path ++ "(&" ++ v ++ ");\n"
        Nothing -> pure ()
      appendToSrc $ addIndent indent ++ "" ++ pathToC (deleterPath deleter) ++ "(" ++ v ++ ");\n"

defnToDeclaration :: MetaData -> SymPath -> [XObj] -> Ty -> String
defnToDeclaration meta path@(SymPath _ name) argList retTy =
  let override = Meta.getString (Meta.getCompilerKey Meta.CNAME) meta
      annotations = case fromMaybe emptyList (Meta.get "annotations" meta) of
        (XObj (Lst xs) _ _) -> xs
        _ -> []
      annotationsStr = joinWith " " (map strToC annotations)
      sep = if not (null annotationsStr) then " " else ""
      fullname = if (null override) then (pathToC path) else override
   in annotationsStr ++ sep
        ++ if name == "main"
          then "int main(int argc, char** argv)"
          else
            let retTyAsC = tyToCLambdaFix retTy
                paramsAsC = paramListToC argList
             in (retTyAsC ++ " " ++ fullname ++ "(" ++ paramsAsC ++ ")")
  where
    strToC (XObj (Str s) _ _) = s
    strToC xobj = pretty xobj

templateToC :: Template -> SymPath -> Ty -> String
templateToC template path actualTy =
  let mappings = unifySignatures (templateSignature template) actualTy
      declaration = templateDeclaration template actualTy
      definition = templateDefinition template actualTy
      tokens = concatMap (concretizeTypesInToken mappings (pathToC path) declaration) definition
   in concatMap show tokens ++ "\n"

templateToDeclaration :: Template -> SymPath -> Ty -> String
templateToDeclaration template path actualTy =
  let mappings = unifySignatures (templateSignature template) actualTy
      e = error "Can't refer to declaration in declaration."
      declaration = templateDeclaration template actualTy
      tokens = concatMap (concretizeTypesInToken mappings (pathToC path) e) declaration
      stokens = concatMap show tokens
      term = if "#define" `isPrefixOf` stokens then "\n" else ";\n"
   in stokens ++ term

memberToDecl :: Int -> Ty -> (XObj, XObj) -> State EmitterState ()
memberToDecl indent selfTy (memberName, memberType) =
  case xobjToTy memberType of
    -- Handle function pointers as members specially to allow members that are functions referring to the struct itself.
    Just t -> appendToSrc (addIndent indent ++ memberTypeToC selfTy t ++ " " ++ mangle (getName memberName) ++ ";\n")
    Nothing -> error ("Invalid memberType: " ++ show memberType)

memberTypeToC :: Ty -> Ty -> String
memberTypeToC selfTy t =
  case t of
    PointerTy inner
      | isSelfStruct selfTy inner ->
        "struct " ++ tyToC inner ++ "*"
    _ ->
      tyToCLambdaFix t
  where
    isSelfStruct (StructTy (ConcreteNameTy selfPath) selfVars) (StructTy (ConcreteNameTy innerPath) innerVars) =
      selfPath == innerPath && selfVars == innerVars
    isSelfStruct _ _ = False

defStructToDeclaration :: Ty -> SymPath -> [XObj] -> String
defStructToDeclaration structTy@(StructTy _ _) _ rest =
  let indent = indentAmount
      typedefCaseToMemberDecl :: XObj -> State EmitterState [()]
      -- ANSI C doesn't allow empty structs, insert a dummy member to keep the compiler happy.
      typedefCaseToMemberDecl (XObj (Arr []) _ _) = sequence $ pure $ appendToSrc (addIndent indent ++ "char __dummy;\n")
      typedefCaseToMemberDecl (XObj (Arr members) _ _) = mapM (memberToDecl indent structTy) (remove (isUnit . fromJust . xobjToTy . snd) (pairwise members))
      typedefCaseToMemberDecl _ = error "Invalid case in typedef."
      -- Note: the names of types are not namespaced
      visit = do
        appendToSrc ("struct " ++ tyToC structTy ++ " {\n")
        mapM_ typedefCaseToMemberDecl rest
        appendToSrc ("};\n")
   in if isTypeGeneric structTy
        then "" -- ("// " ++ show structTy ++ "\n")
        else renderEmitterState (execState visit emptyEmitterState)
defStructToDeclaration _ _ _ = error "defstructtodeclaration"

defSumtypeToDeclaration :: Ty -> [XObj] -> String
defSumtypeToDeclaration sumTy@(StructTy _ _) rest =
  let indent = indentAmount
      visit = do
        appendToSrc ("struct " ++ tyToC sumTy ++ " {\n")
        appendToSrc (addIndent indent ++ "union {\n")
        mapM_ (emitSumtypeCase indent) rest
        appendToSrc (addIndent indent ++ "char __dummy;\n")
        appendToSrc (addIndent indent ++ "} u;\n")
        appendToSrc (addIndent indent ++ "char _tag;\n")
        appendToSrc ("};\n")
        --appendToSrc ("// " ++ show typeVariables ++ "\n")
        mapM_ emitSumtypeCaseTagDefinition (zip [0 ..] rest)
      emitSumtypeCase :: Int -> XObj -> State EmitterState ()
      emitSumtypeCase ind (XObj (Lst [XObj (Sym (SymPath [] caseName) _) _ _, XObj (Arr []) _ _]) _ _) =
        appendToSrc (addIndent ind ++ "// " ++ caseName ++ "\n")
      emitSumtypeCase ind (XObj (Lst [XObj (Sym (SymPath [] caseName) _) _ _, XObj (Arr memberTys) _ _]) _ _) =
        do
          appendToSrc (addIndent ind ++ "struct {\n")
          let members = zip anonMemberSymbols (remove (isUnit . fromJust . xobjToTy) memberTys)
          mapM_ (memberToDecl (ind + indentAmount) sumTy) members
          appendToSrc (addIndent ind ++ "} " ++ caseName ++ ";\n")
      emitSumtypeCase ind (XObj (Sym (SymPath [] caseName) _) _ _) =
        appendToSrc (addIndent ind ++ "// " ++ caseName ++ "\n")
      emitSumtypeCase _ _ = error "emitsumtypecase"
      emitSumtypeCaseTagDefinition :: (Int, XObj) -> State EmitterState ()
      emitSumtypeCaseTagDefinition (tagIndex, XObj (Lst [XObj (Sym (SymPath [] caseName) _) _ _, _]) _ _) =
        appendToSrc ("#define " ++ tagName sumTy caseName ++ " " ++ show tagIndex ++ "\n")
      emitSumtypeCaseTagDefinition (tagIndex, XObj (Sym (SymPath [] caseName) _) _ _) =
        appendToSrc ("#define " ++ tagName sumTy caseName ++ " " ++ show tagIndex ++ "\n")
      emitSumtypeCaseTagDefinition _ = error "emitsumtypecasetagdefinition"
   in if isTypeGeneric sumTy
        then ""
        else renderEmitterState (execState visit emptyEmitterState)
defSumtypeToDeclaration _ _ = error "defsumtypetodeclaration"

defaliasToDeclaration :: Ty -> SymPath -> String
defaliasToDeclaration t path =
  case t of
    (FuncTy argTys retTy _) ->
      "typedef " ++ tyToCLambdaFix retTy ++ "(*" ++ pathToC path ++ ")("
        ++ intercalate ", " (map fixer argTys)
        ++ ");\n"
    _ -> "typedef " ++ tyToC t ++ " " ++ pathToC path ++ ";\n"
  where
    fixer UnitTy = "void*"
    fixer x = tyToCLambdaFix x

toDeclaration :: Binder -> String
toDeclaration (Binder meta xobj@(XObj (Lst xobjs) _ ty)) =
  case xobjs of
    [XObj (Defn _) _ _, XObj (Sym path _) _ _, XObj (Arr argList) _ _, _] ->
      case ty of
        (Just (FuncTy _ retTy _)) -> defnToDeclaration meta path argList retTy ++ ";\n"
        _ -> "" -- called with non-function type, emit nothing.
    [XObj Def _ _, XObj (Sym path _) _ _, _] ->
      case ty of
        Just t ->
          let cname = (Meta.getString (Meta.getCompilerKey Meta.CNAME) meta)
              fullname = if (null cname) then pathToC path else cname
           in if (isUnit t)
                then ""
                else tyToCLambdaFix t ++ " " ++ fullname ++ ";\n"
        _ -> "" -- called with no type, emit nothing.
    XObj (Deftype t) _ _ : XObj (Sym path _) _ _ : rest ->
      defStructToDeclaration t path rest
    XObj (DefSumtype t) _ _ : XObj (Sym _ _) _ _ : rest ->
      defSumtypeToDeclaration t rest
    XObj (Deftemplate _) _ _ : _ ->
      ""
    XObj Macro _ _ : _ ->
      ""
    XObj Dynamic _ _ : _ ->
      ""
    XObj DefDynamic _ _ : _ ->
      ""
    [XObj (Instantiate template) _ _, XObj (Sym path _) _ _] ->
      case ty of
        Just t -> templateToDeclaration template path t
        _ -> "" -- called with no type, emit nothing.
    [XObj (Defalias aliasTy) _ _, XObj (Sym path _) _ _] ->
      defaliasToDeclaration aliasTy path
    [XObj (Interface _ _) _ _, _] ->
      ""
    XObj (External _) _ _ : _ ->
      ""
    XObj (ExternalType Nothing) _ _ : _ ->
      ""
    XObj (ExternalType (Just override)) _ _ : XObj (Sym path _) _ _ : _ ->
      "typedef " ++ override ++ " " ++ tyToC (StructTy (ConcreteNameTy path) []) ++ ";"
    XObj (Command _) _ _ : _ ->
      ""
    XObj (Primitive _) _ _ : _ ->
      ""
    _ -> error ("Internal compiler error: Can't emit other kinds of definitions: " ++ show xobj)
toDeclaration (Binder _ (XObj (Sym (SymPath [] "dummy") Symbol) Nothing (Just IntTy))) = ""
toDeclaration (Binder _ (XObj (Closure _ _) _ _)) = ""
toDeclaration _ = error "Missing case."

paramListToC :: [XObj] -> String
paramListToC xobjs =
  if null $ joinWithComma (map getParam xobjs)
    then ""
    else joinWithComma (map getParam (remove (isUnit . forceTy) xobjs))
  where
    getParam :: XObj -> String
    getParam (XObj (Sym (SymPath _ name) _) _ (Just t)) = tyToCLambdaFix t ++ " " ++ mangle name
    getParam invalid = error (show (InvalidParameter invalid))

projectIncludesToC :: Project -> String
projectIncludesToC proj = intercalate "\n" (map includerToC includes) ++ "\n\n"
  where
    includerToC (SystemInclude file) = "#include <" ++ file ++ ">"
    includerToC (RelativeInclude file) = "#include \"" ++ file ++ "\""
    includes = projectIncludes proj

projectPreprocToC :: Project -> String
projectPreprocToC proj = intercalate "\n" preprocs ++ "\n\n"
  where
    preprocs = projectPreproc proj

binderToC :: ToCMode -> Bool -> Binder -> Either ToCError String
binderToC toCMode emitLines binder =
  let xobj = binderXObj binder
   in case xobj of
        XObj (External _) _ _ -> Right ""
        XObj (ExternalType _) _ _ -> Right ""
        XObj (Command _) _ _ -> Right ""
        XObj (Mod env _) _ _ -> envToC env toCMode emitLines
        _ -> case xobjTy xobj of
          Just t ->
            if isTypeGeneric t
              then Right ""
              else do
                checkForUnresolvedSymbols xobj
                pure (toC toCMode emitLines [] binder)
          Nothing -> Left (BinderIsMissingType binder)

binderToDeclaration :: TypeEnv -> Binder -> Either ToCError String
binderToDeclaration typeEnv binder =
  let xobj = binderXObj binder
   in case xobj of
        XObj (Mod env _) _ _ -> envToDeclarations typeEnv env
        _ -> case xobjTy xobj of
          Just t -> if isTypeGeneric t then Right "" else Right (toDeclaration binder ++ "")
          Nothing -> Left (BinderIsMissingType binder)

-- | Find function paths called in tail position (excluding self-recursive LookupRecursive)
tailCallTargets :: XObj -> [SymPath]
tailCallTargets (XObj (Lst xs) _ _) = case xs of
  XObj (Sym path (LookupGlobal CarpLand AFunction)) _ _ : _ -> [path]
  [XObj If _ _, _, t, f] -> tailCallTargets t ++ tailCallTargets f
  XObj Do _ _ : es@(_ : _) -> tailCallTargets (last es)
  [XObj Let _ _, XObj (Arr _) _ _, b] -> tailCallTargets b
  [XObj The _ _, _, v] -> tailCallTargets v
  _ -> []
tailCallTargets _ = []

-- | Extract defn info from a binder: (path, argList, body, retTy)
defnParts :: Binder -> Maybe (SymPath, [XObj], XObj, Ty)
defnParts (Binder _ (XObj (Lst [XObj (Defn _) _ _, XObj (Sym path _) _ _, XObj (Arr args) _ _, body]) _ (Just (FuncTy _ rt _)))) =
  Just (path, args, body, rt)
defnParts _ = Nothing

-- | Find groups of mutually recursive functions suitable for merged dispatch TCO.
-- Requires: >1 member, same return type, same param types and names, no ref params.
findMutualGroups :: Env -> [[(SymPath, Binder)]]
findMutualGroups env =
  let fnBinders = [(path, b) | (_, b) <- Map.toList (envBindings env), Just (path, _, _, _) <- [defnParts b]]
      graph =
        [ (pb, pathToC path, map pathToC (tailCallTargets body))
          | pb@(path, b) <- fnBinders,
            Just (_, _, body, _) <- [defnParts b]
        ]
      sccs = stronglyConnComp graph
   in [members | CyclicSCC members <- sccs, isQualifying members]
  where
    isQualifying group =
      let infos = [(args, rt) | (_, b) <- group, Just (_, args, _, rt) <- [defnParts b]]
          retTys = map snd infos
          paramTys = map (map forceTy . fst) infos
       in length group > 1
            && all (== head retTys) (tail retTys)
            && all (== head paramTys) (tail paramTys)
            && all (\(_, b) -> maybe False (\(_, a, _, _) -> all (not . hasRef . forceTy) a) (defnParts b)) group
    hasRef (RefTy _ _) = True
    hasRef (FuncTy ats rt _) = any hasRef ats || hasRef rt
    hasRef (StructTy _ tys) = any hasRef tys
    hasRef (PointerTy p) = hasRef p
    hasRef _ = False

envToC :: Env -> ToCMode -> Bool -> Either ToCError String
envToC env toCMode emitLines =
  let binders' = Map.toList (envBindings env)
      mutualGroups = findMutualGroups env
      mutualPaths = Set.fromList [pathToC path | group <- mutualGroups, (path, _) <- group]
      isMutual (_, b) = case defnParts b of
        Just (path, _, _, _) -> Set.member (pathToC path) mutualPaths
        Nothing -> False
      emitGroup group = do
        mapM_ (checkForUnresolvedSymbols . binderXObj . snd) group
        pure (toC toCMode emitLines group (snd (head group)))
   in do
        okCodes <- mapM (binderToC toCMode emitLines . snd) (filter (not . isMutual) binders')
        mutualCodes <- mapM emitGroup mutualGroups
        pure (concat mutualCodes ++ concat okCodes)

globalsToC :: Bool -> Env -> Either ToCError String
globalsToC emitLines globalEnv =
  let allGlobalBinders = findAllGlobalVariables globalEnv
   in do
        okCodes <-
          mapM
            ( \(score, binder) ->
                fmap
                  (\s -> if s == "" then "" else ("\n    // Depth " ++ show score ++ "\n") ++ s)
                  (binderToC Globals emitLines binder)
            )
            (sortGlobalVariableBinders globalEnv allGlobalBinders)
        pure (concat okCodes)

-- | Similar to envToDeclarations, however, to get types, we need to traverse
-- the global environment, pull out local type envs from modules, then emit
-- binders for these types.
--
-- TODO: It should be possible to define a general function that works for both
-- value/type envs, then we can merge this and envToDeclarations
typeEnvToDeclarations :: TypeEnv -> Env -> Either ToCError String
typeEnvToDeclarations typeEnv global =
  let -- We need to carry the type environment to pass the correct environment on the binderToDeclaration call.
      bindersWithScore = (addEnvToScore typeEnv)
      mods = (findModules global)
      allScoredBinders = sortOn fst (foldl go bindersWithScore mods)
      forwardDecls = forwardDeclarations (map snd allScoredBinders)
   in do
        okDecls <-
          mapM
            ( \(score, binder) ->
                fmap
                  (\s -> if s == "" then "" else ("\n// Depth " ++ show score ++ "\n") ++ s)
                  (binderToDeclaration typeEnv binder)
            )
            allScoredBinders
        pure (forwardDecls ++ concat okDecls)
  where
    addEnvToScore tyE = (sortDeclarationBinders tyE global (map snd (Map.toList (binders tyE))))
    go sorted (XObj (Mod e t) _ _) = sorted ++ (foldl go (addEnvToScore t) (findModules e))
    go xs _ = xs

forwardDeclarations :: [Binder] -> String
forwardDeclarations binders' =
  let decls =
        Set.toList $
          Set.fromList $
            foldr
              ( \binder acc ->
                  case forwardDeclFromBinder binder of
                    Just decl -> decl : acc
                    Nothing -> acc
              )
              []
              binders'
   in if null decls then "" else concat decls ++ "\n"

forwardDeclFromBinder :: Binder -> Maybe String
forwardDeclFromBinder (Binder _ (XObj (Lst xobjs) _ _)) =
  case xobjs of
    XObj (Deftype t) _ _ : _ -> forward t
    XObj (DefSumtype t) _ _ : _ -> forward t
    _ -> Nothing
  where
    forward t =
      case t of
        StructTy {}
          | not (isTypeGeneric t) ->
            Just ("typedef struct " ++ tyToC t ++ " " ++ tyToC t ++ ";\n")
        _ -> Nothing
forwardDeclFromBinder _ = Nothing

envToDeclarations :: TypeEnv -> Env -> Either ToCError String
envToDeclarations typeEnv env =
  let bindersWithScore = sortDeclarationBinders typeEnv env (map snd (Map.toList (envBindings env)))
   in do
        okDecls <-
          mapM
            ( \(score, binder) ->
                fmap
                  (\s -> if s == "" then "" else ("\n// Depth " ++ show score ++ "\n") ++ s)
                  (binderToDeclaration typeEnv binder)
            )
            bindersWithScore
        pure (concat okDecls)

-- debugScorePair :: (Int, Binder) -> (Int, Binder)
-- debugScorePair (s,b) = trace ("Scored binder: " ++ show b ++ ", score: " ++ show s) (s,b)

sortDeclarationBinders :: TypeEnv -> Env -> [Binder] -> [(Int, Binder)]
sortDeclarationBinders typeEnv env binders' =
  --trace ("\nSORTED: " ++ (show (sortOn fst (map (scoreBinder typeEnv) binders))))
  sortOn fst (map (scoreTypeBinder typeEnv env) binders')

sortGlobalVariableBinders :: Env -> [Binder] -> [(Int, Binder)]
sortGlobalVariableBinders globalEnv binders' =
  sortOn fst (map (scoreValueBinder globalEnv Set.empty) binders')

checkForUnresolvedSymbols :: XObj -> Either ToCError ()
checkForUnresolvedSymbols = visit
  where
    visit :: XObj -> Either ToCError ()
    visit xobj =
      case xobjTy xobj of
        Nothing -> visitXObj
        Just t ->
          if isTypeGeneric t
            then Left (UnresolvedGenericType xobj)
            else visitXObj
      where
        visitXObj =
          case xobjObj xobj of
            (Lst _) -> visitList xobj
            (Arr _) -> visitArray xobj
            (StaticArr _) -> visitStaticArray xobj
            (MultiSym _ _) -> Left (UnresolvedMultiSymbol xobj)
            (InterfaceSym _) -> Left (UnresolvedInterfaceSymbol xobj)
            _ -> pure ()
    visitList :: XObj -> Either ToCError ()
    visitList (XObj (Lst xobjs) _ _) =
      case mapM visit xobjs of
        Left e -> Left e
        Right _ -> pure ()
    visitList _ = error "The function 'visitList' only accepts XObjs with lists in them."
    visitArray :: XObj -> Either ToCError ()
    visitArray (XObj (Arr xobjs) _ _) =
      case mapM visit xobjs of
        Left e -> Left e
        Right _ -> pure ()
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."
    visitStaticArray :: XObj -> Either ToCError ()
    visitStaticArray (XObj (StaticArr xobjs) _ _) =
      case mapM visit xobjs of
        Left e -> Left e
        Right _ -> pure ()
    visitStaticArray _ = error "The function 'visitStaticArray' only accepts XObjs with arrays in them."

wrapInInitFunction :: Bool -> String -> String
wrapInInitFunction with_core src =
  "void carp_init_globals(int argc, char** argv) {\n"
    ++ ( if with_core
           then
             "  System_args.len = argc;\n  System_args.data = argv;\n"
               ++ "#if defined _WIN32\n"
               ++ "  SetConsoleOutputCP(CP_UTF8);\n"
               ++ "#endif"
           else ""
       )
    ++ src
    ++ "}"

removeSuffix :: String -> String
removeSuffix [] = []
removeSuffix [c] = [c]
removeSuffix ('_' : '_' : _) = []
removeSuffix (c : cs) = c : removeSuffix cs
