module Infer (annotate
             ,initialTypes
             ,genConstraints
             ,assignTypes
             ,concretizeXObj
             ,concretizeDefinition
             ,manageMemory
             ,insideArrayDeleteDeps
             ,insideArrayCopyDeps
             ) where

import Control.Monad.State
import Control.Monad (replicateM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl', sort)
import Data.Maybe (mapMaybe)
import Debug.Trace

import Obj
import Constraints
import Types
import Util
import Eval
import TypeError
import InitialTypes
import GenerateConstraints

-- | Walk the whole expression tree and replace all occurences of VarTy with their corresponding actual type.
assignTypes :: TypeMappings -> XObj -> XObj
assignTypes mappings root = visit root
  where
    visit xobj =
      case obj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        _ -> assignType xobj

    visitList (XObj (Lst xobjs) i t) =
      let visited = map (assignTypes mappings) xobjs
          xobj' = XObj (Lst visited) i t
      in  assignType xobj'
    visitList _ = compilerError "The function 'visitList' only accepts XObjs with lists in them."

    visitArray (XObj (Arr xobjs) i t) =
      let visited = map (assignTypes mappings) xobjs
          xobj' = XObj (Arr visited) i t
      in  assignType xobj'
    visitArray _ = compilerError "The function 'visitArray' only accepts XObjs with arrays in them."

    assignType :: XObj -> XObj
    assignType xobj = case ty xobj of
      Just startingType -> xobj { ty = Just (replaceTyVars mappings startingType) }
      Nothing -> xobj

-- | This function performs two things:
--   1. Finds out which polymorphic functions that needs to be added to the environment for the calls in the function to work.
--   2. Changes the name of symbols at call sites so they use the polymorphic name
--   Both of these results are returned in a tuple: (<new xobj>, <dependencies>)
concretizeXObj :: Bool -> Env -> Env -> XObj -> Either TypeError (XObj, [XObj])
concretizeXObj allowAmbiguity typeEnv rootEnv root =
  case runState (visit rootEnv root) [] of
    (Left err, _) -> Left err
    (Right xobj, deps) -> Right (xobj, deps)
  where
    visit :: Env -> XObj -> State [XObj] (Either TypeError XObj)
    visit env xobj@(XObj (Sym _) _ _) = visitSymbol env xobj
    visit env xobj@(XObj (MultiSym _ _) _ _) = visitMultiSym env xobj
    visit env (XObj (Lst lst) i t) = do visited <- visitList env lst
                                        return $ do okVisited <- visited
                                                    Right (XObj (Lst okVisited) i t)
    visit env (XObj (Arr arr) i (Just t)) = do visited <- fmap sequence (mapM (visit env) arr)
                                               modify ((insideArrayDeleteDeps typeEnv env t) ++ )
                                               modify ((defineArrayTypeAlias t) : )
                                               return $ do okVisited <- visited
                                                           Right (XObj (Arr okVisited) i (Just t))
    visit _ x = return (Right x)

    visitList :: Env -> [XObj] -> State [XObj] (Either TypeError [XObj])
    visitList _ [] = return (Right [])

    visitList env (defn@(XObj Defn _ _) : nameSymbol : args@(XObj (Arr argsArr) _ _) : body : []) =
      do mapM_ checkForNeedOfTypedefs argsArr
         let functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv
             envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName)) _ _) ->
                                     extendEnv e argSymName arg)
                                  functionEnv argsArr
         visitedBody <- (visit envWithArgs) body
         return $ do okBody <- visitedBody
                     return [defn, nameSymbol, args, okBody]        
    visitList env (letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : []) =
      do visitedBindings <- fmap sequence (mapM (visit env) bindings)
         visitedBody <- (visit env) body         
         return $ do okVisitedBindings <- visitedBindings
                     okVisitedBody <- visitedBody
                     return [letExpr, XObj (Arr okVisitedBindings) bindi bindt, okVisitedBody]
    visitList env (func : args) =
      do f <- visit env func
         a <- fmap sequence (mapM (visit env) args)
         return $ do okF <- f
                     okA <- a
                     return (okF : okA)

    checkForNeedOfTypedefs :: XObj -> State [XObj] (Either TypeError ())
    checkForNeedOfTypedefs (XObj _ _ (Just t)) =
      case t of
        (FuncTy _ _) | typeIsGeneric t -> return (Right ())
                     | otherwise -> do modify (defineFunctionTypeAlias t :)
                                       return (Right ())
        _ -> return (Right ())
    checkForNeedOfTypedefs _ = error "Missing type."
    
    visitSymbol :: Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitSymbol env xobj@(XObj (Sym path) i t) =
      case lookupInEnv path env of
        Just (foundEnv, binder)
          | envIsExternal foundEnv -> 
            let theXObj = binderXObj binder
                Just theType = ty theXObj
                Just typeOfVisited = t
            in if --(trace $ "CHECKING " ++ getName xobj ++ " : " ++ show theType ++ " with visited type " ++ show t) $
                  typeIsGeneric theType && not (typeIsGeneric typeOfVisited)
                  then case concretizeDefinition allowAmbiguity typeEnv env theXObj typeOfVisited of
                         Left err -> return (Left err)
                         Right (concrete, deps) ->
                           do modify (concrete :)
                              modify (deps ++)
                              return (Right (XObj (Sym (getPath concrete)) i t))
                  else return (Right xobj)
          | otherwise -> return (Right xobj)
        Nothing -> return (Right xobj)
    visitSymbol _ _ = error "Not a symbol."

    visitMultiSym :: Env -> XObj -> State [XObj] (Either TypeError XObj)
    visitMultiSym env xobj@(XObj (MultiSym originalSymbolName paths) i t) =
      let Just actualType = t
          tys = map (typeFromPath env) paths
          tysToPathsDict = zip tys paths
      in  case filter (matchingSignature actualType) tysToPathsDict of
            [] -> return (Left (NoMatchingSignature xobj originalSymbolName actualType tysToPathsDict))
            [(theType, singlePath)] -> let Just t' = t
                                           fake1 = XObj (Sym (SymPath [] "theType")) Nothing Nothing
                                           fake2 = XObj (Sym (SymPath [] "xobjType")) Nothing Nothing
                                       in  case solve [Constraint theType t' fake1 fake2 OrdMultiSym] of
                                             Right mappings ->
                                               let replaced = replaceTyVars mappings t'
                                                   normalSymbol = XObj (Sym singlePath) i (Just replaced)
                                               in visitSymbol env --- $ (trace ("Disambiguated " ++ pretty xobj ++
                                                                  ---   " to " ++ show singlePath ++ " : " ++ show replaced))
                                                                    normalSymbol
                                             Left failure@(UnificationFailure _ _) ->
                                               return $ Left (UnificationFailed
                                                              (unificationFailure failure)
                                                              (unificationMappings failure)
                                                              [])
                                             Left (Holes holes) ->
                                               return $ Left (HolesFound holes)
            severalPaths -> if allowAmbiguity
                            then return (Right xobj)
                            else return (Left (CantDisambiguate xobj originalSymbolName actualType severalPaths))
      where matchingSignature :: Ty -> (Ty, SymPath) -> Bool
            matchingSignature tA (tB, _) = areUnifiable tA tB
            
    visitMultiSym _ _ = error "Not a multi symbol."

typeFromPath :: Env -> SymPath -> Ty
typeFromPath env p =
  case lookupInEnv p env of
    Just (e, Binder found)
      | envIsExternal e -> forceTy found
      | otherwise -> error "Local bindings shouldn't be ambiguous."
    Nothing -> error ("Couldn't find " ++ show p ++ " in env " ++ safeEnvModuleName env)

-- | Given a definition (def, defn, template, external) and
--   a concrete type (a type without any type variables)
--   this function returns a new definition with the concrete
--   types assigned, and a list of dependencies.
concretizeDefinition :: Bool -> Env -> Env -> XObj -> Ty -> Either TypeError (XObj, [XObj])
concretizeDefinition allowAmbiguity typeEnv globalEnv definition concreteType =
  let SymPath pathStrings name = getPath definition
      Just polyType = ty definition
      suffix = polymorphicSuffix polyType concreteType
      newPath = SymPath pathStrings (name ++ suffix)
  in
    case definition of
      XObj (Lst ((XObj Defn _ _) : _)) _ _ ->
        let withNewPath = setPath definition newPath
            mappings = unifySignatures polyType concreteType
            typed = assignTypes mappings withNewPath
        in  do (concrete, deps) <- concretizeXObj allowAmbiguity typeEnv globalEnv typed
               managed <- manageMemory typeEnv globalEnv concrete
               return (managed, deps)
      XObj (Lst ((XObj (Deftemplate (TemplateCreator templateCreator)) _ _) : _)) _ _ ->
        let template = templateCreator typeEnv globalEnv
        in  Right (instantiateTemplate newPath concreteType template)
      XObj (Lst ((XObj External _ _) : _ : [])) _ _ ->
        if name == "NULL"
        then Right (definition, []) -- A hack to make all versions of NULL have the same name
        else let withNewPath = setPath definition newPath
                 withNewType = withNewPath { ty = Just concreteType }
             in  Right (withNewType, [])
      err ->
        compilerError ("Can't concretize " ++ show err ++ ": " ++ pretty definition)

type MemState = Set.Set Deleter

manageMemory :: Env -> Env -> XObj -> Either TypeError XObj
manageMemory typeEnv globalEnv root =
  let (finalObj, deleteThese) = runState (visit root) (Set.fromList [])
  in  -- (trace ("Delete these: " ++ joinWithComma (map show (Set.toList deleteThese)))) $
      case finalObj of
        Left err -> Left err
        Right ok -> let newInfo = fmap (\i -> i { infoDelete = deleteThese }) (info ok)
                    in  Right $ ok { info = newInfo }
                                          
  where visit :: XObj -> State MemState (Either TypeError XObj)
        visit xobj =
          case obj xobj of
            Lst _ -> visitList xobj
            Arr _ -> visitArray xobj
            Str _ -> do manage xobj
                        return (Right xobj)
            _ -> do return (Right xobj)

        visitArray :: XObj -> State MemState (Either TypeError XObj)
        visitArray xobj@(XObj (Arr arr) _ _) =
          do mapM_ visit arr
             mapM_ unmanage arr
             _ <- manage xobj
             return (Right xobj)

        visitArray _ = error "Must visit array."

        visitList :: XObj -> State MemState (Either TypeError XObj)
        visitList xobj@(XObj (Lst lst) i t) =
          case lst of
            defn@(XObj Defn _ _) : nameSymbol@(XObj (Sym _) _ _) : args@(XObj (Arr argList) _ _) : body : [] ->
              do mapM_ manage argList
                 visitedBody <- visit body
                 unmanage body
                 return $ do okBody <- visitedBody                             
                             return (XObj (Lst (defn : nameSymbol : args : okBody : [])) i t)
            letExpr@(XObj Let _ _) : (XObj (Arr bindings) bindi bindt) : body : [] ->
              do preDeleters <- get
                 visitedBindings <- mapM visitLetBinding (pairwise bindings)
                 visitedBody <- visit body
                 unmanage body
                 postDeleters <- get
                 let diff = postDeleters Set.\\ preDeleters
                     newInfo = setDeletersOnInfo i diff
                     survivors = (postDeleters Set.\\ diff) -- Same as just pre deleters, right?!
                 put survivors
                 --trace ("LET Pre: " ++ show preDeleters ++ "\nPost: " ++ show postDeleters ++ "\nDiff: " ++ show diff ++ "\nSurvivors: " ++ show survivors)
                 manage xobj
                 return $ do okBody <- visitedBody
                             okBindings <- fmap (concatMap (\(n,x) -> [n, x])) (sequence visitedBindings)
                             return (XObj (Lst (letExpr : (XObj (Arr okBindings) bindi bindt) : okBody : [])) newInfo t)
            setbangExpr@(XObj SetBang _ _) : variable : value : [] ->
              do visitedValue <- visit value
                 unmanage value
                 let varInfo = info variable
                     correctVariable = case variable of
                                         XObj (Lst (XObj Ref _ _ : x : _)) _ _ -> x -- Peek inside the ref to get the actual variable to set
                                         x -> x
                     deleters = case createDeleter correctVariable of
                                  Just d  -> Set.fromList [d]
                                  Nothing -> Set.empty
                     newVarInfo = setDeletersOnInfo varInfo deleters
                     newVariable = variable { info = newVarInfo }
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (setbangExpr : newVariable : okValue : [])) i t)
            addressExpr@(XObj Address _ _) : value : [] ->
              do visitedValue <- visit value
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (addressExpr : okValue : [])) i t)
            theExpr@(XObj The _ _) : typeXObj : value : [] ->
              do visitedValue <- visit value
                 transferOwnership value xobj
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (theExpr : typeXObj : okValue : [])) i t)
            refExpr@(XObj Ref _ _) : value : [] ->
              do visitedValue <- visit value
                 refCheck value
                 return $ do okValue <- visitedValue
                             return (XObj (Lst (refExpr : okValue : [])) i t)
            doExpr@(XObj Do _ _) : expressions ->
              do visitedExpressions <- mapM visit expressions
                 transferOwnership (last expressions) xobj
                 return $ do okExpressions <- sequence visitedExpressions
                             return (XObj (Lst (doExpr : okExpressions)) i t)
            whileExpr@(XObj While _ _) : expr : body : [] ->
              do preDeleters <- get
                 visitedExpr <- visit expr
                 visitedBody <- visit body
                 manage body
                 postDeleters <- get
                 -- Visit an extra time to simulate repeated use
                 _ <- visit expr
                 _ <- visit body
                 let diff = postDeleters Set.\\ preDeleters
                 put (postDeleters Set.\\ diff) -- Same as just pre deleters, right?!
                 return $ do okExpr <- visitedExpr
                             okBody <- visitedBody
                             let newInfo = setDeletersOnInfo i diff
                             return (XObj (Lst (whileExpr : okExpr : okBody : [])) newInfo t)
              
            ifExpr@(XObj If _ _) : expr : ifTrue : ifFalse : [] ->
              do visitedExpr <- visit expr
                 deleters <- get
                 
                 let (visitedTrue,  stillAliveTrue)  = runState (do { v <- visit ifTrue;
                                                                      transferOwnership ifTrue xobj;
                                                                      return v
                                                                    })
                                                       deleters
                                                       
                     (visitedFalse, stillAliveFalse) = runState (do { v <- visit ifFalse;
                                                                      transferOwnership ifFalse xobj;
                                                                      return v
                                                                    })
                                                       deleters

                 let removeTrue  = stillAliveTrue
                     removeFalse = stillAliveFalse
                     deletedInTrue  = deleters Set.\\ removeTrue
                     deletedInFalse = deleters Set.\\ removeFalse
                     common = Set.intersection deletedInTrue deletedInFalse
                     delsTrue  = deletedInFalse Set.\\ common
                     delsFalse = deletedInTrue  Set.\\ common
                     stillAlive = deleters Set.\\ (Set.union deletedInTrue deletedInFalse)

                 put stillAlive
                 manage xobj
                     
                 return $ do okExpr  <- visitedExpr
                             okTrue  <- visitedTrue
                             okFalse <- visitedFalse
                             return (XObj (Lst (ifExpr : okExpr : (del okTrue delsTrue) : (del okFalse delsFalse) : [])) i t)
            f : args ->
              do _ <- visit f
                 mapM_ visitArg args
                 manage xobj
                 return (Right (XObj (Lst (f : args)) i t))

            [] -> return (Right xobj)              
        visitList _ = error "Must visit list."

        visitLetBinding :: (XObj, XObj) -> State MemState (Either TypeError (XObj, XObj))
        visitLetBinding (name, expr) =
          do visitedExpr <- visit expr
             transferOwnership expr name
             return $ do okExpr <- visitedExpr
                         return (name, okExpr)

        visitArg :: XObj -> State MemState (Either TypeError XObj)
        visitArg xobj@(XObj _ _ (Just t)) =
          if isManaged t
          then do visitedXObj <- visit xobj
                  unmanage xobj
                  return $ do okXObj <- visitedXObj
                              return okXObj
          else do --(trace ("Ignoring arg " ++ show xobj ++ " because it's not managed."))
                    (visit xobj)
        visitArg xobj@(XObj _ _ _) =
          visit xobj

        createDeleter :: XObj -> Maybe Deleter
        createDeleter xobj =
          case ty xobj of
            Just t -> let var = varOfXObj xobj
                      in  if isManaged t && not (isExternalType typeEnv t)
                          then case nameOfPolymorphicFunction globalEnv t "delete" of
                                 Just pathOfDeleteFunc -> Just (ProperDeleter pathOfDeleteFunc var)
                                 Nothing -> --trace ("Found no delete function for " ++ var ++ " : " ++ (showMaybeTy (ty xobj)))
                                            Just (FakeDeleter var)
                          else Nothing
            Nothing -> error ("No type, can't manage " ++ show xobj)
          
        manage :: XObj -> State MemState ()
        manage xobj =
          case createDeleter xobj of
            Just deleter -> modify (Set.insert deleter)
            Nothing -> return ()

        deletersMatchingXObj :: XObj -> Set.Set Deleter -> [Deleter]
        deletersMatchingXObj xobj deleters =
          let var = varOfXObj xobj
          in  Set.toList $ Set.filter (\d -> case d of
                                               ProperDeleter { deleterVariable = dv } -> dv == var
                                               FakeDeleter   { deleterVariable = dv } -> dv == var)
                                      deleters

        unmanage :: XObj -> State MemState ()
        unmanage xobj =
          let Just t = ty xobj 
              Just i = info xobj
          in if isManaged t && not (isExternalType typeEnv t)
             then do deleters <- get
                     case deletersMatchingXObj xobj deleters of
                       [] -> trace ("Trying to use '" ++ getName xobj ++ "' (expression " ++ freshVar i ++ ") at " ++ prettyInfoFromXObj xobj ++
                                    " but it has already been given away.") (return ())
                       [one] -> let newDeleters = Set.delete one deleters
                                in  put newDeleters
                       _ -> error "Too many variables with the same name in set."                                  
             else return ()

        -- | Check that the value being referenced hasn't already been given away
        refCheck :: XObj -> State MemState ()
        refCheck xobj =
          let Just i = info xobj
              Just t = ty xobj
          in if isManaged t && not (isExternalType typeEnv t)
             then do deleters <- get
                     case deletersMatchingXObj xobj deleters of
                       [] -> trace ("Trying to get reference from '" ++ getName xobj ++
                                    "' (expression " ++ freshVar i ++ ") at " ++ prettyInfoFromXObj xobj ++
                                    " but it has already been given away.") (return ())
                       [_] -> return ()
                       _ -> error "Too many variables with the same name in set."                                  
             else return ()

        transferOwnership :: XObj -> XObj -> State MemState ()
        transferOwnership from to =
          do unmanage from
             manage to
             --trace ("Transfered from " ++ getName from ++ " '" ++ varOfXObj from ++ "' to " ++ getName to ++ " '" ++ varOfXObj to ++ "'") $ return ()

        varOfXObj :: XObj -> String
        varOfXObj xobj =
          case xobj of
            XObj (Sym (SymPath [] name)) _ _ -> name
            _ -> let Just i = info xobj
                 in  freshVar i

setDeletersOnInfo :: Maybe Info -> Set.Set Deleter -> Maybe Info
setDeletersOnInfo i deleters = fmap (\i' -> i' { infoDelete = deleters }) i

del :: XObj -> Set.Set Deleter -> XObj
del xobj deleters = xobj { info = (setDeletersOnInfo (info xobj) deleters) }

isExternalType :: Env -> Ty -> Bool
isExternalType typeEnv (StructTy name _) =
  case lookupInEnv (SymPath [] name) typeEnv of
    Just (_, Binder (XObj (Lst (XObj ExternalType _ _ : _)) _ _)) -> True
    Just _ -> False
    Nothing -> False
isExternalType _ _ =
  False

nameOfPolymorphicFunction :: Env -> Ty -> String -> Maybe SymPath
nameOfPolymorphicFunction env t lookupName
  | isManaged t =
    case filter ((\(Just t') -> areUnifiable (FuncTy [t] UnitTy) t') . ty . binderXObj . snd) (multiLookupALL lookupName env) of
      [] -> Nothing
      [(_, Binder single)] ->
        let Just t' = ty single
            (SymPath pathStrings name) = getPath single
            suffix = polymorphicSuffix t' (FuncTy [t] UnitTy)
            concretizedPath = SymPath pathStrings (name ++ suffix)
        in  Just concretizedPath
      _ -> Nothing
  | otherwise   = Nothing

-- depsOfPolymorphicFunction :: Env -> Ty -> String -> [XObj]
-- depsOfPolymorphicFunction env t name
--   | isManaged t =
--     case filter ((\(Just t') -> (areUnifiable (FuncTy [t] UnitTy) t')) . ty . binderXObj . snd) (multiLookupALL name env) of
--       [] -> (trace $ "No dependency found for " ++ show t) []
--       [(_, Binder (XObj (Lst ((XObj (Instantiate _) _ _) : _)) _ _))] ->
--         []
--       [(_, Binder single)] ->
--         case concretizeDefinition False env single (FuncTy [t] (UnitTy)) of
--           Left err -> error (show err)
--           Right (ok, deps) -> (ok : deps)
--       _ -> (trace $ "Too many dependencies found for " ++ show t) []
--   | otherwise = []

insideArrayDeleteDeps :: Env -> Env -> Ty -> [XObj]
insideArrayDeleteDeps typeEnv env t
  | isManaged t =
    case filter ((\(Just t') -> (areUnifiable (FuncTy [t] UnitTy) t')) . ty . binderXObj . snd) (multiLookupALL "delete" env) of
      [] -> --(trace $ "No 'delete' function found for " ++ show t)
        []
      [(_, Binder (XObj (Lst ((XObj (Instantiate _) _ _) : _)) _ _))] ->
        []
      [(_, Binder single)] ->
        case concretizeDefinition False typeEnv env single (FuncTy [t] (UnitTy)) of
          Left err -> error (show err)
          Right (ok, deps) -> (ok : deps)
      _ -> (trace $ "Too many 'delete' functions found for " ++ show t) []
  | otherwise = []

-- TODO: merge with "insideArrayDeleteDeps"
insideArrayCopyDeps :: Env -> Env -> Ty -> [XObj]
insideArrayCopyDeps typeEnv env t
  | isManaged t =
    case filter ((\(Just t') -> (areUnifiable (FuncTy [(RefTy t)] t) t')) . ty . binderXObj . snd) (multiLookupALL "copy" env) of
      [] -> --(trace $ "No 'copy' function found for " ++ show t)
        []
      [(_, Binder (XObj (Lst ((XObj (Instantiate _) _ _) : _)) _ _))] ->
        []
      [(_, Binder single)] ->
        case concretizeDefinition False typeEnv env single (FuncTy [(RefTy t)] t) of
          Left err -> error (show err)
          Right (ok, deps) -> (ok : deps)
      _ -> (trace $ "Too many 'copy' functions found for " ++ show t) []
  | otherwise = []
                
-- | Convert from the type 'UnificationFailure' to 'TypeError' (enables monadic chaining of Either).
solveConstraintsAndConvertErrorIfNeeded :: [Constraint] -> Either TypeError TypeMappings
solveConstraintsAndConvertErrorIfNeeded constraints =
  case solve constraints of
    Left failure@(UnificationFailure _ _) -> Left (UnificationFailed
                                                   (unificationFailure failure)
                                                   (unificationMappings failure)
                                                   constraints)
    Left (Holes holes) -> Left (HolesFound holes)
    Right ok -> Right ok

check :: XObj -> Either TypeError ()
check xobj@(XObj (Lst (XObj Defn _ _ : _)) _ t) =
  case t of
    Just (FuncTy _ (RefTy _)) -> Left (CantReturnRefTy xobj)
    Just _ -> return ()
    Nothing -> Left (DefnMissingType xobj)
check _ = return ()

annotateOne :: Env -> Env -> XObj -> Bool -> Either TypeError (XObj, [XObj])
annotateOne typeEnv env xobj allowAmbiguity = do
  constraints <- genConstraints xobj
  mappings <- solveConstraintsAndConvertErrorIfNeeded constraints
  let typed = assignTypes mappings xobj
  concretizeXObj allowAmbiguity typeEnv env typed
  
-- | Performs all the steps of creating initial types, solving constraints and assigning the types.
-- | Returns a list of all the bindings that need to be added for the new form to work.
-- | The concretization of MultiSym:s (= ambiguous use of symbols, resolved by type usage)
-- | makes it possible to solve more types so let's do it several times.  
annotate :: Env -> Env -> XObj -> Either TypeError [XObj]
annotate typeEnv globalEnv xobj =
  do initiated <- initialTypes globalEnv xobj
     (annotated, dependencies) <- foldM (\(x, deps) allowAmbiguity ->
                                           do (x', deps') <- annotateOne typeEnv globalEnv x allowAmbiguity
                                              return (x', deps ++ deps'))
                                  (initiated, [])
                                  [True, False]
     final <- manageMemory typeEnv globalEnv annotated
     check final
     _ <- mapM check dependencies
     return (final : dependencies)
