module Infer (annotate
             ,initialTypes
             ,genConstraints
             ,assignTypes
             ,concretizeXObj
             ,concretizeDefinition
             ,manageMemory
             ,depsOfPolymorphicFunction
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
import AssignTypes
import GenerateConstraints
import ManageMemory

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

-- | Get the type of a symbol at a given path.
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

-- | Find all the dependencies of a polymorphic function with a name and a desired concrete type.
depsOfPolymorphicFunction :: Env -> Env -> String -> Ty -> [XObj]
depsOfPolymorphicFunction typeEnv env functionName functionType =
  case filter ((\(Just t') -> (areUnifiable functionType t')) . ty . binderXObj . snd) (multiLookupALL functionName env) of
    [] -> (trace $ "No '" ++ functionName ++ "' function found for type " ++ show functionType ++ ".") []
    [(_, Binder (XObj (Lst ((XObj (Instantiate _) _ _) : _)) _ _))] ->
      []
    [(_, Binder single)] ->
      case concretizeDefinition False typeEnv env single functionType of
        Left err -> error (show err)
        Right (ok, deps) -> (ok : deps)
    _ -> (trace $ "Too many '" ++ functionName ++ "' functions found, can't figure out dependencies.") []

-- | TODO: Can this use the 'depsOfPolymorphicFunction' too?!
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

-- | TODO: merge with "insideArrayDeleteDeps" etc.
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

-- | Will make sure that a form doesn't return a reference.
-- | TODO: This check is needed on nested forms like 'let' statements, etc.
check :: XObj -> Either TypeError ()
check xobj@(XObj (Lst (XObj Defn _ _ : _)) _ t) =
  case t of
    Just (FuncTy _ (RefTy _)) -> Left (CantReturnRefTy xobj)
    Just _ -> return ()
    Nothing -> Left (DefnMissingType xobj)
check _ = return ()

-- | Performs ONE step of annotation. The 'annotate' function will call this function several times.
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
