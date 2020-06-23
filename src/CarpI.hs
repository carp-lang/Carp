-- | Module CarpI defines functions to manipulate Carp interface files
module CarpI where

import Data.Map (toList)
import Data.Set (fromList)
import Data.List
import Debug.Trace
import Text.Parsec ((<|>), try)
import qualified Text.Parsec as Parsec

import Util (joinWith)
import qualified Meta
import Project
import Types
import Info
import Parsing
import Lookup
import Obj
import Deftype

-- | Generating an interface

envToInterface :: Env -> String
envToInterface env =
  let interfaces = map (\(_, binder) -> toInterface binder)
                   (toList (envBindings env))
  in concat interfaces

toInterface :: Binder -> String
toInterface (Binder _ xobj@(XObj (Mod env) _ _)) =
  envToInterface env
toInterface (Binder meta xobj@(XObj (Lst xobjs) _ (Just t))) =
  case xobjs of
    [XObj (Defn _) _ _, XObj (Sym path _) _ _, _, _] ->
      "Ext " ++ show path ++ " " ++ show t ++ "\n"
    [XObj Def _ _, XObj (Sym path _) _ _, _] ->
      "Ext " ++ show path ++ " " ++ show t ++ "\n"
    [XObj (External _) _ _, XObj (Sym path _) _ _] ->
      "Ext " ++ show path ++ " " ++ show t ++ "\n"
    [XObj (Deftype t) _ _, XObj (Sym path _) _ _, rest] ->
      "ExtTy " ++ show path ++ " " ++ pretty rest ++ "\n"
    XObj (DefSumtype t) _ _ : XObj (Sym path _) _ _ : rest ->
      "ExtTy " ++ show path ++ "\n" ++ toConstructors path rest
    [XObj (ExternalType _) _ _, XObj (Sym path _) _ _] ->
      "ExtTy " ++ show path ++ "\n"
    [XObj (ExternalType _) _ _, XObj (Sym path _) _ _, fields] ->
      "ExtTy " ++ show path ++ " " ++ pretty fields ++ "\n"
    XObj Macro _ _ : _ ->
      "" -- TODO how to treat macros ? (and dynamics ?)
    XObj DefDynamic _ _ : _ ->
      "" -- TODO
    _ -> "" -- TODO missing some other constructs ?
  where toConstructors path fields = joinWith "" ((map (toConstructor path)) fields)
        toConstructor ty@(SymPath paths path1) (XObj (Sym path2 _) _ _) =
          "Ext " ++ show (consPath (paths++[path1]) path2) ++ " "
          ++ show (FuncTy [] (ConcreteNameTy (show ty)) StaticLifetimeTy) ++ "\n"
        toConstructor ty@(SymPath paths path1)
          (XObj (Lst [XObj (Sym path2 _) _ _, XObj (Arr tys) _ _]) _ _) =
          "Ext " ++ show (consPath (paths++[path1]) path2) ++ " "
          ++ show (FuncTy (mapfilter xobjToTy tys) (ConcreteNameTy (show ty)) StaticLifetimeTy) ++ "\n"
        mapfilter f [] = []
        mapfilter f (hd:tl) =
          case f hd of
            Just hd -> hd:mapfilter f tl
            Nothing -> mapfilter f tl

toInterface bind@(Binder _ xobj) = ""

-- | Parsing an interface

typ :: Parsec.Parsec String ParseState Types.Ty
typ = do ty <- sexpr
         case (xobjToTy ty) of
           Nothing ->
             fail ("Error while parsing " ++ (pretty ty) ++ " as ty")
           Just ty -> return ty

ext :: Parsec.Parsec String ParseState XObj
ext = do _ <- Parsec.string "Ext"
         _ <- whitespace
         path <- symbol
         _ <- whitespace
         ty <- typ
         return (XObj (Lst [XObj (External Nothing) Nothing (Just ty),
                            path { ty = Just ty }
                           ]) (Just dummyInfo) (Just ty))

extTy :: Parsec.Parsec String ParseState XObj
extTy = do _ <- Parsec.string "ExtTy"
           _ <- whitespace
           path <- symbol
           return (XObj (Lst [XObj (ExternalType Nothing) Nothing Nothing,
                              path
                             ]) (Just dummyInfo) (Just TypeTy))

extTyFields :: Parsec.Parsec String ParseState XObj
extTyFields = do _ <- Parsec.string "ExtTy"
                 _ <- whitespace
                 path <- symbol
                 _ <- whitespace
                 fields <- array
                 return (XObj (Lst [XObj (ExternalType Nothing) Nothing Nothing,
                                    path,
                                    fields
                                   ]) (Just dummyInfo) (Just TypeTy))

line :: Parsec.Parsec String ParseState XObj
line = (try ext) <|> (try extTyFields) <|> (try extTy)

file :: Parsec.Parsec String ParseState [XObj]
file = do padding <- Parsec.many whitespace
          result <- Parsec.sepBy line (Parsec.many whitespace)
          Parsec.eof
          return result

parseInterface :: String -> String -> Either Parsec.ParseError [XObj]
parseInterface text filename =
  let initState = ParseState (Info 1 1 filename (fromList []) 0)
  in Parsec.runParser file initState filename text

parseAndLoadInterface :: Context -> String -> String -> Either Parsec.ParseError Context
parseAndLoadInterface ctx text filename =
  case (parseInterface text filename) of
    Right objs ->
      let tydecls = filter isTypeDecl objs
          decls = filter (not . isTypeDecl) objs
          globalEnv = contextGlobalEnv ctx
          newCtx = ctx { contextGlobalEnv = multipleEnvInsertAt globalEnv decls }
      in Right (multipleRegisterTypes newCtx tydecls)
    Left err -> Left err
  where isTypeDecl xobj@(XObj (Lst (XObj (ExternalType _) _ _:_)) _ _) = True
        isTypeDecl _ = False
        getPathName obj = case (getPath obj) of
                            SymPath _ s -> s
        multipleRegisterTypes =
          foldl' registerType
        registerType ctx xobj@(XObj (Lst [XObj (ExternalType _) _ _, XObj (Sym (SymPath _ s) _) _ _]) _ _) =
          registerTypeWithoutFields ctx s Nothing
        registerType ctx xobj@(XObj (Lst [XObj (ExternalType _) _ _, XObj (Sym (SymPath _ s) _) _ _, members]) _ _) =
          registerTypeWithFields ctx xobj s Nothing members
        multipleEnvInsertAt =
          foldl' (\env obj -> envInsertAndAddModulesAt env (getPath obj) (Binder emptyMeta obj))

-- TODO avoid code duplication between this and primitive
registerTypeWithoutFields :: Context -> String -> (Maybe String) -> Context
registerTypeWithoutFields ctx t override = do
  let pathStrings = contextPath ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
  ctx { contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition) }

registerTypeWithFields :: Context -> XObj -> String -> (Maybe String) -> XObj -> Context
registerTypeWithFields ctx x t override members =
  case (bindingsForRegisteredType typeEnv globalEnv pathStrings t [members] Nothing preExistingModule) of
    Left _ -> ctx
    Right bindings@(_, o, _) -> updateContext bindings
  where updateContext (typeModuleName, typeModuleXObj, deps) =
          do let typeDefinition = XObj (Lst [XObj (ExternalType override) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
                 ctx' = (ctx { contextGlobalEnv = envInsertAt globalEnv (SymPath pathStrings typeModuleName) (Binder emptyMeta typeModuleXObj)
                             , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition)
                             })
             foldl (define True) ctx' deps
        pathStrings = contextPath ctx
        globalEnv = contextGlobalEnv ctx
        typeEnv = contextTypeEnv ctx
        path = SymPath pathStrings t
        preExistingModule = case lookupInEnv (SymPath pathStrings t) globalEnv of
                              Just (_, Binder _ (XObj (Mod found) _ _)) -> Just found
                              _ -> Nothing

define :: Bool -> Context -> XObj -> Context
define hidden ctx@(Context globalEnv _ _ _ _ _ _ _) annXObj =
  ctx {contextGlobalEnv = envInsertAt globalEnv (getPath annXObj) (Binder emptyMeta annXObj)}
