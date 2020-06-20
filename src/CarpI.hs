-- | Module CarpI defines functions to manipulate Carp interface files
module CarpI where

import Data.Map (toList)
import Data.Set (fromList)
import Data.List
import Debug.Trace
import Text.Parsec ((<|>), try)
import qualified Text.Parsec as Parsec

import Types
import Info
import Parsing
import Lookup
import Obj

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
      "Defn " ++ show path ++ " " ++ show t ++ "\n"
    [XObj Def _ _, XObj (Sym path _) _ _, _] ->
      "Def " ++ show path ++ " " ++ show t ++ "\n"
    XObj (Deftype t) _ _ : XObj (Sym path _) _ _ : rest ->
      "" -- TODO
    XObj (DefSumtype t) _ _ : XObj (Sym path _) _ _ : rest ->
      "" -- TODO
    XObj Macro _ _ : _ ->
      "" -- TODO how to treat macros ? (and dynamics ?)
    XObj DefDynamic _ _ : _ ->
      "" -- TODO
    _ -> "" -- TODO missing some other constructs ?
toInterface bind@(Binder _ xobj) = ""

-- | Parsing an interface

typ :: XObj -> Parsec.Parsec String ParseState Types.Ty
typ ty = case (xobjToTy ty) of
           Nothing ->
             fail ("Error while parsing " ++ (pretty ty) ++ " as ty")
           Just ty -> return ty

defn :: Parsec.Parsec String ParseState XObj
defn = do _ <- Parsec.string "Defn"
          _ <- whitespace
          path <- symbol
          _ <- whitespace
          tyObj <- sexpr
          ty <- typ tyObj
          return (XObj (Lst [XObj (External Nothing) Nothing (Just ty),
                             path { ty = Just ty }
                             -- XObj (Arr []) Nothing Nothing,
                             -- XObj (Bol False) Nothing Nothing
                            ]) (Just dummyInfo) (Just ty)) -- TODO

def :: Parsec.Parsec String ParseState XObj
def = do _ <- Parsec.string "Def"
         _ <- whitespace
         path <- symbol
         _ <- whitespace
         tyObj <- sexpr
         ty <- typ tyObj
         return (XObj (Lst [XObj (External Nothing) Nothing (Just ty),
                            path { ty = Just ty }-- ,
                            -- XObj (Bol False) Nothing Nothing
                           ]) (Just dummyInfo) (Just ty)) -- TODO

line :: Parsec.Parsec String ParseState XObj
line = (try defn) <|>
       (try def)

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
          typeEnv = getTypeEnv (contextTypeEnv ctx)
      in Right ctx { contextGlobalEnv = multipleExtendEnv globalEnv decls,
                     contextTypeEnv = TypeEnv (multipleExtendEnv typeEnv tydecls) }
    Left err -> Left err
  where isTypeDecl _ = False -- TODO
        multipleExtendEnv env objs =
          foldl' (\env obj -> envInsertAndAddModulesAt env (getPath obj) (Binder emptyMeta obj)) env objs
