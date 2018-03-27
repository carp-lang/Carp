{-# LANGUAGE OverloadedStrings #-}

module RenderDocs where

import Lucid
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding as E
import Data.Text as Text
import System.Directory
import qualified Data.Map as Map
import Debug.Trace

import Obj
import Types
import Util

saveDocsForEnvs :: FilePath -> String -> [(SymPath, Env)] -> IO ()
saveDocsForEnvs dirPath projectTitle envs =
  let allEnvNames = (fmap (getModuleName . snd) envs)
  in  mapM_ (saveDocsForEnv dirPath projectTitle allEnvNames) envs

getModuleName :: Env -> String
getModuleName env =
  case envModuleName env of
    Just hasName -> hasName
    Nothing -> "Global"

saveDocsForEnv :: FilePath -> String -> [String] -> (SymPath, Env) -> IO ()
saveDocsForEnv dirPath projectTitle moduleNames (pathToEnv, env) =
  do let string = T.unpack text
         SymPath _ moduleName = pathToEnv
         fullPath = dirPath ++ "/" ++ moduleName ++ ".html"
         text = renderText (envToHtml env projectTitle (show pathToEnv) moduleNames)
     createDirectoryIfMissing False dirPath
     writeFile fullPath string

envToHtml :: Env -> String -> String -> [String] -> Html ()
envToHtml env projectTitle moduleName moduleNames =
   html_ $ do head_ $
                do meta_ [charset_ "UTF-8"]
                   link_ [rel_ "stylesheet", href_ "carp_style.css"]
              body_ $
                do div_ [class_ "content"] $
                     do div_ [class_ "logo"] $
                          do a_ [href_ "http://github.com/carp-lang/Carp"] $
                               do img_ [src_ "logo2.png"]
                             --span_ "CARP DOCS FOR"
                             div_ [class_ "title"] (toHtml projectTitle)
                             div_ [class_ "index"] $
                               ul_ $ do mapM_ moduleLink moduleNames
                        h1_ (toHtml moduleName)
                        mapM_ (binderToHtml . snd) (Map.toList (envBindings env))

moduleLink :: String -> Html ()
moduleLink name =
  li_ $ a_ [href_ (Text.pack (name ++ ".html"))] (toHtml name)

binderToHtml :: Binder -> Html ()
binderToHtml (Binder meta xobj) =
  let SymPath _ name = getPath xobj
      description = getBinderDescription xobj
      typeSignature = case ty xobj of
                 Just t -> show t
                 Nothing -> ""
      metaMap = getMeta meta
      docString = case Map.lookup "doc" metaMap of
                    Just (XObj (Str s) _ _) -> s
                    Just found -> pretty found
                    Nothing -> ""
  in  do div_ [class_ "binder"] $
           do a_ [class_ "anchor", href_ (Text.pack ("#" ++ name))] $
                do h3_ [id_ (Text.pack name)] (toHtml name)
              div_ [class_ "description"] (toHtml description)
              p_ [class_ "sig"] (toHtml typeSignature)
              p_ [class_ "doc"] (toHtml docString)
              --p_ (toHtml (description))
