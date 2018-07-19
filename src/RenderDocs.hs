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
  in  do mapM_ (saveDocsForEnv dirPath projectTitle allEnvNames) envs
         writeFile (dirPath ++ "/" ++ projectTitle ++ "_index.html") (projectIndexPage projectTitle allEnvNames)
         putStrLn ("Generated docs to '" ++ dirPath ++ "'")

projectIndexPage :: String -> [String] -> String
projectIndexPage projectTitle moduleNames =
  let html = renderText $ do headOfPage
                             body_ $
                               do div_ [class_ "content"] $
                                    do a_ [href_ "http://github.com/carp-lang/Carp"] $
                                         do div_ [class_ "logo"] $
                                              do img_ [src_ "logo.png"]
                                                 moduleIndex moduleNames
                                            h1_ [class_ "huge"] (toHtml projectTitle)
  in  T.unpack html

headOfPage :: Html ()
headOfPage =
  head_ $
    do meta_ [charset_ "UTF-8"]
       meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"]
       link_ [rel_ "stylesheet", href_ "carp_style.css"]

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
   html_ $ do headOfPage
              body_ $
                do div_ [class_ "content"] $
                     do div_ [class_ "logo"] $
                          do a_ [href_ "http://github.com/carp-lang/Carp"] $
                               do img_ [src_ "logo.png"]
                             --span_ "CARP DOCS FOR"
                             div_ [class_ "title"] (toHtml projectTitle)
                             moduleIndex moduleNames
                        h1_ (toHtml moduleName)
                        mapM_ (binderToHtml . snd) (Prelude.filter shouldEmitDocsForBinder (Map.toList (envBindings env)))

shouldEmitDocsForBinder :: (String, Binder) -> Bool
shouldEmitDocsForBinder (name, Binder meta xobj) =
  not (metaIsTrue meta "hidden")

moduleIndex :: [String] -> Html ()
moduleIndex moduleNames =
  do div_ [class_ "index"] $
       ul_ $ do mapM_ moduleLink moduleNames

moduleLink :: String -> Html ()
moduleLink name =
  li_ $ a_ [href_ (Text.pack (name ++ ".html"))] (toHtml name)

binderToHtml :: Binder -> Html ()
binderToHtml (Binder meta xobj) =
  let name = getSimpleName xobj
      maybeNameAndArgs = getSimpleNameWithArgs xobj
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
              case maybeNameAndArgs of
                Just nameAndArgs -> pre_ [class_ "args"] (toHtml nameAndArgs)
                Nothing -> span_ [] (toHtml (""::String))
              p_ [class_ "doc"] (toHtml docString)
              --p_ (toHtml (description))
