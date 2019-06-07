{-# LANGUAGE OverloadedStrings #-}

module RenderDocs where

import CMark
import Control.Monad (when)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Internal (stringValue)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding as E
import Data.Text as Text
import System.Directory
import qualified Data.Map as Map
import Debug.Trace

import Obj
import Types
import Util

saveDocsForEnvs :: Project -> [(SymPath, Binder)] -> IO ()
saveDocsForEnvs ctx pathsAndEnvBinders =
  let dir = projectDocsDir ctx
      title = projectTitle ctx
      generateIndex = projectDocsGenerateIndex ctx
      allEnvNames = fmap (getModuleName . fst . getEnvAndMetaFromBinder . snd) pathsAndEnvBinders
  in  do mapM_ (saveDocsForEnvBinder ctx allEnvNames) pathsAndEnvBinders
         when generateIndex (writeFile (dir ++ "/" ++ title ++ "_index.html")
                                       (projectIndexPage ctx allEnvNames))
         putStrLn ("Generated docs to '" ++ dir ++ "'")

-- | This function expects a binder that contains an environment, anything else is a runtime error.
getEnvAndMetaFromBinder :: Binder -> (Env, MetaData)
getEnvAndMetaFromBinder envBinder =
  case envBinder of
    Binder meta (XObj (Mod env) _ _) -> (env, meta)
    _ -> error "Binder's not a module. This should be detected in 'commandSaveDocsInternal'."

projectIndexPage :: Project -> [String] -> String
projectIndexPage ctx moduleNames =
  let logo = projectDocsLogo ctx
      url = projectDocsURL ctx
      css = projectDocsStyling ctx
      htmlHeader = toHtml $ projectTitle ctx
      htmlDoc = commonmarkToHtml [optSafe] $ Text.pack $ projectDocsPrelude ctx
      html = renderHtml $ docTypeHtml $
                          do headOfPage css
                             H.body $
                               H.div ! A.class_ "content" $
                                 H.a ! A.href (H.stringValue url) $
                                   do H.div ! A.class_ "logo" $
                                        do H.img ! A.src (H.stringValue logo) ! A.alt "Logo"
                                           moduleIndex moduleNames
                                      H.div $
                                        do H.h1 htmlHeader
                                           preEscapedToHtml htmlDoc
  in html

headOfPage :: String -> H.Html
headOfPage css =
  H.head $
    do H.meta ! A.charset "UTF-8"
       H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
       H.link ! A.rel "stylesheet" ! A.href (H.stringValue css)

getModuleName :: Env -> String
getModuleName env = fromMaybe "Global" (envModuleName env)

saveDocsForEnvBinder :: Project -> [String] -> (SymPath, Binder) -> IO ()
saveDocsForEnvBinder ctx moduleNames (pathToEnv, envBinder) =
  do let SymPath _ moduleName = pathToEnv
         dir = projectDocsDir ctx
         fullPath = dir ++ "/" ++ moduleName ++ ".html"
         string = renderHtml (envBinderToHtml envBinder ctx (show pathToEnv) moduleNames)
     createDirectoryIfMissing False dir
     writeFile fullPath string

envBinderToHtml :: Binder -> Project -> String -> [String] -> H.Html
envBinderToHtml envBinder ctx moduleName moduleNames =
  let (env, meta) = getEnvAndMetaFromBinder envBinder
      title = projectTitle ctx
      css = projectDocsStyling ctx
      moduleDescription = case Map.lookup "doc" (getMeta meta) of
                            Just (XObj (Str s) _ _) -> s
                            Nothing -> ""
      moduleDescriptionHtml = commonmarkToHtml [optSafe] $ Text.pack moduleDescription
  in H.docTypeHtml $
           do headOfPage css
              H.body $
                H.div ! A.class_ "content" $
                  do H.div ! A.class_ "logo" $
                       do H.a ! A.href "http://github.com/carp-lang/Carp" $
                            H.img ! A.src "logo.png"
                          --span_ "CARP DOCS FOR"
                          H.div  ! A.class_ "title" $ toHtml title
                          moduleIndex moduleNames
                     H.h1 (toHtml moduleName)
                     H.div ! A.class_ "module-description" $ preEscapedToHtml moduleDescriptionHtml
                     mapM_ (binderToHtml . snd) (Prelude.filter shouldEmitDocsForBinder (Map.toList (envBindings env)))

shouldEmitDocsForBinder :: (String, Binder) -> Bool
shouldEmitDocsForBinder (name, Binder meta xobj) =
  not (metaIsTrue meta "hidden")

moduleIndex :: [String] -> H.Html
moduleIndex moduleNames =
  H.div ! A.class_ "index" $
    H.ul $ mapM_ moduleLink moduleNames

moduleLink :: String -> H.Html
moduleLink name =
  H.li $ H.a ! A.href (stringValue (name ++ ".html")) $ toHtml name


binderToHtml :: Binder -> H.Html
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
      htmlDoc = commonmarkToHtml [optSafe] $ Text.pack docString
  in  H.div ! A.class_ "binder" $
        do H.a ! A.class_ "anchor" ! A.href (stringValue ("#" ++ name)) $
             H.h3 ! A.id (stringValue name) $ toHtml name
           H.div ! A.class_ "description" $ toHtml description
           H.p  ! A.class_ "sig" $ toHtml typeSignature
           case maybeNameAndArgs of
             Just nameAndArgs -> H.pre ! A.class_ "args" $ toHtml nameAndArgs
             Nothing -> H.span $ toHtml (""::String)
           H.p ! A.class_ "doc" $ preEscapedToHtml htmlDoc
           --p_ (toHtml (description))
