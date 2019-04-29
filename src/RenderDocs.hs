{-# LANGUAGE OverloadedStrings #-}

module RenderDocs where

import CMark
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Internal (stringValue)
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding as E
import Data.Text as Text
import System.Directory
import qualified Data.Map as Map
import Debug.Trace

import Obj
import Types
import Util

saveDocsForEnvs :: Project -> [(SymPath, Env)] -> IO ()
saveDocsForEnvs ctx envs =
  let dir = projectDocsDir ctx
      title = projectTitle ctx
      generateIndex = projectDocsGenerateIndex ctx
      allEnvNames = (fmap (getModuleName . snd) envs)
  in  do mapM_ (saveDocsForEnv ctx allEnvNames) envs
         if generateIndex
         then writeFile (dir ++ "/" ++ title ++ "_index.html")
                        (projectIndexPage ctx allEnvNames)
         else return ()
         putStrLn ("Generated docs to '" ++ dir ++ "'")


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
                               do H.div ! A.class_ "content" $
                                    do H.a ! A.href (H.stringValue url) $
                                         do H.div ! A.class_ "logo" $
                                              do H.img ! A.src (H.stringValue logo) ! A.alt "Logo"
                                                 moduleIndex moduleNames
                                            H.div $
                                              do H.h1 htmlHeader
                                                 (preEscapedToHtml htmlDoc)
  in html

headOfPage :: String -> H.Html
headOfPage css =
  H.head $
    do H.meta ! A.charset "UTF-8"
       H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
       H.link ! A.rel "stylesheet" ! A.href (H.stringValue css)

getModuleName :: Env -> String
getModuleName env =
  case envModuleName env of
    Just hasName -> hasName
    Nothing -> "Global"

saveDocsForEnv :: Project -> [String] -> (SymPath, Env) -> IO ()
saveDocsForEnv ctx moduleNames (pathToEnv, env) =
  do let SymPath _ moduleName = pathToEnv
         dir = projectDocsDir ctx
         fullPath = dir ++ "/" ++ moduleName ++ ".html"
         string = renderHtml (envToHtml env ctx (show pathToEnv) moduleNames)
     createDirectoryIfMissing False dir
     writeFile fullPath string

envToHtml :: Env -> Project -> String -> [String] -> H.Html
envToHtml env ctx moduleName moduleNames =
  let title = projectTitle ctx
      css = projectDocsStyling ctx
  in H.docTypeHtml $
           do headOfPage css
              H.body $
                do H.div ! A.class_ "content" $
                     do H.div ! A.class_ "logo" $
                          do H.a ! A.href "http://github.com/carp-lang/Carp" $
                               do H.img ! A.src "logo.png"
                             --span_ "CARP DOCS FOR"
                             H.div  ! A.class_ "title" $ (toHtml title)
                             moduleIndex moduleNames
                        H.h1 (toHtml moduleName)
                        mapM_ (binderToHtml . snd) (Prelude.filter shouldEmitDocsForBinder (Map.toList (envBindings env)))

shouldEmitDocsForBinder :: (String, Binder) -> Bool
shouldEmitDocsForBinder (name, Binder meta xobj) =
  not (metaIsTrue meta "hidden")

moduleIndex :: [String] -> H.Html
moduleIndex moduleNames =
  do H.div ! A.class_ "index" $
       H.ul $ do mapM_ moduleLink moduleNames

moduleLink :: String -> H.Html
moduleLink name =
  H.li $ H.a ! A.href (stringValue (name ++ ".html")) $ (toHtml name)


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
  in  do H.div ! A.class_ "binder" $
           do H.a ! A.class_ "anchor" ! A.href (stringValue ("#" ++ name)) $
                do H.h3 ! A.id (stringValue name) $ (toHtml name)
              H.div ! A.class_ "description" $ (toHtml description)
              H.p  ! A.class_ "sig" $ (toHtml typeSignature)
              case maybeNameAndArgs of
                Just nameAndArgs -> H.pre ! A.class_ "args" $ toHtml nameAndArgs
                Nothing -> H.span $ toHtml (""::String)
              H.p ! A.class_ "doc" $ (preEscapedToHtml htmlDoc)
              --p_ (toHtml (description))
