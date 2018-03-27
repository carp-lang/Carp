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

saveDocsForEnv :: Env -> FilePath -> IO ()
saveDocsForEnv env dirPath =
  do let string = T.unpack text
         name = case envModuleName env of
                  Just hasName -> hasName
                  Nothing -> "global"
         fullPath = dirPath ++ "/" ++ name ++ ".html"
         text = renderText (envToHtml env name)
     createDirectoryIfMissing False dirPath
     writeFile fullPath string

envToHtml :: Env -> String -> Html ()
envToHtml env name =
   html_ $ do head_ $
                do meta_ [charset_ "UTF-8"]
                   link_ [rel_ "stylesheet", href_ "carp_style.css"]
              body_ $
                do div_ [class_ "content"] $
                     do div_ [class_ "logo"] $
                          do a_ [href_ "http://github.com/carp-lang/Carp"] $
                               do img_ [src_ "logo2.png"]
                             span_ "CARP DOCS"
                        h1_ (toHtml name)
                        mapM_ (binderToHtml . snd) (Map.toList (envBindings env))

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
           do h3_ [id_ (Text.pack name)] (toHtml name)
              div_ [class_ "description"] (toHtml description)
              p_ [class_ "sig"] (toHtml typeSignature)
              p_ [class_ "doc"] (toHtml docString)
              --p_ (toHtml (description))
