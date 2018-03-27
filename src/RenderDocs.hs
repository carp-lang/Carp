{-# LANGUAGE OverloadedStrings #-}

module RenderDocs where

import Lucid
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding as E
import System.Directory
import Debug.Trace

import Obj
import Types
import Util

saveDocsForEnv :: Env -> FilePath -> IO ()
saveDocsForEnv env dirPath =
  do let text = renderText (p_ "Woot?")
         string = T.unpack text
         name = case envModuleName env of
                  Just hasName -> hasName
                  Nothing -> "global"
         fullPath = dirPath ++ "/" ++ name ++ ".html"
     createDirectoryIfMissing False dirPath
     writeFile fullPath string
