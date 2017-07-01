module Main where

import Control.Monad
import System.Console.Readline
import qualified System.Environment as SystemEnvironment
import System.IO (hFlush, stdout)
import qualified Data.Map as Map
import ColorText
import Obj
import Types
import Commands
import Template
import Parsing

defaultProject :: Project
defaultProject = Project { projectTitle = "Untitled"
                         , projectIncludes = [SystemInclude "prelude.h"]
                         , projectCFlags = []
                         , projectLibFlags = []
                         , projectFiles = []
                         , projectEchoC = False
                         , projectCarpDir = "./"
                         , projectOutDir = "./out/"
                         }

repl :: Context -> String -> IO ()
repl context readSoFar =
  do let prompt = strWithColor Yellow (if null readSoFar then "鲮 " else "     ") -- 鲤 / 鲮
     maybeLine <- readline prompt
     case maybeLine of
       Nothing   -> return () -- EOF / control-d
       Just line -> do addHistory line
                       let input = readSoFar ++ line ++ "\n"
                       case balance input of
                         0 -> do let input' = if input == "\n" then contextLastInput context else input
                                 context' <- executeString context input'
                                 repl (context' { contextLastInput = input' }) ""
                         _ -> repl context input

arrayModule :: Env
arrayModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "Array", envImports = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ templateNth
                                , templateReplicate
                                , templateRepeat
                                , templateCopyingMap
                                , templateEMap
                                , templateFilter
                                , templateReduce
                                , templateRaw
                                , templateAset
                                , templateAsetBang
                                , templateCount
                                , templatePushBack
                                , templatePopBack
                                , templateDeleteArray
                                , templateCopyArray
                                ]

startingGlobalEnv :: Env
startingGlobalEnv = Env { envBindings = bs, envParent = Nothing, envModuleName = Nothing, envImports = [], envMode = ExternalEnv }
  where bs = Map.fromList [ register "and" (FuncTy [BoolTy, BoolTy] BoolTy)
                          , register "or" (FuncTy [BoolTy, BoolTy] BoolTy)
                          , register "not" (FuncTy [BoolTy] BoolTy)
                          , templateNoop
                          , ("Array", Binder (XObj (Mod arrayModule) Nothing Nothing))
                          , register "NULL" (VarTy "a")
                          ]

startingTypeEnv :: Env
startingTypeEnv = Env { envBindings = Map.empty, envParent = Nothing, envModuleName = Nothing, envImports = [], envMode = ExternalEnv }

preludeModules :: String -> [String]
preludeModules carpDir = map (\s -> carpDir ++ "/core/" ++ s ++ ".carp") [ "Int"
                                                                         , "Double"
                                                                         , "Float"
                                                                         , "Array"
                                                                         , "String"
                                                                         , "Char"
                                                                         , "IO"
                                                                         , "System"
                                                                         , "Macros"
                                                                         ]

main :: IO ()
main = do putStrLn "Welcome to Carp 0.2.0"
          putStrLn "This is free software with ABSOLUTELY NO WARRANTY."
          putStrLn "Evaluate (help) for more information."
          args <- SystemEnvironment.getArgs
          sysEnv <- SystemEnvironment.getEnvironment
          let projectWithFiles = defaultProject { projectFiles = args }
              projectWithCarpDir = case lookup "CARP_DIR" sysEnv of
                                     Just carpDir -> projectWithFiles { projectCarpDir = carpDir }
                                     Nothing -> projectWithFiles
          context <- foldM executeCommand (Context startingGlobalEnv startingTypeEnv [] projectWithCarpDir "")
                                          (map Load (preludeModules (projectCarpDir projectWithCarpDir)))
          context' <- foldM executeCommand context (map Load args)
          repl context' ""
