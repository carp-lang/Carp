module Main where

import Control.Monad
import qualified System.Environment as SystemEnvironment
import System.IO (hFlush, stdout)
import System.Info (os)
import qualified Data.Map as Map
import ColorText
import Obj
import Types
import Commands
import Template
import ArrayTemplates
import Parsing

defaultProject :: Project
defaultProject = Project { projectTitle = "Untitled"
                         , projectIncludes = [SystemInclude "prelude.h"]
                         , projectCFlags = ["-fPIC"]
                         , projectLibFlags = ["-lm"]
                         , projectFiles = []
                         , projectEchoC = False
                         , projectCarpDir = "./"
                         , projectOutDir = "./out/"
                         , projectPrompt = if os == "darwin" then "鲮 " else "> "
                         }

repl :: Context -> String -> IO ()
repl context readSoFar =
  do putStrWithColor Yellow (if null readSoFar then (projectPrompt (contextProj context)) else "     ") -- 鲤 / 鲮
     hFlush stdout
     input <- fmap (\s -> readSoFar ++ s ++ "\n") getLine
     case balance input of
       0 -> do let input' = if input == "\n" then contextLastInput context else input
               context' <- executeString context input' "REPL"
               repl (context' { contextLastInput = input' }) ""
       _ -> repl context input

arrayModule :: Env
arrayModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "Array", envUseModules = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ templateNth
                                , templateReplicate
                                , templateRepeat
                                , templateCopyingMap
                                , templateEMap
                                , templateFilter
                                , templateReduce
                                , templateRange
                                , templateRaw
                                , templateAset
                                , templateAsetBang
                                , templateCount
                                , templatePushBack
                                , templatePopBack
                                , templateDeleteArray
                                , templateCopyArray
                                , templateStrArray
                                ]

startingGlobalEnv :: Env
startingGlobalEnv = Env { envBindings = bs,
                          envParent = Nothing,
                          envModuleName = Nothing,
                          envUseModules = [(SymPath [] "String")],
                          envMode = ExternalEnv
                        }
  where bs = Map.fromList [ register "and" (FuncTy [BoolTy, BoolTy] BoolTy)
                          , register "or" (FuncTy [BoolTy, BoolTy] BoolTy)
                          , register "not" (FuncTy [BoolTy] BoolTy)
                          , templateNoop
                          , ("Array", Binder (XObj (Mod arrayModule) Nothing Nothing))
                          , register "NULL" (VarTy "a")
                          ]

startingTypeEnv :: Env
startingTypeEnv = Env { envBindings = Map.empty, envParent = Nothing, envModuleName = Nothing, envUseModules = [], envMode = ExternalEnv }

preludeModules :: String -> [String]
preludeModules carpDir = map (\s -> carpDir ++ "/core/" ++ s ++ ".carp") [ "Int"
                                                                         , "Double"
                                                                         , "Float"
                                                                         , "Array"
                                                                         , "String"
                                                                         , "Char"
                                                                         , "Bool"
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
          context <- foldM executeCommand (Context startingGlobalEnv (TypeEnv startingTypeEnv) [] projectWithCarpDir "")
                                          (map Load (preludeModules (projectCarpDir projectWithCarpDir)))
          context' <- foldM executeCommand context (map Load args)
          repl context' ""
          
