module Main where

import Control.Monad
import Control.Monad.IO.Class
import System.Console.Haskeline (getInputLine, InputT, runInputT, defaultSettings)
import qualified System.Environment as SystemEnvironment
import System.IO (stdout)
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

repl :: Context -> String -> InputT IO ()
repl context readSoFar =
  do let prompt = strWithColor Yellow (if null readSoFar then (projectPrompt (contextProj context)) else "     ") -- 鲤 / 鲮
     input <- (getInputLine prompt)
     case input of
        Nothing -> return ()
        Just i -> do
          let concat = readSoFar ++ i ++ "\n"
          case balance concat of
            0 -> do let input' = if concat == "\n" then contextLastInput context else concat
                    context' <- liftIO $ executeString context input' "REPL"
                    repl (context' { contextLastInput = input' }) ""
            _ -> repl context concat

arrayModule :: Env
arrayModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "Array", envUseModules = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ templateNth
                                , templateReplicate
                                , templateRepeat
                                , templateCopyingMap
                                , templateEMap
                                , templateFilter
                                --, templateReduce
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
preludeModules carpDir = map (\s -> carpDir ++ "/core/" ++ s ++ ".carp") [ "Macros"
                                                                         , "Int"
                                                                         , "Double"
                                                                         , "Float"
                                                                         , "Array"
                                                                         , "String"
                                                                         , "Char"
                                                                         , "Bool"
                                                                         , "IO"
                                                                         , "System"
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
          runInputT defaultSettings (repl context' "")

