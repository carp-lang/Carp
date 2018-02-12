module Main where

import Control.Monad
import qualified System.Environment as SystemEnvironment
import System.IO (stdout)
import System.Console.Haskeline (runInputT)

import ColorText
import Obj
import Types
import Repl
import StartingEnv
import Eval
import Util

defaultProject :: Project
defaultProject =
  Project { projectTitle = "Untitled"
          , projectIncludes = [SystemInclude "core.h"]
          , projectCFlags = [""]
          , projectLibFlags = [""]
          , projectFiles = []
          , projectEchoC = False
          , projectCarpDir = "./"
          , projectOutDir = "./out/"
          , projectPrompt = case platform of
                              MacOS -> "鲮 "
                              _     -> "> "
          , projectCarpSearchPaths = []
          , projectPrintTypedAST = False
          , projectCompiler = case platform of
                                Windows -> "cl.exe -lm"
                                _ ->       "clang -fPIC -lm"
          , projectEchoCompilationCommand = False
          }

-- | Starting point of the application.
main :: IO ()
main = do args <- SystemEnvironment.getArgs
          sysEnv <- SystemEnvironment.getEnvironment
          let (argFilesToLoad, execMode, otherOptions) = parseArgs args
              logMemory = LogMemory `elem` otherOptions
              noCore = NoCore `elem` otherOptions
              optimize = Optimize `elem` otherOptions
              projectWithFiles = defaultProject { projectCFlags = (if logMemory then ["-D LOG_MEMORY"] else []) ++
                                                                  (if optimize then ["-O3 -D OPTIMIZE"] else []) ++
                                                                  (projectCFlags defaultProject)
                                                , projectIncludes = if noCore then [] else projectIncludes defaultProject
                                                }
              noArray = False
              coreModulesToLoad = if noCore then [] else (coreModules (projectCarpDir projectWithCarpDir))
              projectWithCarpDir = case lookup "CARP_DIR" sysEnv of
                                     Just carpDir -> projectWithFiles { projectCarpDir = carpDir }
                                     Nothing -> projectWithFiles
              startingContext = (Context
                                 (startingGlobalEnv noArray)
                                 (TypeEnv startingTypeEnv)
                                  []
                                  projectWithCarpDir
                                  ""
                                  execMode)
          context <- loadFiles startingContext coreModulesToLoad
          context' <- loadFiles context argFilesToLoad
          settings <- readlineSettings
          case execMode of
            Repl -> do putStrLn "Welcome to Carp 0.2.0"
                       putStrLn "This is free software with ABSOLUTELY NO WARRANTY."
                       putStrLn "Evaluate (help) for more information."
                       runInputT settings (repl context' "")
            Build -> do _ <- executeString True context' ":b" "Compiler (Build)"
                        return ()
            BuildAndRun -> do _ <- executeString True context' ":bx" "Compiler (Build & Run)"
                              -- TODO: Handle the return value from executeString and return that one to the shell
                              return ()

-- | Options for how to run the compiler.
data OtherOptions = NoCore | LogMemory | Optimize deriving (Show, Eq)

-- | Parse the arguments sent to the compiler from the terminal.
parseArgs :: [String] -> ([FilePath], ExecutionMode, [OtherOptions])
parseArgs args = parseArgsInternal [] Repl [] args
  where parseArgsInternal filesToLoad execMode otherOptions [] =
          (filesToLoad, execMode, otherOptions)
        parseArgsInternal filesToLoad execMode otherOptions (arg:restArgs) =
          case arg of
            "-b" -> parseArgsInternal filesToLoad Build otherOptions restArgs
            "-x" -> parseArgsInternal filesToLoad BuildAndRun otherOptions restArgs
            "--no-core" -> parseArgsInternal filesToLoad execMode (NoCore : otherOptions) restArgs
            "--log-memory" -> parseArgsInternal filesToLoad execMode (LogMemory : otherOptions) restArgs
            "--optimize" -> parseArgsInternal filesToLoad execMode (Optimize : otherOptions) restArgs
            file -> parseArgsInternal (filesToLoad ++ [file]) execMode otherOptions restArgs
