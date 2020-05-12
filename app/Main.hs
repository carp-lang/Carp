module Main where

import qualified System.Environment as SystemEnvironment
import System.Console.Haskeline (runInputT)
import GHC.IO.Encoding

import ColorText
import Obj
import Types
import Repl
import StartingEnv
import Eval
import Util
import Path

defaultProject :: Project
defaultProject =
  Project { projectTitle = "Untitled"
          , projectIncludes = []
          , projectCFlags = [""]
          , projectLibFlags = [""]
          , projectFiles = []
          , projectAlreadyLoaded = []
          , projectEchoC = False
          , projectLibDir = "libs"
          , projectCarpDir = "."
          , projectOutDir = "out"
          , projectDocsDir = "docs"
          , projectDocsLogo = ""
          , projectDocsPrelude = ""
          , projectDocsURL = ""
          , projectDocsGenerateIndex = True
          , projectDocsStyling = "carp_style.css"
          , projectBalanceHints = True
          , projectPrompt = case platform of
                              MacOS -> "鲤 "
                              _     -> "> "
          , projectCarpSearchPaths = []
          , projectPrintTypedAST = False
          , projectCompiler = case platform of
                                Windows -> "clang-cl.exe -D _CRT_SECURE_NO_WARNINGS"
                                _ ->       "clang -fPIC -lm -g"
          , projectCore = True
          , projectEchoCompilationCommand = False
          , projectCanExecute = False
          , projectFilePathPrintLength = FullPath
          , projectGenerateOnly = False
          , projectForceReload = False
          , projectTarget = Native
          }

-- | Starting point of the application.
main :: IO ()
main = do setLocaleEncoding utf8
          args <- SystemEnvironment.getArgs
          sysEnv <- SystemEnvironment.getEnvironment
          let (argFilesToLoad, execMode, otherOptions) = parseArgs args
              logMemory = LogMemory `elem` otherOptions
              noCore = NoCore `elem` otherOptions
              noProfile = NoProfile `elem` otherOptions
              optimize = Optimize `elem` otherOptions
              generateOnly = GenerateOnly `elem` otherOptions
              projectWithFiles = defaultProject { projectCFlags = ["-D LOG_MEMORY" | logMemory] ++
                                                                  ["-O3 -D NDEBUG" | optimize] ++
                                                                  projectCFlags defaultProject,
                                                  projectCore = not noCore,
                                                  projectGenerateOnly = generateOnly}
              noArray = False
              coreModulesToLoad = if noCore then [] else coreModules (projectCarpDir projectWithCarpDir)
              projectWithCarpDir = case lookup "CARP_DIR" sysEnv of
                                     Just carpDir -> projectWithFiles { projectCarpDir = carpDir }
                                     Nothing -> projectWithFiles
              projectWithCustomPrompt = setCustomPromptFromOptions projectWithCarpDir otherOptions
              projectWithCrossCompiler = setCrossCompilerFromOptions projectWithCustomPrompt otherOptions
              startingContext = Context
                                 (startingGlobalEnv noArray)
                                 Nothing
                                 (TypeEnv startingTypeEnv)
                                 []
                                 projectWithCrossCompiler
                                 ""
                                 execMode
                                 []
          context <- loadFilesOnce startingContext coreModulesToLoad
          carpProfile <- configPath "profile.carp"
          hasProfile <- doesFileExist carpProfile
          context' <- if (not noProfile) && hasProfile
                      then loadFiles context [carpProfile]
                      else return context
          finalContext <- loadFiles context' argFilesToLoad
          case execMode of
            Repl -> do putStrLn "Welcome to Carp 0.3.0"
                       putStrLn "This is free software with ABSOLUTELY NO WARRANTY."
                       putStrLn "Evaluate (help) for more information."
                       _ <- runRepl finalContext
                       return ()
            Build -> do _ <- executeString True False finalContext "(build)" "Compiler (Build)"
                        return ()
            Install thing ->
              do _ <- executeString True False finalContext
                      ("(load \"" ++ thing ++ "\")")
                      "Installation"
                 return ()
            BuildAndRun -> do _ <- executeString True False finalContext "(do (build) (run))" "Compiler (Build & Run)"
                              -- TODO: Handle the return value from executeString and return that one to the shell
                              return ()
            Check -> return ()

-- | Options for how to run the compiler.
data OtherOptions = NoCore
                  | NoProfile
                  | LogMemory
                  | Optimize
                  | GenerateOnly
                  | SetPrompt String
                  | Cross String
                  deriving (Show, Eq)

-- | Parse the arguments sent to the compiler from the terminal.
-- | TODO: Switch to 'cmdargs' library for parsing these!
parseArgs :: [String] -> ([FilePath], ExecutionMode, [OtherOptions])
parseArgs args = parseArgsInternal [] Repl [] args
  where parseArgsInternal filesToLoad execMode otherOptions [] =
          (filesToLoad, execMode, otherOptions)
        parseArgsInternal filesToLoad execMode otherOptions (arg:restArgs) =
          case arg of
            "-b" -> parseArgsInternal filesToLoad Build otherOptions restArgs
            "-x" -> parseArgsInternal filesToLoad BuildAndRun otherOptions restArgs
            "-i" -> parseArgsInternal filesToLoad (Install (head restArgs)) otherOptions (tail restArgs)
            "--check" -> parseArgsInternal filesToLoad Check otherOptions restArgs
            "--no-core" -> parseArgsInternal filesToLoad execMode (NoCore : otherOptions) restArgs
            "--no-profile" -> parseArgsInternal filesToLoad execMode (NoProfile : otherOptions) restArgs
            "--log-memory" -> parseArgsInternal filesToLoad execMode (LogMemory : otherOptions) restArgs
            "--optimize" -> parseArgsInternal filesToLoad execMode (Optimize : otherOptions) restArgs
            "--generate-only" -> parseArgsInternal filesToLoad execMode (GenerateOnly : otherOptions) restArgs
            "--prompt" -> case restArgs of
                             newPrompt : restRestArgs ->
                               parseArgsInternal filesToLoad execMode (SetPrompt newPrompt : otherOptions) restRestArgs
                             _ ->
                               error "No prompt given after --prompt"
            "--cross" -> case restArgs of
                           target : restRestArgs ->
                             parseArgsInternal filesToLoad execMode (Cross target : otherOptions) restRestArgs
                           _ -> error "Missing target after --cross"
            file -> parseArgsInternal (filesToLoad ++ [file]) execMode otherOptions restArgs

setCustomPromptFromOptions :: Project -> [OtherOptions] -> Project
setCustomPromptFromOptions project (o:os) =
  case o of
    SetPrompt newPrompt -> setCustomPromptFromOptions (project { projectPrompt = newPrompt }) os
    _ -> setCustomPromptFromOptions project os
setCustomPromptFromOptions project _ =
  project
setCrossCompilerFromOptions :: Project -> [OtherOptions] -> Project
setCrossCompilerFromOptions project (o:os) =
  case o of
    Cross target -> setCrossCompilerFromOptions (project { projectCompiler = "zig cc --target=" ++ target
                                                         , projectTarget = Target target
                                                         }) os
    _ -> setCrossCompilerFromOptions project os
setCrossCompilerFromOptions project _ = project
