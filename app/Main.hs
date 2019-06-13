module Main where

import Control.Monad
import qualified System.Environment as SystemEnvironment
import System.IO (stdout)
import System.Console.Haskeline (runInputT)
import System.Directory (doesPathExist, getHomeDirectory)
import GHC.IO.Encoding

import ColorText
import Obj
import Types
import Repl
import StartingEnv
import Eval
import Util
import Lookup

defaultProject :: Project
defaultProject =
  Project { projectTitle = "Untitled"
          , projectIncludes = [SystemInclude "core.h"]
          , projectCFlags = [""]
          , projectLibFlags = [""]
          , projectFiles = []
          , projectAlreadyLoaded = []
          , projectEchoC = False
          , projectLibDir = ".carp/libs/"
          , projectCarpDir = "./"
          , projectOutDir = case platform of
                              Windows -> ".\\out"
                              _ -> "./out"
          , projectDocsDir = "./docs/"
          , projectDocsLogo = ""
          , projectDocsPrelude = ""
          , projectDocsURL = ""
          , projectDocsGenerateIndex = True
          , projectDocsStyling = "carp_style.css"
          , projectPrompt = case platform of
                              MacOS -> "鲮 "
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
          }

-- | Starting point of the application.
main :: IO ()
main = do setLocaleEncoding utf8
          args <- SystemEnvironment.getArgs
          sysEnv <- SystemEnvironment.getEnvironment
          let (argFilesToLoad, execMode, otherOptions) = parseArgs args
              logMemory = LogMemory `elem` otherOptions
              noCore = NoCore `elem` otherOptions
              optimize = Optimize `elem` otherOptions
              generateOnly = GenerateOnly `elem` otherOptions
              projectWithFiles = defaultProject { projectCFlags = (if logMemory then ["-D LOG_MEMORY"] else []) ++
                                                                  (if optimize then ["-O3 -D NDEBUG"] else []) ++
                                                                  (projectCFlags defaultProject),
                                                  projectCore = not noCore,
                                                  projectGenerateOnly = generateOnly}
              noArray = False
              coreModulesToLoad = if noCore then [] else (coreModules (projectCarpDir projectWithCarpDir))
              projectWithCarpDir = case lookup "CARP_DIR" sysEnv of
                                     Just carpDir -> projectWithFiles { projectCarpDir = carpDir }
                                     Nothing -> projectWithFiles
              projectWithCustomPrompt = setCustomPromptFromOptions projectWithCarpDir otherOptions
              startingContext = (Context
                                 (startingGlobalEnv noArray)
                                 (TypeEnv startingTypeEnv)
                                  []
                                  projectWithCustomPrompt
                                  ""
                                  execMode)
          context <- loadFiles startingContext coreModulesToLoad
          home <- getHomeDirectory
          let carpProfile = home ++ "/.carp/profile.carp"
          hasProfile <- doesPathExist carpProfile
          context' <- if hasProfile
                      then loadFiles context [carpProfile]
                      else do --putStrLn ("No '" ++ carpProfile ++ "' found.")
                              return context
          finalContext <- loadFiles context' argFilesToLoad
          settings <- readlineSettings (bindingNames $ contextGlobalEnv finalContext)
          case execMode of
            Repl -> do putStrLn "Welcome to Carp 0.2.0"
                       putStrLn "This is free software with ABSOLUTELY NO WARRANTY."
                       putStrLn "Evaluate (help) for more information."
                       runInputT settings (repl finalContext "")
            Build -> do _ <- executeString True finalContext ":b" "Compiler (Build)"
                        return ()
            Install thing ->
              do _ <- executeString True finalContext
                      ("(load \"" ++ thing ++ "\")")
                      "Installation"
                 return ()
            BuildAndRun -> do _ <- executeString True finalContext ":bx" "Compiler (Build & Run)"
                              -- TODO: Handle the return value from executeString and return that one to the shell
                              return ()
            Check -> do return ()

-- | Options for how to run the compiler.
data OtherOptions = NoCore
                  | LogMemory
                  | Optimize
                  | GenerateOnly
                  | SetPrompt String
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
            "--log-memory" -> parseArgsInternal filesToLoad execMode (LogMemory : otherOptions) restArgs
            "--optimize" -> parseArgsInternal filesToLoad execMode (Optimize : otherOptions) restArgs
            "--generate-only" -> parseArgsInternal filesToLoad execMode (GenerateOnly : otherOptions) restArgs
            "--prompt" -> case restArgs of
                             newPrompt : restRestArgs ->
                               parseArgsInternal filesToLoad execMode (SetPrompt newPrompt : otherOptions) restRestArgs
                             _ ->
                               error "No prompt given after --prompt"
            file -> parseArgsInternal (filesToLoad ++ [file]) execMode otherOptions restArgs

setCustomPromptFromOptions :: Project -> [OtherOptions] -> Project
setCustomPromptFromOptions project (o:os) =
  case o of
    SetPrompt newPrompt -> setCustomPromptFromOptions (project { projectPrompt = newPrompt }) os
    _ -> setCustomPromptFromOptions project os
setCustomPromptFromOptions project _ =
  project
