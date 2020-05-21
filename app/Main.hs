module Main where

import qualified System.Environment as SystemEnvironment
import System.Console.Haskeline (runInputT)
import GHC.IO.Encoding
import Data.Maybe

import ColorText
import Obj
import Types
import Repl
import StartingEnv
import Eval
import Util
import Path

import Options.Applicative

defaultProject :: Project
defaultProject =
  Project { projectTitle = "Untitled"
          , projectIncludes = []
          , projectCFlags = case platform of
              Windows -> ["-D_CRT_SECURE_NO_WARNINGS"]
              _ -> [ "-fPIC"
                   , "-g"
                   ]
          , projectLibFlags = case platform of
              Windows -> []
              _ -> [ "-lm" ]
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
                                Windows -> "clang-cl.exe"
                                _ ->       "clang"
          , projectCore = True
          , projectEchoCompilationCommand = False
          , projectCanExecute = False
          , projectFilePathPrintLength = FullPath
          , projectGenerateOnly = False
          , projectForceReload = False
          }
  where win = platform == Windows


-- | Starting point of the application.
main :: IO ()
main = do setLocaleEncoding utf8
          args <- SystemEnvironment.getArgs
          sysEnv <- SystemEnvironment.getEnvironment
          fullOpts <- execParser $ Options.Applicative.info (parseFull <**> helper) fullDesc
          let execMode = optExecMode fullOpts
              compOptions = optComp fullOpts
              otherOptions = optOthers fullOpts
              argFilesToLoad = optFiles fullOpts
              logMemory = otherLogMemory otherOptions
              core = not $ otherNoCore otherOptions
              profile = not $ otherNoProfile otherOptions
              optimize = otherOptimize otherOptions
              generateOnly = otherGenerateOnly otherOptions
              compiler = compCompiler compOptions
              cflags = compCompFlags compOptions
              ldflags = compLinkFlags compOptions
              prompt = otherPrompt otherOptions
              applySettings p = p { projectCFlags = ["-D LOG_MEMORY" | logMemory] ++
                                                    ["-O3 -D NDEBUG" | optimize] ++
                                                    fromMaybe (projectCFlags p) cflags
                                  , projectLibFlags = fromMaybe (projectLibFlags p) ldflags
                                  , projectCore = core
                                  , projectGenerateOnly = generateOnly
                                  , projectCarpDir = fromMaybe (projectCarpDir p) $ lookup "CARP_DIR" sysEnv
                                  , projectCompiler = fromMaybe (projectCompiler p) compiler
                                  , projectPrompt = prompt
                                  }
              project = applySettings defaultProject
              noArray = False
              startingContext = Context
                                (startingGlobalEnv noArray)
                                Nothing
                                (TypeEnv startingTypeEnv)
                                []
                                project
                                ""
                                execMode
                                []
              coreModulesToLoad = if core then coreModules (projectCarpDir project) else []
          context <- loadFilesOnce startingContext coreModulesToLoad
          carpProfile <- configPath "profile.carp"
          hasProfile <- doesFileExist carpProfile
          context' <- if profile && hasProfile
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
data FullOptions = FullOptions
  { optExecMode :: ExecutionMode
  , optComp :: CompOptions
  , optOthers :: OtherOptions
  , optFiles :: [FilePath]
  } deriving Show

parseFull :: Parser FullOptions
parseFull = FullOptions
  <$> parseExecMode
  <*> parseComp
  <*> parseOther
  <*> parseFiles

data CompOptions = CompOptions
  { compCompiler :: Maybe String
  , compCompFlags :: Maybe [String]
  , compLinkFlags :: Maybe [String]
  } deriving Show

parseComp :: Parser CompOptions
parseComp = CompOptions
  <$> optional (strOption (long "cc" <> help "Set C compiler to use"))
  <*> optional (some (strOption (long "cflag" <> metavar "FLAG" <> help "Add flag to the compiler invocation")))
  <*> optional (some (strOption (long "ldflag" <> metavar "FLAG" <> help "Add flag to the linker invocation")))

data OtherOptions = OtherOptions
  { otherNoCore :: Bool
  , otherNoProfile :: Bool
  , otherLogMemory :: Bool
  , otherOptimize :: Bool
  , otherGenerateOnly :: Bool
  , otherPrompt :: String
  } deriving Show

parseOther :: Parser OtherOptions
parseOther = OtherOptions
  <$> switch (long "no-core" <> help "Don't load Core.carp")
  <*> switch (long "no-profile" <> help "Don't load profile.carp")
  <*> switch (long "log-memory" <> help "Log memory allocations")
  <*> switch (long "optimize" <> help "Optimized build")
  <*> switch (long "generate-only" <> help "Stop after generating the C code")
  <*> strOption (long "prompt" <> help "Set REPL prompt")

parseExecMode :: Parser ExecutionMode
parseExecMode =
  flag' Check (long "check" <> help "Check project")
  <|> flag' Build (short 'b' <> help "Build project")
  <|> flag' BuildAndRun (short 'x' <> help "Build an run project")
  <|> Install <$> strOption (short 'i' <> help "Install built product")
  <|> pure Repl

parseFiles :: Parser [FilePath]
parseFiles = some (argument str (metavar "FILES...")) <|> pure []
