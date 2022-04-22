module Main where

import ColorText
import Control.Monad (foldM, when)
import Data.Maybe
import Eval
import GHC.IO.Encoding
import Info
import Obj
import Options.Applicative
import Path
import Project
import Repl
import StartingEnv
import System.Console.Haskeline (runInputT)
import qualified System.Environment as SystemEnvironment
import System.Exit (exitFailure)
import Types
import Util

defaultProject :: Project
defaultProject =
  Project
    { projectTitle = "Untitled",
      projectIncludes = [],
      projectCFlags = case platform of
        Windows ->
          [ "-D_CRT_SECURE_NO_WARNINGS"
          ]
        _ ->
          [ "-fPIC",
            "-g",
            "-std=c99",
            -- , "-pedantic"
            "-D_DEFAULT_SOURCE",
            "-Wall",
            "-Werror",
            "-Wno-unused-variable",
            "-Wno-self-assign"
          ],
      projectLibFlags = case platform of
        Windows -> []
        _ -> ["-lm"],
      projectFiles = [],
      projectAlreadyLoaded = [],
      projectEchoC = False,
      projectLibDir = "libs",
      projectCarpDir = ".",
      projectOutDir = "out",
      projectDocsDir = "docs",
      projectDocsLogo = "",
      projectDocsPrelude = "",
      projectDocsURL = "",
      projectDocsGenerateIndex = True,
      projectDocsStyling = "carp_style.css",
      projectBalanceHints = True,
      projectPrompt = case platform of
        MacOS -> "鲤 "
        _ -> "> ",
      projectCarpSearchPaths = [],
      projectPrintTypedAST = False,
      projectCompiler = case platform of
        Windows -> "clang-cl.exe"
        _ -> "clang",
      projectTarget = Native,
      projectCore = True,
      projectEchoCompilationCommand = False,
      projectCanExecute = False,
      projectFilePathPrintLength = FullPath,
      projectGenerateOnly = False,
      projectForceReload = False,
      projectPkgConfigFlags = [],
      projectCModules = [],
      projectLoadStack = [],
      projectPreproc = []
    }

-- | Starting point of the application.
main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- SystemEnvironment.getArgs
  sysEnv <- SystemEnvironment.getEnvironment
  fullOpts <- execParser $ Options.Applicative.info (parseFull <**> helper) fullDesc
  let execMode = optExecMode fullOpts
      otherOptions = optOthers fullOpts
      argFilesToLoad = optFiles fullOpts
      logMemory = otherLogMemory otherOptions
      core = not $ otherNoCore otherOptions
      profile = not $ otherNoProfile otherOptions
      optimize = otherOptimize otherOptions
      generateOnly = otherGenerateOnly otherOptions
      prompt = otherPrompt otherOptions
      carpDir = lookup "CARP_DIR" sysEnv
      ifCarpDirSet comp =
        case carpDir of
          Just _ -> comp
          Nothing -> do
            emitWarning "The environment variable `CARP_DIR` is not set."
            if core
              then emitErrorAndExit "Cannot use core libraries without `CARP_DIR` being set (if you want to provide your own, use `--no-core`)."
              else comp
      applySettings p =
        p
          { projectCFlags =
              ["-D LOG_MEMORY" | logMemory]
                ++ ["-O3 -D NDEBUG" | optimize]
                ++ projectCFlags p,
            projectCore = core,
            projectGenerateOnly = generateOnly,
            projectCarpDir = fromMaybe (projectCarpDir p) carpDir,
            projectPrompt = fromMaybe (projectPrompt p) prompt
          }
      project = applySettings defaultProject
      noArray = False
      startingContext =
        Context
          (startingGlobalEnv noArray)
          Nothing
          (TypeEnv startingTypeEnv)
          []
          project
          ""
          execMode
          []
      coreModulesToLoad = if core then coreModules (projectCarpDir project) else []
      execStr :: String -> String -> Context -> IO Context
      execStr i s ctx = executeString True False ctx s i
      execStrs :: String -> [String] -> Context -> IO Context
      execStrs i strs ctx = foldM (\ctx' str' -> execStr i str' ctx') ctx strs
      preloads = optPreload fullOpts
      postloads = optPostload fullOpts
      load = flip loadFiles
      loadOnce = flip loadFilesOnce
  carpProfile <- configPath "profile.carp"
  hasProfile <- doesFileExist carpProfile
  _ <-
    ifCarpDirSet
      ( pure startingContext
          >>= load [carpProfile | hasProfile && profile]
          >>= execStrs "Preload" preloads
          >>= loadOnce coreModulesToLoad
          >>= load argFilesToLoad
          >>= execStrs "Postload" postloads
          >>= \ctx -> case execMode of
            Repl -> do
              putStrLn "Welcome to Carp 0.5.5"
              putStrLn "This is free software with ABSOLUTELY NO WARRANTY."
              putStrLn "Evaluate (help) for more information."
              snd <$> runRepl ctx
            Build -> execStr "Compiler (Build)" "(build)" ctx
            Install thing -> execStr "Installation" ("(load \"" ++ thing ++ "\")") ctx
            BuildAndRun -> execStr "Compiler (Build & Run)" "(do (build) (run))" ctx
            Check -> execStr "Check" "" ctx
      )
  -- TODO: Handle the return value from executeString and return that one to the shell
  pure ()

-- | Options for how to run the compiler.
data FullOptions = FullOptions
  { optExecMode :: ExecutionMode,
    optOthers :: OtherOptions,
    optPreload :: [String],
    optPostload :: [String],
    optFiles :: [FilePath]
  }
  deriving (Show)

parseFull :: Parser FullOptions
parseFull =
  FullOptions
    <$> parseExecMode
    <*> parseOther
    <*> many (strOption (long "eval-preload" <> metavar "CODE" <> help "Eval CODE after loading config and before FILES"))
    <*> many (strOption (long "eval-postload" <> metavar "CODE" <> help "Eval CODE after loading FILES"))
    <*> parseFiles

data OtherOptions = OtherOptions
  { otherNoCore :: Bool,
    otherNoProfile :: Bool,
    otherLogMemory :: Bool,
    otherOptimize :: Bool,
    otherGenerateOnly :: Bool,
    otherPrompt :: Maybe String
  }
  deriving (Show)

parseOther :: Parser OtherOptions
parseOther =
  OtherOptions
    <$> switch (long "no-core" <> help "Don't load Core.carp")
    <*> switch (long "no-profile" <> help "Don't load profile.carp")
    <*> switch (long "log-memory" <> help "Log memory allocations")
    <*> switch (long "optimize" <> help "Optimized build")
    <*> switch (long "generate-only" <> help "Stop after generating the C code")
    <*> optional (strOption (long "prompt" <> help "Set REPL prompt"))

parseExecMode :: Parser ExecutionMode
parseExecMode =
  flag' Check (long "check" <> help "Check project")
    <|> flag' Build (short 'b' <> help "Build project")
    <|> flag' BuildAndRun (short 'x' <> help "Build an run project")
    <|> Install <$> strOption (short 'i' <> help "Install built product")
    <|> pure Repl

parseFiles :: Parser [FilePath]
parseFiles = many (argument str (metavar "FILES..."))
