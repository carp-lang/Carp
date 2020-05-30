module Main where

import qualified System.Environment as SystemEnvironment
import System.Console.Haskeline (runInputT)
import Control.Monad (foldM)
import GHC.IO.Encoding
import Data.Maybe

import ColorText
import Obj
import Project
import Types
import Repl
import StartingEnv
import Eval
import Util
import Path
import Info

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
          , projectPkgConfigFlags = []
          }

-- | Starting point of the application.
main :: IO ()
main = do setLocaleEncoding utf8
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
              applySettings p = p { projectCFlags = ["-D LOG_MEMORY" | logMemory] ++
                                                    ["-O3 -D NDEBUG" | optimize]
                                                    ++ projectCFlags p
                                  , projectCore = core
                                  , projectGenerateOnly = generateOnly
                                  , projectCarpDir = fromMaybe (projectCarpDir p) $ lookup "CARP_DIR" sysEnv
                                  , projectPrompt = fromMaybe (projectPrompt p) prompt
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
              execStr :: String -> String -> Context -> IO Context
              execStr info str ctx = executeString True False ctx str info
              execStrs :: String -> [String] -> Context -> IO Context
              execStrs info strs ctx = foldM (\ctx str -> execStr info str ctx) ctx strs
              preloads = optPreload fullOpts
              postloads = optPostload fullOpts
              load = flip loadFiles
          carpProfile <- configPath "profile.carp"
          hasProfile <- doesFileExist carpProfile
          _ <- loadFilesOnce startingContext coreModulesToLoad
            >>= load [carpProfile | hasProfile]
            >>= execStrs "Preload" preloads
            >>= load argFilesToLoad
            >>= execStrs "Postload" postloads
            >>= \ctx -> case execMode of
                          Repl -> do putStrLn "Welcome to Carp 0.3.0"
                                     putStrLn "This is free software with ABSOLUTELY NO WARRANTY."
                                     putStrLn "Evaluate (help) for more information."
                                     snd <$> runRepl ctx
                          Build -> execStr "Compiler (Build)" "(build)" ctx
                          Install thing -> execStr "Installation" ("(load \"" ++ thing ++ "\")") ctx
                          BuildAndRun -> execStr "Compiler (Build & Run)" "(do (build) (run))" ctx
                          Check -> execStr "Check" "" ctx
                          -- TODO: Handle the return value from executeString and return that one to the shell
          pure ()
-- | Options for how to run the compiler.
data FullOptions = FullOptions
  { optExecMode :: ExecutionMode
  , optOthers :: OtherOptions
  , optPreload :: [String]
  , optPostload :: [String]
  , optFiles :: [FilePath]
  } deriving Show

parseFull :: Parser FullOptions
parseFull = FullOptions
  <$> parseExecMode
  <*> parseOther
  <*> many (strOption (long "eval-preload" <> metavar "CODE" <> help "Eval CODE after loading config and before FILES"))
  <*> many (strOption (long "eval-postload" <> metavar "CODE" <> help "Eval CODE after loading FILES"))
  <*> parseFiles

data OtherOptions = OtherOptions
  { otherNoCore :: Bool
  , otherNoProfile :: Bool
  , otherLogMemory :: Bool
  , otherOptimize :: Bool
  , otherGenerateOnly :: Bool
  , otherPrompt :: Maybe String
  } deriving Show

parseOther :: Parser OtherOptions
parseOther = OtherOptions
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
