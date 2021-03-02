{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Project where

import Data.Hashable
import GHC.Generics (Generic)
import Info
import Util

data Target = Native | Target String

instance Show Target where
  show Native = "native"
  show (Target x) = x

-- | Project (represents a lot of useful information for working at the REPL and building executables)
data Project = Project
  { projectTitle :: String,
    projectIncludes :: [Includer],
    projectPreproc  :: [String],
    projectCFlags :: [String],
    projectLibFlags :: [String],
    projectPkgConfigFlags :: [String],
    projectFiles :: [(FilePath, ReloadMode)],
    projectAlreadyLoaded :: [FilePath],
    projectEchoC :: Bool,
    projectLibDir :: FilePath,
    projectCarpDir :: FilePath,
    projectOutDir :: FilePath,
    projectDocsDir :: FilePath,
    projectDocsLogo :: FilePath,
    projectDocsPrelude :: String,
    projectDocsURL :: String,
    projectDocsGenerateIndex :: Bool,
    projectDocsStyling :: String,
    projectPrompt :: String,
    projectCarpSearchPaths :: [FilePath],
    projectPrintTypedAST :: Bool,
    projectCompiler :: String,
    projectTarget :: Target,
    projectCore :: Bool,
    projectEchoCompilationCommand :: Bool,
    projectCanExecute :: Bool,
    projectFilePathPrintLength :: FilePathPrintLength,
    projectGenerateOnly :: Bool,
    projectBalanceHints :: Bool,
    projectForceReload :: Bool, -- Setting this to true will make the `load-once` command work just like `load`.
    projectCModules :: [FilePath],
    projectLoadStack :: [FilePath]
  }

projectFlags :: Project -> String
projectFlags proj = joinWithSpace (projectCFlags proj ++ projectLibFlags proj)

instance Show Project where
  show Project {..} =
    unlines
      [ "Title: " ++ projectTitle,
        "Compiler: " ++ projectCompiler,
        "Target: " ++ show projectTarget,
        "Includes:\n    " ++ joinIndented (map show projectIncludes),
        "Preprocessor directives:\n    " ++ joinIndented (map show projectPreproc),
        "Cflags:\n    " ++ joinIndented projectCFlags,
        "Library flags:\n    " ++ joinIndented projectLibFlags,
        "Flags for pkg-config:\n    " ++ joinIndented projectPkgConfigFlags,
        "Carp source files:\n    " ++ joinIndented (map showLoader projectFiles),
        "Already loaded:\n    " ++ joinIndented projectAlreadyLoaded,
        "Echo C: " ++ showB projectEchoC,
        "Echo compilation command: " ++ showB projectEchoCompilationCommand,
        "Can execute: " ++ showB projectCanExecute,
        "Output directory: " ++ projectOutDir,
        "Docs directory: " ++ projectDocsDir,
        "Docs logo: " ++ projectDocsLogo,
        "Docs prelude: " ++ projectDocsPrelude,
        "Docs Project URL: " ++ projectDocsURL,
        "Docs generate index: " ++ showB projectDocsGenerateIndex,
        "Docs CSS URL: " ++ projectDocsStyling,
        "Library directory: " ++ projectLibDir,
        "CARP_DIR: " ++ projectCarpDir,
        "Prompt: " ++ projectPrompt,
        "Using Core: " ++ showB projectCore,
        "Search paths for 'load' command:\n    " ++ joinIndented projectCarpSearchPaths,
        "Print AST (with 'info' command): " ++ showB projectPrintTypedAST,
        "File path print length (when using --check): " ++ show projectFilePathPrintLength,
        "Generate Only: " ++ showB projectGenerateOnly,
        "Balance Hints: " ++ showB projectBalanceHints,
        "Force Reload: " ++ showB projectForceReload,
        "C modules:\n    " ++ joinIndented projectCModules,
        "Load stack:\n    " ++ joinIndented projectLoadStack
      ]
    where
      showB b = if b then "true" else "false"
      joinIndented = joinWith "\n    "

-- | Represent the inclusion of a C header file, either like <string.h> or "string.h"
data Includer
  = SystemInclude String
  | RelativeInclude String
  deriving (Eq, Generic)

instance Hashable Includer

instance Show Includer where
  show (SystemInclude file) = "<" ++ file ++ ">"
  show (RelativeInclude file) = "\"" ++ file ++ "\""

-- | This flag is used on Carp source files to decide wether to reload them or not when calling `(reload)` / `:r`
data ReloadMode = DoesReload | Frozen deriving (Show)

showLoader :: (FilePath, ReloadMode) -> String
showLoader (fp, DoesReload) = fp
showLoader (fp, Frozen) = fp ++ " (frozen)"
