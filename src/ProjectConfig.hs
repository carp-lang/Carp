-- | Defines a frontend for manipulating Project level data.
module ProjectConfig (projectKeyMap) where

import Data.Either (rights)
import Info
import qualified Map
import Obj
    ( XObj(XObj),
      Obj(Str, Lst, Bol),
      unwrapStringXObj,
      wrapString,
      wrapList,
      wrapArray,
      isStr )
import Project
import Util

--------------------------------------------------------------------------------
-- Project + XObj manipulation
--
-- Ideally, we'd define these in Project.hs, but these functions depend on XObj,
-- which would introduce a circular dep into Project.hs.

-- Retrieve a project configuration value as an xobj.
type ProjectGetter = (Project -> XObj)

-- Set a project configuration value. Can return either an error message or an
-- updated project.
type ProjectSetter = (Project -> XObj -> Either String Project)

-- | Get a project's title.
projectGetTitle :: ProjectGetter
projectGetTitle proj =
  let t = projectTitle proj
   in XObj (Str t) Nothing Nothing

-- | Set a project's title.
projectSetTitle :: ProjectSetter
projectSetTitle proj (XObj (Str t) _ _) = Right (proj {projectTitle = t})
projectSetTitle _ _ = Left "can't use a non-string as a project title"

-- | Get the project's C header includes.
projectGetIncludes :: ProjectGetter
projectGetIncludes proj =
  let is = projectIncludes proj
   in wrapList (map (wrapString . show) is)

-- | Get the project's C preprocessor emissions.
projectGetPreproc :: ProjectGetter
projectGetPreproc proj =
  let ps = projectPreproc proj
   in wrapList (map wrapString ps)

-- | Get the project's C compiler flags.
projectGetCFlags :: ProjectGetter
projectGetCFlags proj =
  let fs = projectCFlags proj
   in wrapList (map wrapString fs)

-- | Set the project's C compiler flags
projectSetCFlags :: ProjectSetter
projectSetCFlags proj (XObj (Lst flags) _ _) =
  if not $ all isStr flags
    then Left "can't use a non-string as a C compiler flag"
    else Right proj {projectCFlags = rights $ map unwrapStringXObj flags}
projectSetCFlags _ _ = Left "can't use a non-string as a C compiler flag"

-- | Get the project's C compiler library flags.
projectGetLibFlags :: ProjectGetter
projectGetLibFlags proj =
  let ls = projectLibFlags proj
   in wrapList (map wrapString ls)

-- | Set the project's C compiler library flags
projectSetLibFlags :: ProjectSetter
projectSetLibFlags proj (XObj (Lst flags) _ _) =
  if not $ all isStr flags
    then Left "can't use a non-string as a C compiler library flag"
    else Right proj {projectLibFlags = rights $ map unwrapStringXObj flags}
projectSetLibFlags _ _ = Left "can't use non-string as library flag"

-- | Get the pkg-config flags for the project.
projectGetPkgConfigFlags :: ProjectGetter
projectGetPkgConfigFlags proj =
  let fs = projectPkgConfigFlags proj
   in wrapArray (map wrapString fs)

-- | Set the project's pkg-config flags
projectSetPkgConfigFlags :: ProjectSetter
projectSetPkgConfigFlags proj (XObj (Lst flags) _ _) =
  if not $ all isStr flags
    then Left "can't use a non-string as a C compiler library flag"
    else Right proj {projectPkgConfigFlags = rights $ map unwrapStringXObj flags}
projectSetPkgConfigFlags _ _ = Left "can't use non-string as pkg-config flag"

projectGetEchoC :: ProjectGetter
projectGetEchoC proj = XObj (Bol (projectEchoC proj)) Nothing Nothing

projectSetEchoC :: ProjectSetter
projectSetEchoC proj (XObj (Bol b) _ _) = Right (proj {projectEchoC = b})
projectSetEchoC _ _ = Left "can't use non-bool as echo-c value"

-- | Get the output directory for the project.
projectGetOutDir :: ProjectGetter
projectGetOutDir proj = XObj (Str (projectOutDir proj)) Nothing Nothing

-- | Set the output directory for the project.
projectSetOutDir :: ProjectSetter
projectSetOutDir proj (XObj (Str dir) _ _) = Right (proj {projectOutDir = dir})
projectSetOutDir _ _ = Left "can't use non-string as output directory"

-- | Get the documentation directory for the project.
projectGetDocsDir :: ProjectGetter
projectGetDocsDir proj = XObj (Str (projectDocsDir proj)) Nothing Nothing

-- | Set the documentation directory for the project.
projectSetDocsDir :: ProjectSetter
projectSetDocsDir proj (XObj (Str dir) _ _) = Right (proj {projectDocsDir = dir})
projectSetDocsDir _ _ = Left "can't use non-string as docs directory"

-- | Get the documentation logo for the project.
projectGetDocsLogo :: ProjectGetter
projectGetDocsLogo proj = XObj (Str (projectDocsLogo proj)) Nothing Nothing

-- | Set the documentation logo for the project.
projectSetDocsLogo :: ProjectSetter
projectSetDocsLogo proj (XObj (Str dir) _ _) = Right (proj {projectDocsLogo = dir})
projectSetDocsLogo _ _ = Left "can't use non-string as docs logo"

-- | Get the documentation prelude for the project.
projectGetDocsPrelude :: ProjectGetter
projectGetDocsPrelude proj = XObj (Str (projectDocsPrelude proj)) Nothing Nothing

-- | Set the documentation prelude for the project.
projectSetDocsPrelude :: ProjectSetter
projectSetDocsPrelude proj (XObj (Str text) _ _) = Right (proj {projectDocsPrelude = text})
projectSetDocsPrelude _ _ = Left "can't use non-string as docs prelude"

-- | Get the documentation URL for the project.
projectGetDocsURL :: ProjectGetter
projectGetDocsURL proj = XObj (Str (projectDocsURL proj)) Nothing Nothing

-- | Set the documentation URL for the project.
projectSetDocsURL :: ProjectSetter
projectSetDocsURL proj (XObj (Str url) _ _) = Right (proj {projectDocsURL = url})
projectSetDocsURL _ _ = Left "can't use non-string as docs url"

-- | Get the generate-index option for the project.
projectGetDocsGenerateIndex :: ProjectGetter
projectGetDocsGenerateIndex proj = XObj (Bol (projectDocsGenerateIndex proj)) Nothing Nothing

-- | Set the generate-index option for the project.
projectSetDocsGenerateIndex :: ProjectSetter
projectSetDocsGenerateIndex proj (XObj (Bol b) _ _) = Right (proj {projectDocsGenerateIndex = b})
projectSetDocsGenerateIndex _ _ = Left "can't use non-bool as docs generate index option"

-- | Get the documentation styling CSS path for the project.
projectGetDocsStyling :: ProjectGetter
projectGetDocsStyling proj = XObj (Str (projectDocsStyling proj)) Nothing Nothing

-- | Set the documentation styling CSS path for the project.
projectSetDocsStyling :: ProjectSetter
projectSetDocsStyling proj (XObj (Str path) _ _) = Right (proj {projectDocsStyling = path})
projectSetDocsStyling _ _ = Left "can't use non-string as docs styling path"

-- | Get the prompt for the project.
projectGetPrompt :: ProjectGetter
projectGetPrompt proj = XObj (Str (projectPrompt proj)) Nothing Nothing

-- | Set the prompt for the project.
projectSetPrompt :: ProjectSetter
projectSetPrompt proj (XObj (Str p) _ _) = Right (proj {projectPrompt = p})
projectSetPrompt _ _ = Left "can't use non-string as project prompt"

-- | Get the search paths for the project.
projectGetCarpSearchPaths :: ProjectGetter
projectGetCarpSearchPaths proj =
  let fs = projectCarpSearchPaths proj
   in wrapList (map wrapString fs)

-- | Set the project's carp search paths
--
-- NOTE: This retains existing behavior in which one can only add one flag at
-- a time and the flags are append only. A slightly more functional interface
-- would take a list of flags as arguments;
projectSetCarpSearchPaths :: ProjectSetter
projectSetCarpSearchPaths proj (XObj (Str p) _ _) = Right (proj {projectCarpSearchPaths = addIfNotPresent p (projectCarpSearchPaths proj)})
projectSetCarpSearchPaths _ _ = Left "can't use non-string as search path"

-- | Get the print-ast option for the project.
projectGetPrintTypedAST :: ProjectGetter
projectGetPrintTypedAST proj = XObj (Bol (projectPrintTypedAST proj)) Nothing Nothing

-- | Set the print-ast option for the project.
projectSetPrintTypedAST :: ProjectSetter
projectSetPrintTypedAST proj (XObj (Bol b) _ _) = Right (proj {projectPrintTypedAST = b})
projectSetPrintTypedAST _ _ = Left "can't use non-bool as print-ast option"

-- | Get the C compiler for the project.
projectGetCompiler :: ProjectGetter
projectGetCompiler proj = XObj (Str (projectCompiler proj)) Nothing Nothing

-- | Set the C compiler for the project.
projectSetCompiler :: ProjectSetter
projectSetCompiler proj (XObj (Str c) _ _) = Right (proj {projectCompiler = c})
projectSetCompiler _ _ = Left "can't use non-string as compiler"

-- | Get the target for the project.
projectGetTarget :: ProjectGetter
projectGetTarget proj = XObj (Str (show (projectTarget proj))) Nothing Nothing

-- | Set the target for the project.
projectSetTarget :: ProjectSetter
projectSetTarget proj (XObj (Str "native") _ _) = Right (proj {projectTarget = Native})
projectSetTarget proj (XObj (Str t) _ _) = Right (proj {projectTarget = Target t})
projectSetTarget _ _ = Left "can't use non-string as target"

-- | Get the core configuration option for the project.
projectGetCore :: ProjectGetter
projectGetCore proj = XObj (Bol (projectCore proj)) Nothing Nothing

-- | Set the core configuration option for the project.
projectSetCore :: ProjectSetter
projectSetCore proj (XObj (Bol b) _ _) = Right (proj {projectCore = b})
projectSetCore _ _ = Left "can't use non-bool as core option"

-- | Get the echo compilation configuration option for the project.
projectGetEchoCompilationCommand :: ProjectGetter
projectGetEchoCompilationCommand proj = XObj (Bol (projectEchoCompilationCommand proj)) Nothing Nothing

-- | Set the echo compilation configuration option for the project.
projectSetEchoCompilationCommand :: ProjectSetter
projectSetEchoCompilationCommand proj (XObj (Bol b) _ _) = Right (proj {projectEchoCompilationCommand = b})
projectSetEchoCompilationCommand _ _ = Left "can't use non-bool as echo-compiler-cmd option"

-- | Get the file path print length for the project.
projectGetFilePathPrintLength :: ProjectGetter
projectGetFilePathPrintLength proj = wrapString (show (projectFilePathPrintLength proj))

-- | Set the file path print length for the project.
projectSetFilePathPrintLength :: ProjectSetter
projectSetFilePathPrintLength proj (XObj (Str "short") _ _) = Right (proj {projectFilePathPrintLength = ShortPath})
projectSetFilePathPrintLength proj (XObj (Str "full") _ _) = Right (proj {projectFilePathPrintLength = FullPath})
projectSetFilePathPrintLength _ (XObj (Str s) _ _) = Left (s ++ "is not a valid value for file-path-print-length")
projectSetFilePathPrintLength _ _ = Left "can't use non-string as file-path-print-length"

-- | Get the generate-only configuration option for the project.
projectGetGenerateOnly :: ProjectGetter
projectGetGenerateOnly proj = XObj (Bol (projectGenerateOnly proj)) Nothing Nothing

-- | Set the generate-only configuration option for the project.
projectSetGenerateOnly :: ProjectSetter
projectSetGenerateOnly proj (XObj (Bol b) _ _) = Right (proj {projectGenerateOnly = b})
projectSetGenerateOnly _ _ = Left "can't use non-bool as generate-only option"

-- | Get the balance hints configuration option for the project.
projectGetBalanceHints :: ProjectGetter
projectGetBalanceHints proj = XObj (Bol (projectBalanceHints proj)) Nothing Nothing

-- | Set the balance hints configuration option for the project.
projectSetBalanceHints :: ProjectSetter
projectSetBalanceHints proj (XObj (Bol b) _ _) = Right (proj {projectBalanceHints = b})
projectSetBalanceHints _ _ = Left "can't use non-bool as paren-balance-hints option"

-- | Get the generate-only configuration option for the project.
projectGetForceReload :: ProjectGetter
projectGetForceReload proj = XObj (Bol (projectForceReload proj)) Nothing Nothing

-- | Set the generate-only configuration option for the project.
projectSetForceReload :: ProjectSetter
projectSetForceReload proj (XObj (Bol b) _ _) = Right (proj {projectForceReload = b})
projectSetForceReload _ _ = Left "can't use non-bool as force-reload option"

-- | Get the c modules for a project.
projectGetCModules :: ProjectGetter
projectGetCModules proj = wrapList (map wrapString (projectCModules proj))

-- | Set the c modules for a project.
--
-- NOTE: This retains existing behavior in which one can only add one flag at
-- a time and the flags are append only. A slightly more functional interface
-- would take a list of flags as arguments;
projectSetCModules :: ProjectSetter
projectSetCModules proj (XObj (Str p) _ _) = Right (proj {projectCModules = addIfNotPresent p (projectCModules proj)})
projectSetCModules _ _ = Left "can't use non-string as c module"

-- | Get the load stack of a project.
projectGetLoadStack :: ProjectGetter
projectGetLoadStack proj = wrapArray (map wrapString (projectLoadStack proj))

-- | Mapping of compiler defined project keys to getter and setter functions.
-- This helps ensure we automatically enable access of project configuration
-- fields from Carp as they are added to the compiler.
--
-- The first field is the key's getter, the second is its setter. If the field
-- should not have a setter, set the setter field to Nothing. If the field
-- should not have a getter, set the getter field to Nothing.
projectKeyMap :: Map.Map String (Maybe ProjectGetter, Maybe ProjectSetter)
projectKeyMap =
  Map.fromList
    [ ("title", (Just projectGetTitle, Just projectSetTitle)),
      ("includes", (Just projectGetIncludes, Nothing)), -- has special setter variants defined in Commands.hs
      ("preproc", (Just projectGetPreproc, Nothing)), -- has special setter variants defined in Commands.hs
      ("cflag", (Just projectGetCFlags, Just projectSetCFlags)),
      ("libflag", (Just projectGetLibFlags, Just projectSetLibFlags)),
      ("pkgconfigflag", (Just projectGetPkgConfigFlags, Just projectSetPkgConfigFlags)),
      ("echo-c", (Just projectGetEchoC, Just projectSetEchoC)),
      ("output-directory", (Just projectGetOutDir, Just projectSetOutDir)),
      ("docs-directory", (Just projectGetDocsDir, Just projectSetDocsDir)),
      ("docs-logo", (Just projectGetDocsLogo, Just projectSetDocsLogo)),
      ("docs-prelude", (Just projectGetDocsPrelude, Just projectSetDocsPrelude)),
      ("docs-url", (Just projectGetDocsURL, Just projectSetDocsURL)),
      ("docs-generate-index", (Just projectGetDocsGenerateIndex, Just projectSetDocsGenerateIndex)),
      ("docs-styling", (Just projectGetDocsStyling, Just projectSetDocsStyling)),
      ("prompt", (Just projectGetPrompt, Just projectSetPrompt)),
      ("search-path", (Just projectGetCarpSearchPaths, Just projectSetCarpSearchPaths)),
      ("print-ast", (Just projectGetPrintTypedAST, Just projectSetPrintTypedAST)),
      ("compiler", (Just projectGetCompiler, Just projectSetCompiler)),
      ("target", (Just projectGetTarget, Just projectSetTarget)),
      ("core", (Just projectGetCore, Just projectSetCore)),
      ("echo-compiler-cmd", (Just projectGetEchoCompilationCommand, Just projectSetEchoCompilationCommand)),
      ("file-path-print-length", (Just projectGetFilePathPrintLength, Just projectSetFilePathPrintLength)),
      ("generate-only", (Just projectGetGenerateOnly, Just projectSetGenerateOnly)),
      ("paren-balance-hints", (Just projectGetBalanceHints, Just projectSetBalanceHints)),
      ("force-reload", (Just projectGetForceReload, Just projectSetForceReload)),
      ("cmod", (Just projectGetCModules, Just projectSetCModules)),
      ("load-stack", (Just projectGetLoadStack, Nothing))
    ]
