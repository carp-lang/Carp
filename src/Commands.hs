module Commands where

import Control.Exception
import Control.Monad (join, when, foldM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Bits (finiteBitSize)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(..))
import System.Info (os, arch)
import System.Process (callCommand, spawnCommand, waitForProcess)
import System.IO (openFile, hPutStr, hClose, utf8, hSetEncoding, IOMode(..))
import System.Directory (makeAbsolute)
import qualified Data.Map as Map

import Parsing
import Emit
import Obj
import Project
import Types
import Infer
import Deftype
import ColorText
import Template
import Util
import Lookup
import RenderDocs
import TypeError
import Path
import Info
import qualified Meta
import Reify


data CarpException =
    ShellOutException { shellOutMessage :: String, returnCode :: Int }
  | CancelEvaluationException
  | EvalException EvalError
  deriving (Eq, Show)

instance Exception CarpException

-- | A lot of commands need to return nil, which signifies a side effecting function and no printing of the result.
dynamicNil :: Either a XObj
dynamicNil = Right (XObj (Lst []) (Just dummyInfo) (Just UnitTy)) -- TODO: Remove/unwrap (Right ...) to a XObj

-- | Dynamic 'true'.
trueXObj :: XObj
trueXObj = XObj (Bol True) Nothing Nothing

-- | Dynamic 'false'.
falseXObj :: XObj
falseXObj = XObj (Bol False) Nothing Nothing

boolToXObj :: Bool -> XObj
boolToXObj b = if b then trueXObj else falseXObj

-- | Use this function to register commands in the environment.
addCommand :: SymPath -> Int -> CommandCallback -> String -> String -> (String, Binder)
addCommand name arity callback doc example = addCommandConfigurable name (Just arity) callback doc example

addCommandConfigurable :: SymPath -> Maybe Int -> CommandCallback -> String -> String -> (String, Binder)
addCommandConfigurable path maybeArity callback doc example =
  let cmd = XObj (Lst [XObj (Command (CommandFunction f)) (Just dummyInfo) Nothing
                      ,XObj (Sym path Symbol) Nothing Nothing
                      ,unfoldArgs
                      ])
            (Just dummyInfo) (Just DynamicTy)
      SymPath _ name = path
      meta = Meta.set "doc" (XObj (Str docString) Nothing Nothing) emptyMeta
  in (name, Binder meta cmd)
  where f = case maybeArity of
              Just arity -> withArity arity
              Nothing -> callback
        docString = doc ++ "\n\n" ++ exampleUsage
        exampleUsage = "Example Usage:\n```\n" ++ example ++ "\n```\n"
        withArity arity ctx args =
          if length args == arity
            then callback ctx args
            else
              pure (evalError ctx ("Invalid args to '" ++ show path ++ "' command: " ++ joinWithComma (map pretty args) ++ "\n\n" ++ exampleUsage) Nothing)
        unfoldArgs =
          case maybeArity of
            Just arity ->
              let tosym x = (XObj (Sym (SymPath [] x) Symbol) Nothing Nothing)
              in  XObj (Arr (map (tosym . intToArgName) [1..arity])) Nothing Nothing
            Nothing -> XObj (Arr [(XObj (Sym (SymPath [] "") Symbol) Nothing Nothing)]) Nothing Nothing

presentError :: MonadIO m => String -> a -> m a
presentError msg ret =
  liftIO $ do putStrLnWithColor Red msg
              pure ret

-- | Command for changing various project settings.
commandProjectConfig :: CommandCallback
commandProjectConfig ctx [xobj@(XObj (Str key) _ _), value] = do
  let proj = contextProj ctx
      env = contextGlobalEnv ctx
      newProj = case key of
                  "cflag" -> do cflag <- unwrapStringXObj value
                                pure (proj { projectCFlags = addIfNotPresent cflag (projectCFlags proj) })
                  "libflag" -> do libflag <- unwrapStringXObj value
                                  pure (proj { projectLibFlags = addIfNotPresent libflag (projectLibFlags proj) })
                  "pkgconfigflag" -> do pkgconfigflag <- unwrapStringXObj value
                                        pure (proj { projectPkgConfigFlags = addIfNotPresent pkgconfigflag (projectPkgConfigFlags proj) })
                  "cmod" -> do cmod <- unwrapStringXObj value
                               pure (proj { projectCModules = addIfNotPresent cmod (projectCModules proj) })
                  "prompt" -> do prompt <- unwrapStringXObj value
                                 pure (proj { projectPrompt = prompt })
                  "search-path" -> do searchPath <- unwrapStringXObj value
                                      pure (proj { projectCarpSearchPaths = addIfNotPresent searchPath (projectCarpSearchPaths proj) })
                  "print-ast" -> do printAST <- unwrapBoolXObj value
                                    pure (proj { projectPrintTypedAST = printAST })
                  "echo-c" -> do echoC <- unwrapBoolXObj value
                                 pure (proj { projectEchoC = echoC })
                  "echo-compiler-cmd" -> do echoCompilerCmd <- unwrapBoolXObj value
                                            pure (proj { projectEchoCompilationCommand = echoCompilerCmd })
                  "compiler" -> do compiler <- unwrapStringXObj value
                                   pure (proj { projectCompiler = compiler })
                  "target" -> do target <- unwrapStringXObj value
                                 pure (proj { projectTarget = Target target })
                  "title" -> do title <- unwrapStringXObj value
                                pure (proj { projectTitle = title })
                  "output-directory" -> do outDir <- unwrapStringXObj value
                                           pure (proj { projectOutDir = outDir })
                  "docs-directory" -> do docsDir <- unwrapStringXObj value
                                         pure (proj { projectDocsDir = docsDir })
                  "docs-generate-index" ->
                    do docsGenerateIndex <- unwrapBoolXObj value
                       pure (proj { projectDocsGenerateIndex = docsGenerateIndex })
                  "docs-logo" -> do logo <- unwrapStringXObj value
                                    pure (proj { projectDocsLogo = logo })
                  "docs-prelude" -> do prelude <- unwrapStringXObj value
                                       pure (proj { projectDocsPrelude = prelude })
                  "docs-url" -> do url <- unwrapStringXObj value
                                   pure (proj { projectDocsURL = url })
                  "docs-styling" -> do url <- unwrapStringXObj value
                                       pure (proj { projectDocsStyling = url })
                  "file-path-print-length" -> do length <- unwrapStringXObj value
                                                 case length of
                                                   "short" -> pure (proj { projectFilePathPrintLength = ShortPath })
                                                   "full" -> pure (proj { projectFilePathPrintLength = ShortPath })
                                                   _ -> Left ("Project.config can't understand the value '" ++ length ++ "' for key 'file-path-print-length.")
                  "generate-only" -> do generateOnly <- unwrapBoolXObj value
                                        pure (proj { projectGenerateOnly = generateOnly })
                  "paren-balance-hints" ->
                    do balanceHints <- unwrapBoolXObj value
                       pure (proj { projectBalanceHints = balanceHints })
                  "force-reload" -> do forceReload <- unwrapBoolXObj value
                                       pure (proj { projectForceReload = forceReload })
                  _ -> Left ("Project.config can't understand the key '" ++ key ++ "' at " ++ prettyInfoFromXObj xobj ++ ".")
  case newProj of
    Left errorMessage -> presentError ("[CONFIG ERROR] " ++ errorMessage) (ctx, dynamicNil)
    Right ok -> pure (ctx {contextProj=ok}, dynamicNil)
commandProjectConfig ctx [faultyKey, _] =
  presentError ("First argument to 'Project.config' must be a string: " ++ pretty faultyKey) (ctx, dynamicNil)

-- | Command for changing various project settings.
commandProjectGetConfig :: CommandCallback
commandProjectGetConfig ctx [xobj@(XObj (Str key) _ _)] =
  let proj = contextProj ctx
      env = contextGlobalEnv ctx
      xstr s = XObj s (Just dummyInfo) (Just StringTy)
      getVal ctx proj = case key of
          "cflag" -> Right $ Str $ show $ projectCFlags proj
          "libflag" -> Right $ Str $ show $ projectLibFlags proj
          "pkgconfigflag" -> Right $ Arr $ xstr . Str <$> projectPkgConfigFlags proj
          "load-stack" -> Right $ Arr $ xstr . Str <$> projectLoadStack proj
          "prompt" -> Right $ Str $ projectPrompt proj
          "search-path" -> Right $ Str $ show $ projectCarpSearchPaths proj
          "print-ast" -> Right $ Bol $ projectPrintTypedAST proj
          "echo-c" -> Right $ Bol $ projectEchoC proj
          "echo-compiler-cmd" -> Right $ Bol $ projectEchoCompilationCommand proj
          "compiler" -> Right $ Str $ projectCompiler proj
          "target" -> Right $ Str $ show $ projectTarget proj
          "title" -> Right $ Str $ projectTitle proj
          "output-directory" -> Right $ Str $ projectOutDir proj
          "docs-directory" -> Right $ Str $ projectDocsDir proj
          "docs-logo" -> Right $ Str $ projectDocsLogo proj
          "docs-prelude" -> Right $ Str $ projectDocsPrelude proj
          "docs-url" -> Right $ Str $ projectDocsURL proj
          "docs-generate-index" -> Right $ Bol $ projectDocsGenerateIndex proj
          "docs-styling" -> Right $ Str $ projectDocsStyling proj
          "file-path-print-length" -> Right $ Str $ show (projectFilePathPrintLength proj)
          "generate-only" -> Right $ Bol $ projectGenerateOnly proj
          "paren-balance-hints" -> Right $ Bol $ projectBalanceHints proj
          _ -> Left key
  in pure $ case getVal ctx proj of
       Right val -> (ctx, Right $ xstr val)
       Left key -> (evalError ctx ("[CONFIG ERROR] Project.get-config can't understand the key '" ++ key) (info xobj))

commandProjectGetConfig ctx [faultyKey] =
  presentError ("First argument to 'Project.config' must be a string: " ++ pretty faultyKey) (ctx, dynamicNil)

-- | Command for exiting the REPL/compiler
commandQuit :: CommandCallback
commandQuit ctx args =
  do liftIO exitSuccess
     pure (ctx, dynamicNil)

-- | Command for printing the generated C output (in out/main.c)
commandCat :: CommandCallback
commandCat ctx args = do
  let outDir = projectOutDir (contextProj ctx)
      outMain = outDir </> "main.c"
  liftIO $ do callCommand ("cat -n " ++ outMain)
              pure (ctx, dynamicNil)

-- | Command for running the executable generated by the 'build' command.
commandRunExe :: CommandCallback
commandRunExe ctx args = do
  let proj = contextProj ctx
      outDir = projectOutDir proj
      quoted x = "\"" ++ x ++ "\""
      outExe = quoted $ outDir </> projectTitle (contextProj ctx)
  if projectCanExecute proj
    then liftIO $ do handle <- spawnCommand outExe
                     exitCode <- waitForProcess handle
                     case exitCode of
                       ExitSuccess -> pure (ctx, Right (XObj (Num IntTy 0) (Just dummyInfo) (Just IntTy)))
                       ExitFailure i -> throw (ShellOutException ("'" ++ outExe ++ "' exited with return value " ++ show i ++ ".") i)
    else liftIO $ do putStrLnWithColor Red "Can't call the 'run' command, need to build an executable first (requires a 'main' function)."
                     pure (ctx, dynamicNil)

-- | Command for building the project, producing an executable binary or a shared library.
commandBuild :: Bool -> Context -> [XObj] -> IO (Context, Either EvalError XObj)
commandBuild shutUp ctx args = do
  let env = contextGlobalEnv ctx
      typeEnv = contextTypeEnv ctx
      proj = contextProj ctx
      execMode = contextExecMode ctx
      src = do decl <- envToDeclarations typeEnv env
               typeDecl <- envToDeclarations typeEnv (getTypeEnv typeEnv)
               c <- envToC env Functions
               initGlobals <- fmap (wrapInInitFunction (projectCore proj)) (globalsToC env)
               pure ("//Types:\n" ++ typeDecl ++
                       "\n\n//Declarations:\n" ++ decl ++
                       "\n\n//Init globals:\n" ++ initGlobals ++
                       "\n\n//Definitions:\n" ++ c
                      )
  case src of
    Left err ->
      pure (evalError ctx ("I encountered an error when emitting code:\n\n" ++ show err) Nothing)
    Right okSrc ->
      do let compiler = projectCompiler proj
             echoCompilationCommand = projectEchoCompilationCommand proj
             incl = projectIncludesToC proj
             includeCorePath = projectCarpDir proj ++ "/core/ "
             cModules = projectCModules proj
             flags = projectFlags proj
             outDir = projectOutDir proj
             outMain = outDir </> "main.c"
             outExe = outDir </> projectTitle proj
             outLib = outDir </> projectTitle proj
             generateOnly = projectGenerateOnly proj
             compile hasMain =
               do let cmd = joinWithSpace $ [ compiler
                                            , if hasMain then "" else "-shared"
                                            , "-o"
                                            , outExe
                                            , "-I"
                                            , includeCorePath
                                            , flags
                                            , outMain
                                            ] ++ cModules
                    in liftIO $ do when echoCompilationCommand (putStrLn cmd)
                                   callCommand cmd
                                   when (execMode == Repl && not shutUp) $
                                     (putStrLn ("Compiled to '" ++ outExe ++ (if hasMain then "' (executable)" else "' (shared library)")))
                                   pure (setProjectCanExecute hasMain ctx, dynamicNil)
         liftIO $ createDirectoryIfMissing False outDir
         outputHandle <- openFile outMain WriteMode
         hSetEncoding outputHandle utf8
         hPutStr outputHandle (incl ++ okSrc)
         hClose outputHandle
         if generateOnly then pure (ctx, dynamicNil) else
             case Map.lookup "main" (envBindings env) of
               Just _ ->  compile True
               Nothing -> compile False

setProjectCanExecute :: Bool -> Context -> Context
setProjectCanExecute value ctx =
  let proj = contextProj ctx
      proj' = proj { projectCanExecute = value }
  in ctx { contextProj = proj' }

-- | Command for printing all the bindings in the current environment.
commandListBindings :: CommandCallback
commandListBindings ctx args =
  liftIO $ do putStrLn "Types:\n"
              putStrLn (prettyEnvironment (getTypeEnv (contextTypeEnv ctx)))
              putStrLn "\nGlobal environment:\n"
              putStrLn (prettyEnvironment (contextGlobalEnv ctx))
              putStrLn ""
              pure (ctx, dynamicNil)

-- | Command for printing help.
commandHelp :: CommandCallback
commandHelp ctx [XObj (Str "about") _ _] =
  liftIO $ do putStrLn "Carp is an ongoing research project by Erik Svedäng, et al."
              putStrLn ""
              putStrLn "Licensed under the Apache License, Version 2.0 (the \"License\"); \n\
                       \you may not use this file except in compliance with the License. \n\
                       \You may obtain a copy of the License at \n\
                       \http://www.apache.org/licenses/LICENSE-2.0"
              putStrLn ""
              putStrLn "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY \n\
                       \EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE \n\
                       \IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR \n\
                       \PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE \n\
                       \LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR \n\
                       \CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF \n\
                       \SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR \n\
                       \BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, \n\
                       \WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE \n\
                       \OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN\n\
                       \IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
              putStrLn ""
              pure (ctx, dynamicNil)

commandHelp ctx [XObj (Str "language") _ _] =
  liftIO $ do putStrLn "Special forms:"
              putStrLn "(if <condition> <then> <else>)"
              putStrLn "(while <condition> <body>)"
              putStrLn "(do <statement1> <statement2> ... <exprN>)"
              putStrLn "(let [<sym1> <expr1> <name2> <expr2> ...] <body>)"
              putStrLn "(fn [<args>] <body>)"
              putStrLn "(the <type> <expression>)"
              putStrLn "(ref <expression>)"
              putStrLn "(address <expr>)"
              putStrLn "(set! <var> <value>)"
              putStrLn "(break)"
              putStrLn ""
              putStrLn "To use functions in modules without qualifying them:"
              putStrLn "(use <module>)"
              putStrLn "(with <module> <form>)"
              putStrLn ""
              putStrLn ("Valid non-alphanumerics: " ++ validCharacters)
              putStrLn ""
              putStrLn "Number literals:"
              putStrLn "1      Int"
              putStrLn "1l     Long"
              putStrLn "1.0    Double"
              putStrLn "1.0f   Float"
              putStrLn ""
              putStrLn "Reader macros:"
              putStrLn "&<expr>   (ref <expr>)"
              putStrLn "@<expr>   (copy <expr>)"
              putStrLn ""
              pure (ctx, dynamicNil)

commandHelp ctx [XObj (Str "macros") _ _] =
  liftIO $ do putStrLn "To inspect what macros expand to:"
              putStrLn "(expand <quoted expression>)"
              putStrLn ""
              putStrLn "Some useful macros:"
              putStrLn "(cond <condition1> <expr1> ... <else-condition>)"
              putStrLn "(case <expr> <compare-this-1> <expr1> ... <else-condition>)"
              putStrLn "(when <condition> <body>)"
              putStrLn "(for [<var> <from> <to>] <body>)"
              putStrLn "(=> <expr> <func1> <func2> ...)"
              putStrLn "(==> <expr> <func1> <func2> ...)"
              putStrLn ""
              pure (ctx, dynamicNil)

commandHelp ctx [XObj (Str "structs") _ _] =
  liftIO $ do putStrLn "To define a struct without any generic member variables:"
              putStrLn "(deftype <name> [<member> <type>, ...])"
              putStrLn ""
              putStrLn "If you need generic members:"
              putStrLn "(deftype (<name> <type variable 1> ...) [<member> <type>, ...])"
              putStrLn ""
              putStrLn "A type definition will generate the following methods:"
              putStrLn "Getters  (<method-name> (Ref <struct>))"
              putStrLn "Setters  (set-<method-name> <struct> <new-value>)"
              putStrLn "Updaters (update-<method-name> <struct> <new-value>)"
              putStrLn "init (stack allocation)"
              putStrLn "new (heap allocation)"
              putStrLn "copy"
              putStrLn "delete (used internally, no need to call this explicitly)"
              putStrLn ""
              pure (ctx, dynamicNil)

commandHelp ctx [XObj(Str "shortcuts") _ _] =
  liftIO $ do putStrLn "GHC-style shortcuts at the repl:"
              putStrLn "(reload)       :r"
              putStrLn "(build)        :b"
              putStrLn "(run)          :x"
              putStrLn "(cat)          :c"
              putStrLn "(env)          :e"
              putStrLn "(help)         :h"
              putStrLn "(project)      :p"
              putStrLn "(quit)         :q"
              putStrLn "(type <arg>)   :t"
              putStrLn "(expand <arg>) :m"
              putStrLn "(info <arg>)   :i"
              putStrLn ""
              putStrLn "The shortcuts can be combined like this: \":rbx\""
              putStrLn ""
              pure (ctx, dynamicNil)

commandHelp ctx [XObj(Str "interop") _ _] =
  liftIO $ do putStrLn "(register <name> <type>)                      - Make an external variable or function available for usage."
              putStrLn "(register-type <name> [<member> <type>, ...]) - Make an external struct available for usage."
              putStrLn ""
              putStrLn "C-compiler configuration:"
              putStrLn "(system-include <file>)          - Include a system header file."
              putStrLn "(local-include <file>)           - Include a local header file."
              putStrLn "(add-cflag <flag>)               - Add a cflag to the compilation step."
              putStrLn "(add-lib <flag>)                 - Add a library flag to the compilation step."
              putStrLn "(add-c <flag>)                   - Add a C/ObjC/C++ module to the compilation step."
              pure (ctx, dynamicNil)

commandHelp ctx [XObj(Str "project") _ _] =
  liftIO $ do putStrLn "(Project.config <setting> <value>) handles the following settings:"
              putStrLn ""
              putStrLn "'cflag'                - Add a flag to the compiler."
              putStrLn "'libflag'              - Add a library flag to the compiler."
              putStrLn "'pkgconfigflag'        - Add a flag to pkg-config invocations."
              putStrLn "'cmod'                 - Add a C/ObjC/C++ module to the compiler invocation."
              putStrLn "'compiler'             - Set what compiler should be run with the 'build' command."
              putStrLn "'title'                - Set the title of the current project, will affect the name of the binary produced."
              putStrLn "'output-directory'     - Where to put compiler artifacts, etc."
              putStrLn "'docs-directory'       - Where to put generated documentation."
              putStrLn "'docs-logo'            - Location of the documentation logo."
              putStrLn "'docs-prelude'         - The documentation foreword."
              putStrLn "'docs-url'             - A URL for the project (useful for generated documentation)."
              putStrLn "'docs-generate-index'  - Whether to generate the documentation index."
              putStrLn "'docs-styling'         - A URL to CSS for the project documentation."
              putStrLn "'prompt'               - Set the prompt in the repl."
              putStrLn "'search-path'          - Add a path where the Carp compiler will look for '*.carp' files."
              putStrLn ""
              putStrLn "'echo-c'               - When a form is defined using 'def' or 'defn' its C code will be printed."
              putStrLn "'echo-compiler-cmd'    - When building the project the command for running the C compiler will be printed."
              putStrLn "'print-ast'            - The 'info' command will print the AST for a binding."
              putStrLn "'paren-balance-hints'  - Whether to print the ongoing stack of parens to close in the REPL, or not."
              putStrLn "'force-reload'         - If true, the 'load-once' command will work just like 'load' (useful for library developers)."
              putStrLn ""
              pure (ctx, dynamicNil)

commandHelp ctx [] =
  liftIO $ do putStrLn "Compiler commands:"
              putStrLn "(load <file>)      - Load a .carp file, evaluate its content, and add it to the project."
              putStrLn "(reload)           - Reload all the project files."
              putStrLn "(build)            - Produce an executable or shared library."
              putStrLn "(run)              - Run the executable produced by 'build' (if available)."
              putStrLn "(cat)              - Look at the generated C code (make sure you build first)."
              putStrLn "(env)              - List the bindings in the global environment."
              putStrLn "(type <symbol>)    - Get the type of a binding."
              putStrLn "(info <symbol>)    - Get information about a binding."
              putStrLn "(project)          - Display information about your project."
              putStrLn "(quit)             - Terminate this Carp REPL."
              putStrLn "(help <chapter>)   - Available chapters: \"language\", \"macros\", \"structs\", \"interop\", \"shortcuts\", \"project\", \"about\"."
              putStrLn ""
              putStrLn "(Project.config! <setting> <value>) - Change a project setting."
              putStrLn ""
              putStrLn "To define things:"
              putStrLn "(def <name> <constant>)                       - Define a global variable."
              putStrLn "(defn <name> [<args>] <body>)                 - Define a function."
              putStrLn "(definterface <name> <type>)                  - Create an interface for a group of functions sharing the same name."
              putStrLn "(defmodule <name> <def1> <def2> ...)          - Define a module and/or add definitions to an existing one."
              putStrLn "(deftype <name> [<member> <type>, ...])       - Define a new struct type."
              putStrLn ""
              putStrLn "Compiler flags:"
              putStrLn "-b                               - Build."
              putStrLn "-x                               - Build and run."
              putStrLn "--no-core                        - Don't load the core library."
              putStrLn "--log-memory                     - Enables use of memory logging functions in the Debug module."
              putStrLn "--optimize                       - Removes safety checks and runs the C-compiler with the '-O3' flag."
              putStrLn "--check                          - Report all errors found in a machine readable way."
              putStrLn "--generate-only                  - Don't compile the C source."
              pure (ctx, dynamicNil)

commandHelp ctx args =
  pure (evalError ctx ("Invalid args to `help` command: " ++ joinWithComma (map pretty args)) Nothing)

-- | Command for printing information about the current project.
commandProject :: CommandCallback
commandProject ctx args = do
     liftIO (print (contextProj ctx))
     pure (ctx, dynamicNil)

-- | Command for getting the name of the operating system you're on.
commandHostOS :: CommandCallback
commandHostOS ctx _ =
  pure (ctx, (Right (XObj (Str os) (Just dummyInfo) (Just StringTy))))

-- | Command for getting the native architecture.
commandHostArch :: CommandCallback
commandHostArch ctx _ =
  pure (ctx, (Right (XObj (Str arch) (Just dummyInfo) (Just StringTy))))

-- | Command for adding a header file include to the project.
commandAddInclude :: (String -> Includer) -> CommandCallback
commandAddInclude includerConstructor ctx [x] =
  case x of
    XObj (Str file) _ _ -> do
      let proj = contextProj ctx
          includer = includerConstructor file
          includers = projectIncludes proj
          includers' = if includer `elem` includers
                       then includers
                       else includers ++ [includer] -- Add last to preserve include order
          proj' = proj { projectIncludes = includers' }
      pure (ctx { contextProj = proj' }, dynamicNil)
    _ ->
      pure (evalError ctx ("Argument to 'include' must be a string, but was `" ++ pretty x ++ "`") (info x))

commandAddSystemInclude = commandAddInclude SystemInclude

commandAddRelativeInclude :: CommandCallback
commandAddRelativeInclude ctx [x] =
  case x of
    XObj (Str file) i@(Just info) t ->
        let compiledFile = infoFile info
        in commandAddInclude RelativeInclude ctx [
          XObj (Str $ takeDirectory compiledFile </> file) i t
        ]
    _ ->
      pure (evalError ctx ("Argument to 'include' must be a string, but was `" ++ pretty x ++ "`") (info x))

commandIsList :: CommandCallback
commandIsList ctx [x] =
  pure $ case x of
    XObj (Lst _) _ _ -> (ctx, Right trueXObj)
    _ -> (ctx, Right falseXObj)

commandIsArray :: CommandCallback
commandIsArray ctx [x] =
  pure $ case x of
    XObj (Arr _) _ _ -> (ctx, Right trueXObj)
    _ -> (ctx, Right falseXObj)

commandIsSymbol :: CommandCallback
commandIsSymbol ctx [x] =
  pure $ case x of
    XObj (Sym _ _) _ _ -> (ctx, Right trueXObj)
    _ -> (ctx, Right falseXObj)

commandArray :: CommandCallback
commandArray ctx args =
  pure (ctx, Right (XObj (Arr args) (Just dummyInfo) Nothing))

commandList :: CommandCallback
commandList ctx args =
  pure (ctx, Right (XObj (Lst args) (Just dummyInfo) Nothing))

commandLength :: CommandCallback
commandLength ctx [x] =
  pure $ case x of
    XObj (Lst lst) _ _ ->
      (ctx, (Right (XObj (Num IntTy (Integral (length lst))) Nothing Nothing)))
    XObj (Arr arr) _ _ ->
      (ctx, (Right (XObj (Num IntTy (Integral (length arr))) Nothing Nothing)))
    _ -> evalError ctx ("Applying 'length' to non-list: " ++ pretty x) (info x)

commandCar :: CommandCallback
commandCar ctx [x] =
  pure $ case x of
    XObj (Lst (car : _)) _ _ -> (ctx, Right car)
    XObj (Arr (car : _)) _ _ -> (ctx, Right car)
    _ -> evalError ctx ("Applying 'car' to non-list: " ++ pretty x) (info x)

commandCdr :: CommandCallback
commandCdr ctx [x] =
  pure $ case x of
    XObj (Lst (_ : cdr)) i _ -> (ctx, Right (XObj (Lst cdr) i Nothing))
    XObj (Arr (_ : cdr)) i _ -> (ctx, Right (XObj (Arr cdr) i Nothing))
    _ -> evalError ctx "Applying 'cdr' to non-list or empty list" (info x)

commandLast :: CommandCallback
commandLast ctx [x] =
  pure $ case x of
    XObj (Lst lst@(x:xs)) _ _ -> (ctx, Right (last lst))
    XObj (Arr arr@(x:xs)) _ _ -> (ctx, Right (last arr))
    _ -> evalError ctx "Applying 'last' to non-list or empty list." (info x)

commandAllButLast :: CommandCallback
commandAllButLast ctx [x] =
  pure $ case x of
    XObj (Lst lst) i _ -> (ctx, Right (XObj (Lst (init lst)) i Nothing))
    XObj (Arr arr) i _ -> (ctx, Right (XObj (Arr (init arr)) i Nothing))
    _ -> evalError ctx "Applying 'all-but-last' to non-list or empty list." (info x)

commandCons :: CommandCallback
commandCons ctx [x, xs] =
  pure $ case xs of
    XObj (Lst lst) _ _ ->
      (ctx, Right (XObj (Lst (x : lst)) (info x) (ty x))) -- TODO: probably not correct to just copy 'i' and 't'?
    XObj (Arr arr) _ _ -> (ctx, Right (XObj (Arr (x : arr)) (info x) (ty x)))
    _ -> evalError ctx "Applying 'cons' to non-list or empty list." (info xs)

commandConsLast :: CommandCallback
commandConsLast ctx [x, xs] =
  pure $ case xs of
    XObj (Lst lst) i t ->
      (ctx, Right (XObj (Lst (lst ++ [x])) i t)) -- TODO: should they get their own i:s and t:s
    _ -> evalError ctx "Applying 'cons-last' to non-list or empty list." (info xs)

commandAppend :: CommandCallback
commandAppend ctx [xs, ys] =
  pure $ case (xs, ys) of
    (XObj (Lst lst1) i t, XObj (Lst lst2) _ _) ->
      (ctx, Right (XObj (Lst (lst1 ++ lst2)) i t)) -- TODO: should they get their own i:s and t:s
    (XObj (Arr arr1) i t, XObj (Arr arr2) _ _) -> (ctx, Right (XObj (Arr (arr1 ++ arr2)) i t))
    _ -> evalError ctx "Applying 'append' to non-array/list or empty list." (info xs)

commandMacroError :: CommandCallback
commandMacroError ctx [msg] =
  pure $ case msg of
    XObj (Str smsg) _ _ -> evalError ctx smsg (info msg)
    x                  -> evalError ctx (pretty x) (info msg)

commandMacroLog :: CommandCallback
commandMacroLog ctx msgs = do
  liftIO (mapM_ (putStr . logify) msgs)
  liftIO (putStr "\n")
  pure (ctx, dynamicNil)
  where logify msg =
          case msg of
            XObj (Str msg) _ _ -> msg
            x                  -> pretty x

commandEq :: CommandCallback
commandEq ctx [a, b] =
  pure $ case cmp (a, b) of
    Left (a, b) -> evalError ctx ("Can't compare " ++ pretty a ++ " with " ++ pretty b) (info a)
    Right b -> (ctx, Right (boolToXObj b))
  where
    cmp (XObj (Num aTy aNum) _ _, XObj (Num bTy bNum) _ _) | aTy == bTy =
      Right $ aNum == bNum
    cmp (XObj (Str sa) _ _, XObj (Str sb) _ _) = Right $ sa == sb
    cmp (XObj (Chr ca) _ _, XObj (Chr cb) _ _) = Right $ ca == cb
    cmp (XObj (Sym sa _) _ _, XObj (Sym sb _) _ _) = Right $ sa == sb
    cmp (XObj (Bol xa) _ _, XObj (Bol xb) _ _) = Right $ xa == xb
    cmp (XObj Def _ _, XObj Def _ _) = Right $ True
    cmp (XObj Do _ _, XObj Do _ _) = Right $ True
    cmp (XObj Let _ _, XObj Let _ _) = Right $ True
    cmp (XObj While _ _, XObj While _ _) = Right $ True
    cmp (XObj Break _ _, XObj Break _ _) = Right $ True
    cmp (XObj If _ _, XObj If _ _) = Right $ True
    cmp (XObj With _ _, XObj With _ _) = Right $ True
    cmp (XObj DocStub _ _, XObj DocStub _ _) = Right $ True
    cmp (XObj Address _ _, XObj Address _ _) = Right $ True
    cmp (XObj SetBang _ _, XObj SetBang _ _) = Right $ True
    cmp (XObj Macro _ _, XObj Macro _ _) = Right $ True
    cmp (XObj Dynamic _ _, XObj Dynamic _ _) = Right $ True
    cmp (XObj DefDynamic _ _, XObj DefDynamic _ _) = Right $ True
    cmp (XObj The _ _, XObj The _ _) = Right $ True
    cmp (XObj Ref _ _, XObj Ref _ _) = Right $ True
    cmp (XObj Deref _ _, XObj Deref _ _) = Right $ True
    cmp (XObj (Lst []) _ _, XObj (Lst []) _ _) = Right True
    cmp (XObj (Lst elemsA) _ _, XObj (Lst elemsB) _ _) =
      if length elemsA == length elemsB
      then foldr cmp' (Right True) (zip elemsA elemsB)
      else Right False
    cmp (XObj (Arr []) _ _, XObj (Arr []) _ _) = Right True
    cmp (XObj (Arr elemsA) _ _, XObj (Arr elemsB) _ _) =
      if length elemsA == length elemsB
      then foldr cmp' (Right True) (zip elemsA elemsB)
      else Right False
    cmp invalid = Left invalid
    cmp' _ invalid@(Left _) = invalid
    cmp' _ (Right False) = Right False
    cmp' elem (Right True) = cmp elem

commandComp :: (Number -> Number -> Bool) -> String -> CommandCallback
commandComp op opname ctx [XObj (Num aTy aNum) _ _, XObj (Num bTy bNum) _ _] | aTy == bTy = pure $ (ctx, Right (boolToXObj (op aNum bNum)))
commandComp _ opname ctx [a, b] = pure $ evalError ctx ("Can't compare (" ++ opname ++ ") " ++ pretty a ++ " with " ++ pretty b) (info a)


commandLt :: CommandCallback
commandLt = commandComp (<) "<"

commandGt :: CommandCallback
commandGt = commandComp (>) ">"

commandCharAt :: CommandCallback
commandCharAt ctx [a, b] =
  pure $ case (a, b) of
    (XObj (Str s) _ _, XObj (Num IntTy (Integral i)) _ _) ->
      if length s > i
      then (ctx, Right (XObj (Chr (s !! i)) (Just dummyInfo) (Just IntTy)))
      else evalError ctx ("Can't call char-at with " ++ pretty a ++ " and " ++ show i ++ ", index too large") (info a)
    _ -> evalError ctx ("Can't call char-at with " ++ pretty a ++ " and " ++ pretty b) (info a)

commandIndexOf :: CommandCallback
commandIndexOf ctx [a, b] =
  pure $ case (a, b) of
    (XObj (Str s) _ _, XObj (Chr c) _ _) ->
      (ctx, Right (XObj (Num IntTy (Integral (getIdx c s))) (Just dummyInfo) (Just IntTy)))
    _ -> evalError ctx ("Can't call index-of with " ++ pretty a ++ " and " ++ pretty b) (info a)
  where getIdx c s = fromMaybe (-1) $ elemIndex c s

commandSubstring :: CommandCallback
commandSubstring ctx [a, b, c] =
  pure $ case (a, b, c) of
    (XObj (Str s) _ _, XObj (Num IntTy (Integral f)) _ _, XObj (Num IntTy (Integral t)) _ _) ->
      (ctx, Right (XObj (Str (take t (drop f s))) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call substring with " ++ pretty a ++ ", " ++ pretty b ++ " and " ++ pretty c) (info a)

commandStringLength :: CommandCallback
commandStringLength ctx [a] =
  pure $ case a of
    XObj (Str s) _ _ ->
      (ctx, Right (XObj (Num IntTy (Integral (length s))) (Just dummyInfo) (Just IntTy)))
    _ -> evalError ctx ("Can't call length with " ++ pretty a) (info a)

commandStringConcat :: CommandCallback
commandStringConcat ctx [a] =
  pure $ case a of
    XObj (Arr strings) _ _ ->
      case mapM unwrapStringXObj strings of
        Left err -> evalError ctx err (info a)
        Right result -> (ctx, Right (XObj (Str (join result)) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call concat with " ++ pretty a) (info a)

commandStringSplitOn :: CommandCallback
commandStringSplitOn ctx [XObj (Str sep) _ _, XObj (Str s) _ _] =
  pure $ (ctx, Right (XObj (Arr (xstr <$> splitOn sep s)) (Just dummyInfo) Nothing))
  where xstr o = XObj (Str o) (Just dummyInfo) (Just StringTy)
commandStringSplitOn ctx [sep, s] =
  pure $ evalError ctx ("Can't call split-on with " ++ pretty sep ++ ", " ++ pretty s) (info sep)

commandSymConcat :: CommandCallback
commandSymConcat ctx [a] =
  pure $ case a of
    XObj (Arr syms) _ _ ->
      case mapM unwrapSymPathXObj syms of
        Left err -> evalError ctx err (info a)
        Right result -> (ctx, Right (XObj (Sym (SymPath [] (join (map show result))) (LookupGlobal CarpLand AVariable)) (Just dummyInfo) Nothing))
    _ -> evalError ctx ("Can't call concat with " ++ pretty a) (info a)

commandSymPrefix :: CommandCallback
commandSymPrefix ctx [XObj (Sym (SymPath [] prefix) _) _ _, XObj (Sym (SymPath [] suffix) _) i t] =
  pure $ (ctx, Right (XObj (Sym (SymPath [prefix] suffix) (LookupGlobal CarpLand AVariable)) i t))
commandSymPrefix ctx [x, XObj (Sym (SymPath [] _) _) _ _] =
  pure $ evalError ctx ("Can’t call `prefix` with " ++ pretty x) (info x)
commandSymPrefix ctx [_, x] =
  pure $ evalError ctx ("Can’t call `prefix` with " ++ pretty x) (info x)

commandSymFrom :: CommandCallback
commandSymFrom ctx [x@(XObj (Sym _ _) _ _)] = pure (ctx, Right x)
commandSymFrom ctx [XObj (Str s) i t] = pure (ctx, Right $ XObj (sFrom_ s) i t)
commandSymFrom ctx [XObj (Pattern s) i t] = pure (ctx, Right $ XObj (sFrom_ s) i t)
commandSymFrom ctx [XObj (Chr c) i t] = pure (ctx, Right $ XObj (sFrom_ (show c)) i t)
commandSymFrom ctx [XObj n@(Num _ v) i t] = pure (ctx, Right $ XObj (sFrom_ (show v)) i t)
commandSymFrom ctx [XObj (Bol b) i t] = pure (ctx, Right $ XObj (sFrom_ (show b)) i t)
commandSymFrom ctx [x] =
  pure $ evalError ctx ("Can’t call `from` with " ++ pretty x) (info x)

commandSymStr :: CommandCallback
commandSymStr ctx [XObj (Sym s _) i _] =
  pure (ctx, Right $ XObj (Str (show s)) i (Just StringTy))
commandSymStr ctx [x] =
  pure $ evalError ctx ("Can’t call `str` with " ++ pretty x) (info x)

sFrom_ s = Sym (SymPath [] s) (LookupGlobal CarpLand AVariable)

commandPathDirectory :: CommandCallback
commandPathDirectory ctx [a] =
  pure $ case a of
    XObj (Str s) _ _ ->
      (ctx, Right (XObj (Str (takeDirectory s)) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call `directory` with " ++ pretty a) (info a)

commandPathAbsolute :: CommandCallback
commandPathAbsolute ctx [a] =
  case a of
    XObj (Str s) _ _ -> do
      abs <- makeAbsolute s
      pure $ (ctx, Right (XObj (Str abs) (Just dummyInfo) (Just StringTy)))
    _ -> pure $ evalError ctx ("Can't call `absolute` with " ++ pretty a) (info a)


commandArith :: (Number -> Number -> Number) -> String -> CommandCallback
commandArith op opname ctx [XObj (Num aTy aNum) _ _, XObj (Num bTy bNum) _ _] | aTy == bTy =
  pure $ (ctx, Right (XObj (Num aTy (op aNum bNum)) (Just dummyInfo) (Just aTy)))
commandArith _ opname ctx [a, b] = pure $ evalError ctx ("Can't call " ++ opname ++ " with " ++ pretty a ++ " and " ++ pretty b) (info a)

commandPlus :: CommandCallback
commandPlus = commandArith (+) "+"

commandMinus :: CommandCallback
commandMinus = commandArith (-) "-"

commandDiv :: CommandCallback
commandDiv ctx p@[XObj (Num _ (Integral _)) _ _, XObj (Num _ (Integral _)) _ _] = commandArith div "/" ctx p
commandDiv ctx p@[XObj (Num _ (Floating _)) _ _, XObj (Num _ (Floating _)) _ _] = commandArith (/) "/" ctx p
commandDiv ctx p = commandArith (error "div") "/" ctx p

commandMul :: CommandCallback
commandMul = commandArith (*) "*"

commandStr :: CommandCallback
commandStr ctx xs =
  pure (ctx, Right (XObj (Str (join (map f xs))) (Just dummyInfo) (Just StringTy)))
  -- | TODO: Is there a better function to call here than some exceptions + 'pretty'?
  where f (XObj (Str s) _ _) = s
        f (XObj (Sym path mode) _ _) = show path
        f x = escape $ pretty x
        escape [] = []
        escape ('\\':y) = "\\\\" ++ escape y
        escape (x:y) = x : escape y

commandNot :: CommandCallback
commandNot ctx [x] =
  pure $ case x of
    XObj (Bol ab) _ _ -> (ctx, Right (boolToXObj (not ab)))
    _ -> evalError ctx ("Can't perform logical operation (not) on " ++ pretty x) (info x)

commandReadFile :: CommandCallback
commandReadFile ctx [filename] =
  case filename of
    XObj (Str fname) _ _ -> do
         exceptional <- liftIO ((try $ slurp fname) :: (IO (Either IOException String)))
         pure $ case exceptional of
            Right contents -> (ctx, Right (XObj (Str contents) (Just dummyInfo) (Just StringTy)))
            Left _ -> (evalError ctx ("The argument to `read-file` `" ++ fname ++ "` does not exist") (info filename))
    _ -> pure (evalError ctx ("The argument to `read-file` must be a string, I got `" ++ pretty filename ++ "`") (info filename))

commandWriteFile :: CommandCallback
commandWriteFile ctx [filename, contents] =
  case filename of
    XObj (Str fname) _ _ ->
      case contents of
        XObj (Str s) _ _ -> do
         exceptional <- liftIO ((try $ writeFile fname s) :: (IO (Either IOException ())))
         pure $ case exceptional of
            Right () -> (ctx, dynamicNil)
            Left _ -> evalError ctx ("Cannot write to argument to `" ++ fname ++ "`, an argument to `write-file`") (info filename)
        _ -> pure (evalError ctx ("The second argument to `write-file` must be a string, I got `" ++ pretty contents ++ "`") (info contents))
    _ -> pure (evalError ctx ("The first argument to `write-file` must be a string, I got `" ++ pretty filename ++ "`") (info filename))

commandHostBitWidth :: CommandCallback
commandHostBitWidth ctx [] =
  let bitSize = Integral (finiteBitSize (undefined :: Int))
  in pure (ctx, Right (XObj (Num IntTy bitSize) (Just dummyInfo) (Just IntTy)))

commandSaveDocsInternal :: CommandCallback
commandSaveDocsInternal ctx [modulePath] = do
     let globalEnv = contextGlobalEnv ctx
     case modulePath of
       XObj (Lst xobjs) _ _ ->
         case mapM unwrapSymPathXObj xobjs of
           Left err -> pure (evalError ctx err (info modulePath))
           Right okPaths ->
             case mapM (getEnvironmentBinderForDocumentation ctx globalEnv) okPaths of
               Left err -> pure (evalError ctx err (info modulePath))
               Right okEnvBinders -> saveDocs ctx (zip okPaths okEnvBinders)
       x ->
         pure (evalError ctx ("Invalid arg to save-docs-internal (expected list of symbols): " ++ pretty x) (info modulePath))
  where getEnvironmentBinderForDocumentation :: Context -> Env -> SymPath -> Either String Binder
        getEnvironmentBinderForDocumentation ctx env path =
          case lookupInEnv path env of
            Just (_, foundBinder@(Binder _ (XObj (Mod foundEnv) _ _))) ->
              Right foundBinder
            Just (_, Binder _ x) ->
              Left ("I can’t generate documentation for `" ++ pretty x ++ "` because it isn’t a module")
            Nothing ->
              Left ("I can’t find the module `" ++ show path ++ "`")

saveDocs :: Context -> [(SymPath, Binder)] -> IO (Context, Either a XObj)
saveDocs ctx pathsAndEnvBinders = do
     liftIO (saveDocsForEnvs (contextProj ctx) pathsAndEnvBinders)
     pure (ctx, dynamicNil)

commandSexpression :: CommandCallback
commandSexpression ctx [xobj, (XObj (Bol b) _ _)] =
  commandSexpressionInternal ctx [xobj] b
commandSexpression ctx [xobj] =
  commandSexpressionInternal ctx [xobj] False
commandSexpression ctx xobj =
  pure $ evalError ctx ("s-expr expects a symbol argument and an optional bool, but got: " ++ unwords (map pretty xobj)) (Just dummyInfo)

commandSexpressionInternal :: Context -> [XObj] -> Bool -> IO (Context, Either EvalError XObj)
commandSexpressionInternal ctx [xobj] bol =
  let env = contextGlobalEnv ctx
      tyEnv = getTypeEnv $ contextTypeEnv ctx
  in case xobj of
       (XObj (Lst [inter@(XObj (Interface ty _) _ _), path]) i t) ->
         pure (ctx, Right (XObj (Lst [(toSymbols inter), path, (reify ty)]) i t))
       (XObj (Lst forms) i t) ->
         pure (ctx, Right (XObj (Lst (map toSymbols forms)) i t))
       mod@(XObj (Mod e) i t) ->
         if bol
         then getMod
         else
           case lookupInEnv (SymPath [] (fromMaybe "" (envModuleName e))) tyEnv of
             Just (_, Binder _ (XObj (Lst forms) i t)) ->
               pure (ctx, Right (XObj (Lst (map toSymbols forms)) i t))
             Just (_, Binder _ xobj') ->
               pure (ctx, Right (toSymbols xobj'))
             Nothing ->
               getMod
         where getMod =
                 case (toSymbols mod) of
                   x@(XObj (Lst xs) i t) ->
                     bindingSyms e (ctx, Right x)
                 where bindingSyms env start =
                         (mapM (\x -> commandSexpression ctx [x]) $
                         map snd $
                         Map.toList $ Map.map binderXObj (envBindings env))
                         >>= pure . foldl combine start
                       combine (c, (Right (XObj (Lst xs) i t))) (_ , (Right y@(XObj (Lst ys) _ _))) =
                         (c, Right (XObj (Lst (xs ++ [y])) i t))
                       combine _ (c, (Left err)) =
                         (c, Left err)
                       combine (c, Left err) _ =
                         (c, Left err)
       _ ->
         pure $ evalError ctx ("can't get an s-expression for: " ++ pretty xobj ++ " is it a bound symbol or literal s-expression?") (Just dummyInfo)

toSymbols :: XObj -> XObj
toSymbols (XObj (Mod e) i t) =
  (XObj (Lst [XObj (Sym (SymPath [] "defmodule") Symbol) i t,
              XObj (Sym (SymPath [] (fromMaybe "" (envModuleName e))) Symbol) i t]) i t)
toSymbols (XObj (Defn _) i t) = (XObj (Sym (SymPath [] "defn") Symbol) i t)
toSymbols (XObj Def i t) = (XObj (Sym (SymPath [] "def") Symbol) i t)
toSymbols (XObj (Deftype _) i t) = (XObj (Sym (SymPath [] "deftype") Symbol) i t)
toSymbols (XObj (DefSumtype _) i t) = (XObj (Sym (SymPath [] "deftype") Symbol) i t)
toSymbols (XObj (Interface _ _) i t) = (XObj (Sym (SymPath [] "definterface") Symbol) i t)
toSymbols (XObj Macro i t) = (XObj (Sym (SymPath [] "defmacro") Symbol) i t)
toSymbols (XObj (Command _) i t) = (XObj (Sym (SymPath [] "command") Symbol) i t)
toSymbols (XObj (Primitive _) i t) = (XObj (Sym (SymPath [] "primitive") Symbol) i t)
toSymbols (XObj (External _) i t) = (XObj (Sym (SymPath [] "external") Symbol) i t)
toSymbols x = x
