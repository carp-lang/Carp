module Commands where

import Control.Exception
import Control.Monad (join, when, foldM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Bits (finiteBitSize)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(..))
import System.Info (os)
import System.Process (callCommand, spawnCommand, waitForProcess)
import System.IO (openFile, hPutStr, hClose, utf8, hSetEncoding, IOMode(..))
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

-- | Use this function to register commands in the environment.
addCommand :: SymPath -> Int -> CommandCallback -> String -> String -> (String, Binder)
addCommand name arity callback doc example = addCommandConfigurable name (Just arity) callback doc example

addCommandConfigurable :: SymPath -> Maybe Int -> CommandCallback -> String -> String -> (String, Binder)
addCommandConfigurable path maybeArity callback doc example =
  let cmd = XObj (Lst [XObj (Command (CommandFunction f)) (Just dummyInfo) Nothing
                      ,XObj (Sym path Symbol) Nothing Nothing
                      ])
            (Just dummyInfo) (Just DynamicTy)
      SymPath _ name = path
      meta = MetaData (Map.insert "doc" (XObj (Str docString) Nothing Nothing) Map.empty)
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
              return (evalError ctx ("Invalid args to '" ++ show path ++ "' command: " ++ joinWithComma (map pretty args) ++ "\n\n" ++ exampleUsage) Nothing)

presentError :: MonadIO m => String -> a -> m a
presentError msg ret =
  liftIO $ do putStrLnWithColor Red msg
              return ret

-- | Command for changing various project settings.
commandProjectConfig :: CommandCallback
commandProjectConfig ctx [xobj@(XObj (Str key) _ _), value] = do
  let proj = contextProj ctx
      env = contextGlobalEnv ctx
      newProj = case key of
                  "cflag" -> do cflag <- unwrapStringXObj value
                                return (proj { projectCFlags = addIfNotPresent cflag (projectCFlags proj) })
                  "libflag" -> do libflag <- unwrapStringXObj value
                                  return (proj { projectLibFlags = addIfNotPresent libflag (projectLibFlags proj) })
                  "pkgconfigflag" -> do pkgconfigflag <- unwrapStringXObj value
                                        return (proj { projectPkgConfigFlags = addIfNotPresent pkgconfigflag (projectPkgConfigFlags proj) })
                  "prompt" -> do prompt <- unwrapStringXObj value
                                 return (proj { projectPrompt = prompt })
                  "search-path" -> do searchPath <- unwrapStringXObj value
                                      return (proj { projectCarpSearchPaths = addIfNotPresent searchPath (projectCarpSearchPaths proj) })
                  "print-ast" -> do printAST <- unwrapBoolXObj value
                                    return (proj { projectPrintTypedAST = printAST })
                  "echo-c" -> do echoC <- unwrapBoolXObj value
                                 return (proj { projectEchoC = echoC })
                  "echo-compiler-cmd" -> do echoCompilerCmd <- unwrapBoolXObj value
                                            return (proj { projectEchoCompilationCommand = echoCompilerCmd })
                  "compiler" -> do compiler <- unwrapStringXObj value
                                   return (proj { projectCompiler = compiler })
                  "title" -> do title <- unwrapStringXObj value
                                return (proj { projectTitle = title })
                  "output-directory" -> do outDir <- unwrapStringXObj value
                                           return (proj { projectOutDir = outDir })
                  "docs-directory" -> do docsDir <- unwrapStringXObj value
                                         return (proj { projectDocsDir = docsDir })
                  "docs-generate-index" ->
                    do docsGenerateIndex <- unwrapBoolXObj value
                       return (proj { projectDocsGenerateIndex = docsGenerateIndex })
                  "docs-logo" -> do logo <- unwrapStringXObj value
                                    return (proj { projectDocsLogo = logo })
                  "docs-prelude" -> do prelude <- unwrapStringXObj value
                                       return (proj { projectDocsPrelude = prelude })
                  "docs-url" -> do url <- unwrapStringXObj value
                                   return (proj { projectDocsURL = url })
                  "docs-styling" -> do url <- unwrapStringXObj value
                                       return (proj { projectDocsStyling = url })
                  "file-path-print-length" -> do length <- unwrapStringXObj value
                                                 case length of
                                                   "short" -> return (proj { projectFilePathPrintLength = ShortPath })
                                                   "full" -> return (proj { projectFilePathPrintLength = ShortPath })
                                                   _ -> Left ("Project.config can't understand the value '" ++ length ++ "' for key 'file-path-print-length.")
                  "generate-only" -> do generateOnly <- unwrapBoolXObj value
                                        return (proj { projectGenerateOnly = generateOnly })
                  "paren-balance-hints" ->
                    do balanceHints <- unwrapBoolXObj value
                       return (proj { projectBalanceHints = balanceHints })
                  "force-reload" -> do forceReload <- unwrapBoolXObj value
                                       return (proj { projectForceReload = forceReload })
                  _ -> Left ("Project.config can't understand the key '" ++ key ++ "' at " ++ prettyInfoFromXObj xobj ++ ".")
  case newProj of
    Left errorMessage -> presentError ("[CONFIG ERROR] " ++ errorMessage) (ctx, dynamicNil)
    Right ok -> return (ctx {contextProj=ok}, dynamicNil)
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
          "prompt" -> Right $ Str $ projectPrompt proj
          "search-path" -> Right $ Str $ show $ projectCarpSearchPaths proj
          "print-ast" -> Right $ Bol $ projectPrintTypedAST proj
          "echo-c" -> Right $ Bol $ projectEchoC proj
          "echo-compiler-cmd" -> Right $ Bol $ projectEchoCompilationCommand proj
          "compiler" -> Right $ Str $ projectCompiler proj
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
  in case getVal ctx proj of
       Right val -> return (ctx, Right $ xstr val)
       Left key -> return (evalError ctx ("[CONFIG ERROR] Project.get-config can't understand the key '" ++ key) (info xobj))

commandProjectGetConfig ctx [faultyKey] =
  presentError ("First argument to 'Project.config' must be a string: " ++ pretty faultyKey) (ctx, dynamicNil)

-- | Command for exiting the REPL/compiler
commandQuit :: CommandCallback
commandQuit ctx args =
  do liftIO exitSuccess
     return (ctx, dynamicNil)

-- | Command for printing the generated C output (in out/main.c)
commandCat :: CommandCallback
commandCat ctx args = do
  let outDir = projectOutDir (contextProj ctx)
      outMain = outDir </> "main.c"
  liftIO $ do callCommand ("cat -n " ++ outMain)
              return (ctx, dynamicNil)

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
                       ExitSuccess -> return (ctx, Right (XObj (Num IntTy 0) (Just dummyInfo) (Just IntTy)))
                       ExitFailure i -> throw (ShellOutException ("'" ++ outExe ++ "' exited with return value " ++ show i ++ ".") i)
    else liftIO $ do putStrLnWithColor Red "Can't call the 'run' command, need to build an executable first (requires a 'main' function)."
                     return (ctx, dynamicNil)

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
               return ("//Types:\n" ++ typeDecl ++
                       "\n\n//Declarations:\n" ++ decl ++
                       "\n\n//Init globals:\n" ++ initGlobals ++
                       "\n\n//Definitions:\n" ++ c
                      )
  case src of
    Left err ->
      return (evalError ctx ("I encountered an error when emitting code:\n\n" ++ show err) Nothing)
    Right okSrc ->
      do let compiler = projectCompiler proj
             echoCompilationCommand = projectEchoCompilationCommand proj
             incl = projectIncludesToC proj
             includeCorePath = " -I" ++ projectCarpDir proj ++ "/core/ "
             flags = includeCorePath ++ projectFlags proj
             outDir = projectOutDir proj
             outMain = outDir </> "main.c"
             outExe = outDir </> projectTitle proj
             outLib = outDir </> projectTitle proj
             generateOnly = projectGenerateOnly proj
         liftIO $ createDirectoryIfMissing False outDir
         outputHandle <- openFile outMain WriteMode
         hSetEncoding outputHandle utf8
         hPutStr outputHandle (incl ++ okSrc)
         hClose outputHandle
         if generateOnly then return (ctx, dynamicNil) else
             case Map.lookup "main" (envBindings env) of
                             Just _ -> do let cmd = compiler ++ " " ++ outMain ++ " -o \"" ++ outExe ++ "\" " ++ flags
                                          liftIO $ do when echoCompilationCommand (putStrLn cmd)
                                                      callCommand cmd
                                                      when (execMode == Repl && not shutUp) (putStrLn ("Compiled to '" ++ outExe ++ "' (executable)"))
                                          return (setProjectCanExecute True ctx, dynamicNil)
                             Nothing -> do let cmd = compiler ++ " " ++ outMain ++ " -shared -o \"" ++ outLib ++ "\" " ++ flags
                                           liftIO $ do when echoCompilationCommand (putStrLn cmd)
                                                       callCommand cmd
                                                       when (execMode == Repl && not shutUp) (putStrLn ("Compiled to '" ++ outLib ++ "' (shared library)"))
                                           return (setProjectCanExecute False ctx, dynamicNil)

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
              return (ctx, dynamicNil)

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
              return (ctx, dynamicNil)

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
              return (ctx, dynamicNil)

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
              return (ctx, dynamicNil)

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
              return (ctx, dynamicNil)

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
              return (ctx, dynamicNil)

commandHelp ctx [XObj(Str "interop") _ _] =
  liftIO $ do putStrLn "(register <name> <type>)                      - Make an external variable or function available for usage."
              putStrLn "(register-type <name> [<member> <type>, ...]) - Make an external struct available for usage."
              putStrLn ""
              putStrLn "C-compiler configuration:"
              putStrLn "(system-include <file>)          - Include a system header file."
              putStrLn "(local-include <file>)           - Include a local header file."
              putStrLn "(add-cflag <flag>)               - Add a cflag to the compilation step."
              putStrLn "(add-lib <flag>)                 - Add a library flag to the compilation step."
              return (ctx, dynamicNil)

commandHelp ctx [XObj(Str "project") _ _] =
  liftIO $ do putStrLn "(Project.config <setting> <value>) handles the following settings:"
              putStrLn ""
              putStrLn "'cflag'                - Add a flag to the compiler."
              putStrLn "'libflag'              - Add a library flag to the compiler."
              putStrLn "'pkgconfigflag'        - Add a flag to pkg-config invocations."
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
              return (ctx, dynamicNil)

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
              return (ctx, dynamicNil)

commandHelp ctx args =
  return (evalError ctx ("Invalid args to `help` command: " ++ joinWithComma (map pretty args)) Nothing)

-- | Command for printing information about the current project.
commandProject :: CommandCallback
commandProject ctx args = do
     liftIO (print (contextProj ctx))
     return (ctx, dynamicNil)

-- | Command for getting the name of the operating system you're on.
commandOS :: CommandCallback
commandOS ctx _ =
  return (ctx, (Right (XObj (Str os) (Just dummyInfo) (Just StringTy))))

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
      return (ctx { contextProj = proj' }, dynamicNil)
    _ ->
      return (evalError ctx ("Argument to 'include' must be a string, but was `" ++ pretty x ++ "`") (info x))

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
      return (evalError ctx ("Argument to 'include' must be a string, but was `" ++ pretty x ++ "`") (info x))

commandIsList :: CommandCallback
commandIsList ctx [x] =
  case x of
    XObj (Lst _) _ _ -> return (ctx, Right trueXObj)
    _ -> return (ctx, Right falseXObj)

commandIsArray :: CommandCallback
commandIsArray ctx [x] =
  case x of
    XObj (Arr _) _ _ -> return (ctx, Right trueXObj)
    _ -> return (ctx, Right falseXObj)

commandIsSymbol :: CommandCallback
commandIsSymbol ctx [x] =
  case x of
    XObj (Sym _ _) _ _ -> return (ctx, Right trueXObj)
    _ -> return (ctx, Right falseXObj)

commandArray :: CommandCallback
commandArray ctx args =
  return (ctx, Right (XObj (Arr args) (Just dummyInfo) Nothing))

commandList :: CommandCallback
commandList ctx args =
  return (ctx, Right (XObj (Lst args) (Just dummyInfo) Nothing))

commandLength :: CommandCallback
commandLength ctx [x] =
  case x of
    XObj (Lst lst) _ _ ->
      return (ctx, (Right (XObj (Num IntTy (fromIntegral (length lst))) Nothing Nothing)))
    XObj (Arr arr) _ _ ->
      return (ctx, (Right (XObj (Num IntTy (fromIntegral (length arr))) Nothing Nothing)))
    _ ->
      return (evalError ctx ("Applying 'length' to non-list: " ++ pretty x) (info x))

commandCar :: CommandCallback
commandCar ctx [x] =
  case x of
    XObj (Lst (car : _)) _ _ -> return (ctx, Right car)
    XObj (Arr (car : _)) _ _ -> return (ctx, Right car)
    _ ->
      return (evalError ctx ("Applying 'car' to non-list: " ++ pretty x) (info x))

commandCdr :: CommandCallback
commandCdr ctx [x] =
  case x of
    XObj (Lst (_ : cdr)) i _ -> return (ctx, Right (XObj (Lst cdr) i Nothing))
    XObj (Arr (_ : cdr)) i _ -> return (ctx, Right (XObj (Arr cdr) i Nothing))
    _ ->
      return (evalError ctx "Applying 'cdr' to non-list or empty list" (info x))

commandLast :: CommandCallback
commandLast ctx [x] =
  case x of
    XObj (Lst lst) _ _ -> return (ctx, Right (last lst))
    XObj (Arr arr) _ _ -> return (ctx, Right (last arr))
    _ ->
      return (evalError ctx "Applying 'last' to non-list or empty list." (info x))

commandAllButLast :: CommandCallback
commandAllButLast ctx [x] =
  case x of
    XObj (Lst lst) i _ -> return (ctx, Right (XObj (Lst (init lst)) i Nothing))
    XObj (Arr arr) i _ -> return (ctx, Right (XObj (Arr (init arr)) i Nothing))
    _ ->
      return (evalError ctx "Applying 'all-but-last' to non-list or empty list." (info x))

commandCons :: CommandCallback
commandCons ctx [x, xs] =
  case xs of
    XObj (Lst lst) _ _ ->
      return (ctx, Right (XObj (Lst (x : lst)) (info x) (ty x))) -- TODO: probably not correct to just copy 'i' and 't'?
    XObj (Arr arr) _ _ ->
      return (ctx, Right (XObj (Arr (x : arr)) (info x) (ty x)))
    _ ->
      return (evalError ctx "Applying 'cons' to non-list or empty list." (info xs))

commandConsLast :: CommandCallback
commandConsLast ctx [x, xs] =
  case xs of
    XObj (Lst lst) i t ->
      return (ctx, Right (XObj (Lst (lst ++ [x])) i t)) -- TODO: should they get their own i:s and t:s
    _ ->
      return (evalError ctx "Applying 'cons-last' to non-list or empty list." (info xs))

commandAppend :: CommandCallback
commandAppend ctx [xs, ys] =
  case (xs, ys) of
    (XObj (Lst lst1) i t, XObj (Lst lst2) _ _) ->
      return (ctx, Right (XObj (Lst (lst1 ++ lst2)) i t)) -- TODO: should they get their own i:s and t:s
    (XObj (Arr arr1) i t, XObj (Arr arr2) _ _) ->
      return (ctx, Right (XObj (Arr (arr1 ++ arr2)) i t))
    _ ->
      return (evalError ctx "Applying 'append' to non-array/list or empty list." (info xs))

commandMacroError :: CommandCallback
commandMacroError ctx [msg] =
  case msg of
    XObj (Str smsg) _ _ -> return (evalError ctx smsg (info msg))
    x                  -> return (evalError ctx (pretty x) (info msg))

commandMacroLog :: CommandCallback
commandMacroLog ctx msgs = do
  liftIO (mapM_ (putStr . logify) msgs)
  liftIO (putStr "\n")
  return (ctx, dynamicNil)
  where logify msg =
          case msg of
            XObj (Str msg) _ _ -> msg
            x                  -> pretty x

commandEq :: CommandCallback
commandEq ctx [a, b] =
  return $ case (a, b) of
    (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
      if (round aNum :: Int) == (round bNum :: Int)
      then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Num LongTy aNum) _ _, XObj (Num LongTy bNum) _ _) ->
      if (round aNum :: Int) == (round bNum :: Int)
      then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Num FloatTy aNum) _ _, XObj (Num floatTy bNum) _ _) ->
      if aNum == bNum
      then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Num DoubleTy aNum) _ _, XObj (Num DoubleTy bNum) _ _) ->
      if aNum == bNum
      then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Str sa) _ _, XObj (Str sb) _ _) ->
      if sa == sb then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Chr ca) _ _, XObj (Chr cb) _ _) ->
      if ca == cb then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Sym sa _) _ _, XObj (Sym sb _) _ _) ->
      if sa == sb then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Bol xa) _ _, XObj (Bol xb) _ _) ->
      if xa == xb then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Lst []) _ _, XObj (Lst []) _ _) ->
      (ctx, Right trueXObj)
    _ -> evalError ctx ("Can't compare " ++ pretty a ++ " with " ++ pretty b) (info a)

commandLt :: CommandCallback
commandLt ctx [a, b] =
 return $ case (a, b) of
   (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
     if (round aNum :: Int) < (round bNum :: Int)
     then (ctx, Right trueXObj) else (ctx, Right falseXObj)
   (XObj (Num LongTy aNum) _ _, XObj (Num LongTy bNum) _ _) ->
     if (round aNum :: Int) < (round bNum :: Int)
     then (ctx, Right trueXObj) else (ctx, Right falseXObj)
   (XObj (Num FloatTy aNum) _ _, XObj (Num floatTy bNum) _ _) ->
     if aNum < bNum
     then (ctx, Right trueXObj) else (ctx, Right falseXObj)
   (XObj (Num DoubleTy aNum) _ _, XObj (Num DoubleTy bNum) _ _) ->
     if aNum < bNum
     then (ctx, Right trueXObj) else (ctx, Right falseXObj)
   _ -> evalError ctx ("Can't compare (<) " ++ pretty a ++ " with " ++ pretty b) (info a)

commandGt :: CommandCallback
commandGt ctx [a, b] =
  return $ case (a, b) of
    (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
      if (round aNum :: Int) > (round bNum :: Int)
      then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Num LongTy aNum) _ _, XObj (Num LongTy bNum) _ _) ->
      if (round aNum :: Int) > (round bNum :: Int)
      then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Num FloatTy aNum) _ _, XObj (Num floatTy bNum) _ _) ->
      if aNum > bNum
      then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    (XObj (Num DoubleTy aNum) _ _, XObj (Num DoubleTy bNum) _ _) ->
      if aNum > bNum
      then (ctx, Right trueXObj) else (ctx, Right falseXObj)
    _ -> evalError ctx ("Can't compare (>) " ++ pretty a ++ " with " ++ pretty b) (info a)

commandCharAt :: CommandCallback
commandCharAt ctx [a, b] =
  return $ case (a, b) of
    (XObj (Str s) _ _, XObj (Num IntTy n) _ _) ->
      let i = (round n :: Int)
      in if length s > i
         then (ctx, Right (XObj (Chr (s !! i)) (Just dummyInfo) (Just IntTy)))
         else evalError ctx ("Can't call char-at with " ++ pretty a ++ " and " ++ show i ++ ", index too large") (info a)
    _ -> evalError ctx ("Can't call char-at with " ++ pretty a ++ " and " ++ pretty b) (info a)

commandIndexOf :: CommandCallback
commandIndexOf ctx [a, b] =
  return $ case (a, b) of
    (XObj (Str s) _ _, XObj (Chr c) _ _) ->
      (ctx, Right (XObj (Num IntTy (getIdx c s)) (Just dummyInfo) (Just IntTy)))
    _ -> evalError ctx ("Can't call index-of with " ++ pretty a ++ " and " ++ pretty b) (info a)
  where getIdx c s = fromIntegral $ fromMaybe (-1) $ elemIndex c s

commandSubstring :: CommandCallback
commandSubstring ctx [a, b, c] =
  return $ case (a, b, c) of
    (XObj (Str s) _ _, XObj (Num IntTy f) _ _, XObj (Num IntTy t) _ _) ->
      (ctx, Right (XObj (Str (take (round t :: Int) (drop (round f :: Int) s))) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call substring with " ++ pretty a ++ ", " ++ pretty b ++ " and " ++ pretty c) (info a)

commandStringLength :: CommandCallback
commandStringLength ctx [a] =
  return $ case a of
    XObj (Str s) _ _ ->
      (ctx, Right (XObj (Num IntTy (fromIntegral (length s))) (Just dummyInfo) (Just IntTy)))
    _ -> evalError ctx ("Can't call length with " ++ pretty a) (info a)

commandStringConcat :: CommandCallback
commandStringConcat ctx [a] =
  return $ case a of
    XObj (Arr strings) _ _ ->
      case mapM unwrapStringXObj strings of
        Left err -> evalError ctx err (info a)
        Right result -> (ctx, Right (XObj (Str (join result)) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call concat with " ++ pretty a) (info a)

commandSymConcat :: CommandCallback
commandSymConcat ctx [a] =
  return $ case a of
    XObj (Arr syms) _ _ ->
      case mapM unwrapSymPathXObj syms of
        Left err -> evalError ctx err (info a)
        Right result -> (ctx, Right (XObj (Sym (SymPath [] (join (map show result))) (LookupGlobal CarpLand AVariable)) (Just dummyInfo) Nothing))
    _ -> evalError ctx ("Can't call concat with " ++ pretty a) (info a)

commandSymPrefix :: CommandCallback
commandSymPrefix ctx [XObj (Sym (SymPath [] prefix) _) _ _, XObj (Sym (SymPath [] suffix) _) i t] =
  return $ (ctx, Right (XObj (Sym (SymPath [prefix] suffix) (LookupGlobal CarpLand AVariable)) i t))
commandSymPrefix ctx [x, XObj (Sym (SymPath [] _) _) _ _] =
  return $ evalError ctx ("Can’t call `prefix` with " ++ pretty x) (info x)
commandSymPrefix ctx [_, x] =
  return $ evalError ctx ("Can’t call `prefix` with " ++ pretty x) (info x)

commandSymFrom :: CommandCallback
commandSymFrom ctx [x@(XObj (Sym _ _) _ _)] = return (ctx, Right x)
commandSymFrom ctx [XObj (Str s) i t] = return (ctx, Right $ XObj (sFrom_ s) i t)
commandSymFrom ctx [XObj (Pattern s) i t] = return (ctx, Right $ XObj (sFrom_ s) i t)
commandSymFrom ctx [XObj (Chr c) i t] = return (ctx, Right $ XObj (sFrom_ (show c)) i t)
commandSymFrom ctx [XObj n@(Num _ _) i t] =
  return (ctx, Right $ XObj (sFrom_ (simpleFromNum n)) i t)
commandSymFrom ctx [XObj (Bol b) i t] = return (ctx, Right $ XObj (sFrom_ (show b)) i t)
commandSymFrom ctx [x] =
  return $ evalError ctx ("Can’t call `from` with " ++ pretty x) (info x)

commandSymStr :: CommandCallback
commandSymStr ctx [XObj (Sym s _) i _] =
  return (ctx, Right $ XObj (Str (show s)) i (Just StringTy))
commandSymStr ctx [x] =
  return $ evalError ctx ("Can’t call `str` with " ++ pretty x) (info x)

sFrom_ s = Sym (SymPath [] s) (LookupGlobal CarpLand AVariable)

simpleFromNum (Num IntTy num) = show (round num :: Int)
simpleFromNum (Num LongTy num) = show (round num :: Int)
simpleFromNum (Num _ num) = show num

commandStringDirectory :: CommandCallback
commandStringDirectory ctx [a] =
  return $ case a of
    XObj (Str s) _ _ ->
      (ctx, Right (XObj (Str (takeDirectory s)) (Just dummyInfo) (Just StringTy)))
    _ -> evalError ctx ("Can't call `directory` with " ++ pretty a) (info a)

commandPlus :: CommandCallback
commandPlus ctx [a, b] =
  return $ case (a, b) of
    (XObj (Num aty aNum) _ _, XObj (Num bty bNum) _ _) ->
      if aty == bty
      then (ctx, Right (XObj (Num aty (aNum + bNum)) (Just dummyInfo) (Just aty)))
      else evalError ctx ("Can't call + with " ++ pretty a ++ " and " ++ pretty b) (info a)
    _ -> evalError ctx ("Can't call + with " ++ pretty a ++ " and " ++ pretty b) (info a)

commandMinus :: CommandCallback
commandMinus ctx [a, b] =
  return $ case (a, b) of
    (XObj (Num aty aNum) _ _, XObj (Num bty bNum) _ _) ->
      if aty == bty
      then (ctx, Right (XObj (Num aty (aNum - bNum)) (Just dummyInfo) (Just aty)))
      else evalError ctx ("Can't call - with " ++ pretty a ++ " and " ++ pretty b) (info a)
    _ -> evalError ctx ("Can't call - with " ++ pretty a ++ " and " ++ pretty b) (info a)

commandDiv :: CommandCallback
commandDiv ctx [a, b] =
  return $ case (a, b) of
    (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
      (ctx, Right (XObj (Num IntTy (fromIntegral (quot (round aNum ::Int) (round bNum :: Int)))) (Just dummyInfo) (Just IntTy)))
    (XObj (Num aty aNum) _ _, XObj (Num bty bNum) _ _) ->
      if aty == bty
      then (ctx, Right (XObj (Num aty (aNum / bNum)) (Just dummyInfo) (Just aty)))
      else evalError ctx ("Can't call / with " ++ pretty a ++ " and " ++ pretty b) (info a)
    _ -> evalError ctx ("Can't call / with " ++ pretty a ++ " and " ++ pretty b) (info a)

commandMul :: CommandCallback
commandMul ctx [a, b] =
  return $ case (a, b) of
    (XObj (Num aty aNum) _ _, XObj (Num bty bNum) _ _) ->
      if aty == bty
      then (ctx, Right (XObj (Num aty (aNum * bNum)) (Just dummyInfo) (Just aty)))
      else evalError ctx ("Can't call * with " ++ pretty a ++ " and " ++ pretty b) (info a)
    _ -> evalError ctx ("Can't call * with " ++ pretty a ++ " and " ++ pretty b) (info a)

commandStr :: CommandCallback
commandStr ctx xs =
  return (ctx, Right (XObj (Str (join (map f xs))) (Just dummyInfo) (Just StringTy)))
  -- | TODO: Is there a better function to call here than some exceptions + 'pretty'?
  where f (XObj (Str s) _ _) = s
        f (XObj (Sym path mode) _ _) = show path
        f x = escape $ pretty x
        escape [] = []
        escape ('\\':y) = "\\\\" ++ escape y
        escape (x:y) = x : escape y

commandNot :: CommandCallback
commandNot ctx [x] =
  case x of
    XObj (Bol ab) _ _ ->
      if ab
      then return (ctx, Right falseXObj)
      else return (ctx, Right trueXObj)
    _ ->
      return (evalError ctx ("Can't perform logical operation (not) on " ++ pretty x) (info x))

commandReadFile :: CommandCallback
commandReadFile ctx [filename] =
  case filename of
    XObj (Str fname) _ _ -> do
         exceptional <- liftIO ((try $ slurp fname) :: (IO (Either IOException String)))
         case exceptional of
            Right contents ->
              return (ctx, Right (XObj (Str contents) (Just dummyInfo) (Just StringTy)))
            Left _ ->
              return (evalError ctx ("The argument to `read-file` `" ++ fname ++ "` does not exist") (info filename))
    _ ->
      return (evalError ctx ("The argument to `read-file` must be a string, I got `" ++ pretty filename ++ "`") (info filename))

commandWriteFile :: CommandCallback
commandWriteFile ctx [filename, contents] =
  case filename of
    XObj (Str fname) _ _ ->
      case contents of
        XObj (Str s) _ _ -> do
         exceptional <- liftIO ((try $ writeFile fname s) :: (IO (Either IOException ())))
         case exceptional of
            Right () -> return (ctx, dynamicNil)
            Left _ ->
              return (evalError ctx ("Cannot write to argument to `" ++ fname ++ "`, an argument to `write-file`") (info filename))
        _ ->
          return (evalError ctx ("The second argument to `write-file` must be a string, I got `" ++ pretty contents ++ "`") (info contents))
    _ ->
      return (evalError ctx ("The first argument to `write-file` must be a string, I got `" ++ pretty filename ++ "`") (info filename))

commandBitWidth :: CommandCallback
commandBitWidth ctx [] =
  let bitSize = fromIntegral (finiteBitSize (undefined :: Int))
  in return (ctx, Right (XObj (Num IntTy bitSize) (Just dummyInfo) (Just IntTy)))

commandSaveDocsInternal :: CommandCallback
commandSaveDocsInternal ctx [modulePath] = do
     let globalEnv = contextGlobalEnv ctx
     case modulePath of
       XObj (Lst xobjs) _ _ ->
         case mapM unwrapSymPathXObj xobjs of
           Left err -> return (evalError ctx err (info modulePath))
           Right okPaths ->
             case mapM (getEnvironmentBinderForDocumentation ctx globalEnv) okPaths of
               Left err -> return (evalError ctx err (info modulePath))
               Right okEnvBinders -> saveDocs ctx (zip okPaths okEnvBinders)
       x ->
         return (evalError ctx ("Invalid arg to save-docs-internal (expected list of symbols): " ++ pretty x) (info modulePath))
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
     return (ctx, dynamicNil)

commandSexpression :: CommandCallback
commandSexpression ctx [xobj, (XObj (Bol b) _ _)] =
  commandSexpressionInternal ctx [xobj] b
commandSexpression ctx [xobj] =
  commandSexpressionInternal ctx [xobj] False
commandSexpression ctx xobj =
  return $ evalError ctx ("s-expr expects a symbol argument and an optional bool, but got: " ++ unwords (map pretty xobj)) (Just dummyInfo)

commandSexpressionInternal :: Context -> [XObj] -> Bool -> IO (Context, Either EvalError XObj)
commandSexpressionInternal ctx [xobj] bol =
  let env = contextGlobalEnv ctx
      tyEnv = getTypeEnv $ contextTypeEnv ctx
  in case xobj of
       (XObj (Lst [inter@(XObj (Interface ty _) _ _), path]) i t) ->
         return (ctx, Right (XObj (Lst [(toSymbols inter), path, (tyToXObj ty)]) i t))
       (XObj (Lst forms) i t) ->
         return (ctx, Right (XObj (Lst (map toSymbols forms)) i t))
       mod@(XObj (Mod e) i t) ->
         if bol
         then getMod
         else
           case lookupInEnv (SymPath [] (fromMaybe "" (envModuleName e))) tyEnv of
             Just (_, Binder _ (XObj (Lst forms) i t)) ->
               return (ctx, Right (XObj (Lst (map toSymbols forms)) i t))
             Just (_, Binder _ xobj') ->
               return (ctx, Right (toSymbols xobj'))
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
                         >>= return . foldl combine start
                       combine (c, (Right (XObj (Lst xs) i t))) (_ , (Right y@(XObj (Lst ys) _ _))) =
                         (c, Right (XObj (Lst (xs ++ [y])) i t))
                       combine _ (c, (Left err)) =
                         (c, Left err)
                       combine (c, Left err) _ =
                         (c, Left err)
       _ ->
         return $ evalError ctx ("can't get an s-expression for: " ++ pretty xobj ++ " is it a bound symbol or literal s-expression?") (Just dummyInfo)

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
toSymbols x = x
