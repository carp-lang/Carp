module Commands where

import Parsing
import Emit
import Obj
import Types
import Infer
import Deftype
import ColorText
import Template
import Util
import Lookup
import RenderDocs

import System.Directory
import System.Info (os)
import Control.Monad.State
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO, modify, get, put)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(..))
import System.FilePath (takeDirectory)
import qualified Data.Map as Map
import System.Process (callCommand, spawnCommand, waitForProcess)
import Control.Exception

type CommandCallback = [XObj] -> StateT Context IO (Either EvalError XObj)

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
addCommand :: String -> Int -> CommandCallback -> (String, Binder)
addCommand name arity callback =
  addCommandConfigurable name (Just arity) callback

addCommandConfigurable :: String -> Maybe Int -> CommandCallback -> (String, Binder)
addCommandConfigurable name maybeArity callback =
  let path = SymPath [] name
      cmd = XObj (Lst [XObj (Command (CommandFunction f)) (Just dummyInfo) Nothing
                      ,XObj (Sym path Symbol) Nothing Nothing
                      ])
            (Just dummyInfo) (Just DynamicTy)
  in (name, Binder emptyMeta cmd)
  where f = case maybeArity of
              Just arity -> withArity arity
              Nothing -> withoutArity
        withArity arity args =
          if length args == arity
            then callback args
            else return (Left (EvalError ("Invalid args to '" ++ name ++ "' command: " ++ joinWithComma (map pretty args))))
        withoutArity args =
          callback args

-- | DEPRECATED Command for changing various project settings.
commandProjectSet :: CommandCallback
commandProjectSet [XObj (Str key) _ _, value] =
  do ctx <- get
     let proj = contextProj ctx
         env = contextGlobalEnv ctx
     case value of
       XObj (Str valueStr) _ _ -> do
          newCtx <- case key of
                      "cflag" -> return ctx { contextProj = proj { projectCFlags = addIfNotPresent valueStr (projectCFlags proj) } }
                      "libflag" -> return ctx { contextProj = proj { projectLibFlags = addIfNotPresent valueStr (projectLibFlags proj) } }
                      "prompt" -> return ctx { contextProj = proj { projectPrompt = valueStr } }
                      "search-path" -> return ctx { contextProj = proj { projectCarpSearchPaths = addIfNotPresent valueStr (projectCarpSearchPaths proj) } }
                      "printAST" -> return ctx { contextProj = proj { projectPrintTypedAST = (valueStr == "true") } }
                      "echoC" -> return ctx { contextProj = proj { projectEchoC = (valueStr == "true") } }
                      "echoCompilationCommand" -> return ctx { contextProj = proj { projectEchoCompilationCommand = (valueStr == "true") } }
                      "compiler" -> return ctx { contextProj = proj { projectCompiler = valueStr } }
                      "title"    -> return ctx { contextProj = proj { projectTitle = valueStr } }
                      _ -> presentError ("Unrecognized key: '" ++ key ++ "'") ctx
          put newCtx
          return dynamicNil
       val -> presentError "Argument to project-set! must be a string" dynamicNil

presentError :: MonadIO m => String -> a -> m a
presentError msg ret =
  liftIO $ do putStrLnWithColor Red msg
              return ret

-- | Command for changing various project settings.
commandProjectConfig :: CommandCallback
commandProjectConfig [xobj@(XObj (Str key) _ _), value] =
  do ctx <- get
     let proj = contextProj ctx
         env = contextGlobalEnv ctx
         newProj = case key of
                     "cflag" -> do cflag <- unwrapStringXObj value
                                   return (proj { projectCFlags = addIfNotPresent cflag (projectCFlags proj) })
                     "libflag" -> do libflag <- unwrapStringXObj value
                                     return (proj { projectLibFlags = addIfNotPresent libflag (projectLibFlags proj) })
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
                     _ -> Left ("Project.config can't understand the key '" ++ key ++ "' at " ++ prettyInfoFromXObj xobj ++ ".")
     case newProj of
       Left errorMessage -> presentError ("[CONFIG ERROR] " ++ errorMessage) dynamicNil
       Right ok -> do put (ctx { contextProj = ok })
                      return dynamicNil
commandProjectConfig [faultyKey, _] =
  do presentError ("First argument to 'Project.config' must be a string: " ++ pretty faultyKey) dynamicNil

-- | Command for exiting the REPL/compiler
commandQuit :: CommandCallback
commandQuit args =
  do liftIO exitSuccess
     return dynamicNil

-- | Command for printing the generated C output (in out/main.c)
commandCat :: CommandCallback
commandCat args =
  do ctx <- get
     let outDir = projectOutDir (contextProj ctx)
         outMain = outDir ++ "/" ++ "main.c"
     liftIO $ do callCommand ("cat -n " ++ outMain)
                 return dynamicNil

-- | Command for running the executable generated by the 'build' command.
commandRunExe :: CommandCallback
commandRunExe args =
  do ctx <- get
     let outDir = projectOutDir (contextProj ctx)
         outExe = "\"" ++ outDir ++ "/" ++ projectTitle (contextProj ctx) ++ "\""
     liftIO $ do handle <- spawnCommand outExe
                 exitCode <- waitForProcess handle
                 case exitCode of
                   ExitSuccess -> return (Right (XObj (Num IntTy 0) (Just dummyInfo) (Just IntTy)))
                   ExitFailure i -> throw (ShellOutException ("'" ++ outExe ++ "' exited with return value " ++ show i ++ ".") i)

-- | Command for building the project, producing an executable binary or a shared library.
commandBuild :: CommandCallback
commandBuild args =
  do ctx <- get
     let env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         proj = contextProj ctx
         execMode = contextExecMode ctx
         src = do decl <- envToDeclarations typeEnv env
                  typeDecl <- envToDeclarations typeEnv (getTypeEnv typeEnv)
                  c <- envToC env Functions
                  initGlobals <- fmap wrapInInitFunction (globalsToC env)
                  return ("//Types:\n" ++ typeDecl ++
                          "\n\n//Declarations:\n" ++ decl ++
                          "\n\n//Init globals:\n" ++ initGlobals ++
                          "\n\n//Definitions:\n" ++ c
                         )
     case src of
       Left err ->
         return (Left (EvalError ("[CODEGEN ERROR] " ++ show err)))
       Right okSrc ->
         liftIO $ do let compiler = projectCompiler proj
                         echoCompilationCommand = projectEchoCompilationCommand proj
                         incl = projectIncludesToC proj
                         includeCorePath = " -I" ++ projectCarpDir proj ++ "/core/ "
                         switches = " -g "
                         flags = projectFlags proj ++ includeCorePath ++ switches
                         outDir = projectOutDir proj ++ "/"
                         outMain = outDir ++ "main.c"
                         outExe = outDir ++ projectTitle proj
                         outLib = outDir ++ projectTitle proj
                     createDirectoryIfMissing False outDir
                     writeFile outMain (incl ++ okSrc)
                     case Map.lookup "main" (envBindings env) of
                       Just _ -> do let cmd = compiler ++ " " ++ outMain ++ " -o \"" ++ outExe ++ "\" " ++ flags
                                    when echoCompilationCommand (putStrLn cmd)
                                    callCommand cmd
                                    when (execMode == Repl) (putStrLn ("Compiled to '" ++ outExe ++ "' (executable)"))
                                    return dynamicNil
                       Nothing -> do let cmd = compiler ++ " " ++ outMain ++ " -shared -o \"" ++ outLib ++ "\" " ++ flags
                                     when echoCompilationCommand (putStrLn cmd)
                                     callCommand cmd
                                     when (execMode == Repl) (putStrLn ("Compiled to '" ++ outLib ++ "' (shared library)"))
                                     return dynamicNil

-- | Command for printing all the bindings in the current environment.
commandListBindings :: CommandCallback
commandListBindings args =
  do ctx <- get
     liftIO $ do putStrLn "Types:\n"
                 putStrLn (prettyEnvironment (getTypeEnv (contextTypeEnv ctx)))
                 putStrLn "\nGlobal environment:\n"
                 putStrLn (prettyEnvironment (contextGlobalEnv ctx))
                 putStrLn ""
                 return dynamicNil

-- | Command for printing help.
commandHelp :: CommandCallback

commandHelp [XObj (Str "about") _ _] =
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
              return dynamicNil

commandHelp [XObj (Str "language") _ _] =
  liftIO $ do putStrLn "Special forms:"
              putStrLn "(if <condition> <then> <else>)"
              putStrLn "(while <condition> <body>)"
              putStrLn "(do <statement1> <statement2> ... <exprN>)"
              putStrLn "(let [<sym1> <expr1> <name2> <expr2> ...] <body>)"
              --putStrLn "(fn [<args>] <body>)"
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
              return dynamicNil

commandHelp [XObj (Str "macros") _ _] =
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
              return dynamicNil

commandHelp [XObj (Str "structs") _ _] =
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
              return dynamicNil

commandHelp [XObj(Str "shortcuts") _ _] =
  liftIO $ do putStrLn "GHC-style shortcuts at the repl:"
              putStrLn "(reload)   :r"
              putStrLn "(build)    :b"
              putStrLn "(run)      :x"
              putStrLn "(cat)      :c"
              putStrLn "(env)      :e"
              putStrLn "(help)     :h"
              putStrLn "(project)  :p"
              putStrLn "(quit)     :q"
              putStrLn ""
              putStrLn "The shortcuts can be combined like this: \":rbx\""
              putStrLn ""
              return dynamicNil

commandHelp [XObj(Str "interop") _ _] =
  liftIO $ do putStrLn "(register <name> <type>)                      - Make an external variable or function available for usage."
              putStrLn "(register-type <name> [<member> <type>, ...]) - Make an external struct available for usage."
              putStrLn ""
              putStrLn "C-compiler configuration:"
              putStrLn "(system-include <file>)          - Include a system header file."
              putStrLn "(local-include <file>)           - Include a local header file."
              putStrLn "(add-cflag <flag>)               - Add a cflag to the compilation step."
              putStrLn "(add-lib <flag>)                 - Add a library flag to the compilation step."
              return dynamicNil

commandHelp [XObj(Str "project") _ _] =
  liftIO $ do putStrLn "(Project.config <setting> <value>) handles the following settings:"
              putStrLn ""
              putStrLn "'cflag'              - Add a flag to the compiler."
              putStrLn "'libflag'            - Add a library flag to the compiler."
              putStrLn "'compiler'           - Set what compiler should be run with the 'build' command."
              putStrLn "'title'              - Set the title of the current project, will affect the name of the binary produced."
              putStrLn "'prompt'             - Set the prompt in the repl."
              putStrLn "'search-path'        - Add a path where the Carp compiler will look for '*.carp' files."
              putStrLn ""
              putStrLn "'echo-c'             - When a form is defined using 'def' or 'defn' its C code will be printed."
              putStrLn "'echo-compiler-cmd'  - When building the project the command for running the C compiler will be printed."
              putStrLn "'print-ast'          - The 'info' command will print the AST for a binding."
              putStrLn ""
              return dynamicNil

commandHelp [] =
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
              return dynamicNil

commandHelp args =
  do liftIO $ putStrLn ("Can't find help for " ++ joinWithComma (map pretty args))
     return dynamicNil

-- | Command for printing information about the current project.
commandProject :: CommandCallback
commandProject args =
  do ctx <- get
     liftIO (print (contextProj ctx))
     return dynamicNil

-- | Command for printing a message to the screen.
commandPrint :: CommandCallback
commandPrint args =
  do liftIO $ mapM_ (putStrLn . pretty) args
     return dynamicNil

-- | Command for getting the name of the operating system you're on.
commandOS :: CommandCallback
commandOS _ =
  return (Right (XObj (Str os) (Just dummyInfo) (Just StringTy)))

-- | Command for adding a header file include to the project.
commandAddInclude :: (String -> Includer) -> CommandCallback
commandAddInclude includerConstructor [XObj (Str file) _ _] =
  do ctx <- get
     let proj = contextProj ctx
         includer = includerConstructor file
         includers = projectIncludes proj
         includers' = if includer `elem` includers
                      then includers
                      else includer : includers
         proj' = proj { projectIncludes = includers' }
     put (ctx { contextProj = proj' })
     return dynamicNil

commandAddSystemInclude = commandAddInclude SystemInclude
commandAddLocalInclude  = commandAddInclude LocalInclude

commandIsList :: CommandCallback
commandIsList [x] =
  case x of
    XObj (Lst _) _ _ -> return (Right trueXObj)
    _ -> return (Right falseXObj)

commandIsArray :: CommandCallback
commandIsArray [x] =
  case x of
    XObj (Arr _) _ _ -> return (Right trueXObj)
    _ -> return (Right falseXObj)

commandIsSymbol :: CommandCallback
commandIsSymbol [x] =
  case x of
    XObj (Sym _ _) _ _ -> return (Right trueXObj)
    _ -> return (Right falseXObj)

commandCount :: CommandCallback
commandCount [x] =
  case x of
    XObj (Lst lst) _ _ -> return (Right (XObj (Num IntTy (fromIntegral (length lst))) Nothing Nothing))
    XObj (Arr arr) _ _ -> return (Right (XObj (Num IntTy (fromIntegral (length arr))) Nothing Nothing))
    _ -> return (Left (EvalError ("Applying 'count' to non-list: " ++ pretty x ++ " at " ++ prettyInfoFromXObj x)))

commandCar :: CommandCallback
commandCar [x] =
  case x of
    XObj (Lst (car : _)) _ _ -> return (Right car)
    XObj (Arr (car : _)) _ _ -> return (Right car)
    _ -> return (Left (EvalError ("Applying 'car' to non-list: " ++ pretty x)))

commandCdr :: CommandCallback
commandCdr [x] =
  case x of
    XObj (Lst (_ : cdr)) _ _ -> return (Right (XObj (Lst cdr) Nothing Nothing))
    XObj (Arr (_ : cdr)) _ _ -> return (Right (XObj (Arr cdr) Nothing Nothing))
    _ -> return (Left (EvalError "Applying 'cdr' to non-list or empty list"))

commandLast :: CommandCallback
commandLast [x] =
  case x of
    XObj (Lst lst) _ _ -> return (Right (last lst))
    XObj (Arr arr) _ _ -> return (Right (last arr))
    _ -> return (Left (EvalError "Applying 'last' to non-list or empty list."))

commandAllButLast :: CommandCallback
commandAllButLast [x] =
  case x of
    XObj (Lst lst) _ _ -> return (Right (XObj (Lst (init lst)) Nothing Nothing))
    XObj (Arr arr) _ _ -> return (Right (XObj (Arr (init arr)) Nothing Nothing))
    _ -> return (Left (EvalError "Applying 'all-but-last' to non-list or empty list."))

commandCons :: CommandCallback
commandCons [x, xs] =
  case xs of
    XObj (Lst lst) _ _ -> return (Right (XObj (Lst (x : lst)) (info x) (ty x))) -- TODO: probably not correct to just copy 'i' and 't'?
    XObj (Arr arr) _ _ -> return (Right (XObj (Arr (x : arr)) (info x) (ty x)))
    _ -> return (Left (EvalError "Applying 'cons' to non-list or empty list."))

commandConsLast :: CommandCallback
commandConsLast [x, xs] =
  case xs of
    XObj (Lst lst) i t -> return (Right (XObj (Lst (lst ++ [x])) i t)) -- TODO: should they get their own i:s and t:s
    _ -> return (Left (EvalError "Applying 'cons-last' to non-list or empty list."))

commandAppend :: CommandCallback
commandAppend [xs, ys] =
  case (xs, ys) of
    (XObj (Lst lst1) i t, XObj (Lst lst2) _ _) ->
      return (Right (XObj (Lst (lst1 ++ lst2)) i t)) -- TODO: should they get their own i:s and t:s
    _ ->
      return (Left (EvalError "Applying 'append' to non-list or empty list."))

commandMacroError :: CommandCallback
commandMacroError [msg] =
  case msg of
    XObj (Str msg) _ _ -> return (Left (EvalError msg))
    x                  -> return (Left (EvalError (pretty x)))

commandMacroLog :: CommandCallback
commandMacroLog [msg] =
  case msg of
    XObj (Str msg) _ _ -> do liftIO (putStrLn msg)
                             return dynamicNil
    x                  -> do liftIO (putStrLn (pretty x))
                             return dynamicNil

commandEq :: CommandCallback
commandEq [a, b] =
  return $ case (a, b) of
    (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
      if (round aNum :: Int) == (round bNum :: Int)
      then Right trueXObj else Right falseXObj
    (XObj (Num LongTy aNum) _ _, XObj (Num LongTy bNum) _ _) ->
      if (round aNum :: Int) == (round bNum :: Int)
      then Right trueXObj else Right falseXObj
    (XObj (Num FloatTy aNum) _ _, XObj (Num floatTy bNum) _ _) ->
      if aNum == bNum
      then Right trueXObj else Right falseXObj
    (XObj (Num DoubleTy aNum) _ _, XObj (Num DoubleTy bNum) _ _) ->
      if aNum == bNum
      then Right trueXObj else Right falseXObj
    (XObj (Str sa) _ _, XObj (Str sb) _ _) ->
      if sa == sb then Right trueXObj else Right falseXObj
    (XObj (Chr ca) _ _, XObj (Chr cb) _ _) ->
      if ca == cb then Right trueXObj else Right falseXObj
    (XObj (Sym sa _) _ _, XObj (Sym sb _) _ _) ->
      if sa == sb then Right trueXObj else Right falseXObj
    (XObj (Bol xa) _ _, XObj (Bol xb) _ _) ->
      if xa == xb then Right trueXObj else Right falseXObj
    _ ->
      Left (EvalError ("Can't compare " ++ pretty a ++ " with " ++ pretty b))

commandLt :: CommandCallback
commandLt [a, b] =
 return $ case (a, b) of
   (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
     if (round aNum :: Int) < (round bNum :: Int)
     then Right trueXObj else Right falseXObj
   (XObj (Num LongTy aNum) _ _, XObj (Num LongTy bNum) _ _) ->
     if (round aNum :: Int) < (round bNum :: Int)
     then Right trueXObj else Right falseXObj
   (XObj (Num FloatTy aNum) _ _, XObj (Num floatTy bNum) _ _) ->
     if aNum < bNum
     then Right trueXObj else Right falseXObj
   (XObj (Num DoubleTy aNum) _ _, XObj (Num DoubleTy bNum) _ _) ->
     if aNum < bNum
     then Right trueXObj else Right falseXObj
   _ ->
     Left (EvalError ("Can't compare (<) " ++ pretty a ++ " with " ++ pretty b))

commandGt :: CommandCallback
commandGt [a, b] =
  return $ case (a, b) of
    (XObj (Num IntTy aNum) _ _, XObj (Num IntTy bNum) _ _) ->
      if (round aNum :: Int) > (round bNum :: Int)
      then Right trueXObj else Right falseXObj
    (XObj (Num LongTy aNum) _ _, XObj (Num LongTy bNum) _ _) ->
      if (round aNum :: Int) > (round bNum :: Int)
      then Right trueXObj else Right falseXObj
    (XObj (Num FloatTy aNum) _ _, XObj (Num floatTy bNum) _ _) ->
      if aNum > bNum
      then Right trueXObj else Right falseXObj
    (XObj (Num DoubleTy aNum) _ _, XObj (Num DoubleTy bNum) _ _) ->
      if aNum > bNum
      then Right trueXObj else Right falseXObj
    _ ->
      Left (EvalError ("Can't compare (>) " ++ pretty a ++ " with " ++ pretty b))

commandCharAt :: CommandCallback
commandCharAt [a, b] =
  return $ case (a, b) of
    (XObj (Str s) _ _, XObj (Num IntTy n) _ _) ->
      Right (XObj (Chr (s !! (round n :: Int))) (Just dummyInfo) (Just IntTy))
    _ ->
      Left (EvalError ("Can't call char-at with " ++ pretty a ++ " and " ++ pretty b))

commandIndexOf :: CommandCallback
commandIndexOf [a, b] =
  return $ case (a, b) of
    (XObj (Str s) _ _, XObj (Chr c) _ _) ->
      Right (XObj (Num IntTy (getIdx c s)) (Just dummyInfo) (Just IntTy))
    _ ->
      Left (EvalError ("Can't call index-of with " ++ pretty a ++ " and " ++ pretty b))
  where getIdx c s = fromIntegral $ fromMaybe (-1) $ elemIndex c s

commandSubstring :: CommandCallback
commandSubstring [a, b, c] =
  return $ case (a, b, c) of
    (XObj (Str s) _ _, XObj (Num IntTy f) _ _, XObj (Num IntTy t) _ _) ->
      Right (XObj (Str (take (round t :: Int) (drop (round f :: Int) s))) (Just dummyInfo) (Just StringTy))
    _ ->
      Left (EvalError ("Can't call substring with " ++ pretty a ++ ", " ++ pretty b ++ " and " ++ pretty c))

commandStringCount :: CommandCallback
commandStringCount [a] =
  return $ case a of
    XObj (Str s) _ _ ->
      Right (XObj (Num IntTy (fromIntegral (length s))) (Just dummyInfo) (Just IntTy))
    _ ->
      Left (EvalError ("Can't call count with " ++ pretty a))

commandStringJoin :: CommandCallback
commandStringJoin [a] =
  return $ case a of
    XObj (Arr strings) _ _ ->
      case (sequence (map unwrapStringXObj strings)) of
        Left err -> Left (EvalError err)
        Right result -> Right (XObj (Str (join result)) (Just dummyInfo) (Just StringTy))
    _ ->
      Left (EvalError ("Can't call count with " ++ pretty a))

commandStringDirectory :: CommandCallback
commandStringDirectory [a] =
  return $ case a of
    XObj (Str s) _ _ ->
      Right (XObj (Str (takeDirectory s)) (Just dummyInfo) (Just StringTy))
    _ ->
      Left (EvalError ("Can't call count with " ++ pretty a))

commandPlus :: CommandCallback
commandPlus [a, b] =
  return $ case (a, b) of
    (XObj (Num aty aNum) _ _, XObj (Num bty bNum) _ _) ->
      if aty == bty
      then Right (XObj (Num aty (aNum + bNum)) (Just dummyInfo) (Just aty))
      else Left (EvalError ("Can't call + with " ++ pretty a ++ " and " ++ pretty b))
    _ ->
      Left (EvalError ("Can't call + with " ++ pretty a ++ " and " ++ pretty b))

commandMinus :: CommandCallback
commandMinus [a, b] =
  return $ case (a, b) of
    (XObj (Num aty aNum) _ _, XObj (Num bty bNum) _ _) ->
      if aty == bty
      then Right (XObj (Num aty (aNum - bNum)) (Just dummyInfo) (Just aty))
      else Left (EvalError ("Can't call - with " ++ pretty a ++ " and " ++ pretty b))
    _ ->
      Left (EvalError ("Can't call - with " ++ pretty a ++ " and " ++ pretty b))

commandDiv :: CommandCallback
commandDiv [a, b] =
  return $ case (a, b) of
    (XObj (Num aty aNum) _ _, XObj (Num bty bNum) _ _) ->
      if aty == bty
      then Right (XObj (Num aty (aNum / bNum)) (Just dummyInfo) (Just aty))
      else Left (EvalError ("Can't call / with " ++ pretty a ++ " and " ++ pretty b))
    _ ->
      Left (EvalError ("Can't call / with " ++ pretty a ++ " and " ++ pretty b))

commandMul :: CommandCallback
commandMul [a, b] =
  return $ case (a, b) of
    (XObj (Num aty aNum) _ _, XObj (Num bty bNum) _ _) ->
      if aty == bty
      then Right (XObj (Num aty (aNum * bNum)) (Just dummyInfo) (Just aty))
      else Left (EvalError ("Can't call * with " ++ pretty a ++ " and " ++ pretty b))
    _ ->
      Left (EvalError ("Can't call * with " ++ pretty a ++ " and " ++ pretty b))

commandStr :: CommandCallback
commandStr xs =
  return (Right (XObj (Str (join (map f xs))) (Just dummyInfo) (Just StringTy)))
  where f (XObj (Str s) _ _) = s
        f x = pretty x

commandNot :: CommandCallback
commandNot [x] =
  case x of
    XObj (Bol ab) _ _ ->
      if ab
      then return (Right falseXObj)
      else return (Right trueXObj)
    _ ->
      return (Left (EvalError ("Can't perform logical operation (not) on " ++ pretty x)))

commandSaveDocsInternal :: CommandCallback
commandSaveDocsInternal [modulePath] =
  do ctx <- get
     let globalEnv = contextGlobalEnv ctx
     case modulePath of
       XObj (Lst xobjs) _ _ ->
         case sequence (map unwrapSymPathXObj xobjs) of
           Left err -> return (Left (EvalError err))
           Right okPaths ->
             case sequence (map (getEnvironmentForDocumentation globalEnv) okPaths) of
               Left err -> return (Left err)
               Right okEnvs -> saveDocs (zip okPaths okEnvs)
       x ->
         return (Left (EvalError ("Invalid arg to save-docs-internal (expected list of symbols): " ++ pretty x)))
  where getEnvironmentForDocumentation :: Env -> SymPath -> Either EvalError Env
        getEnvironmentForDocumentation env path =
          case lookupInEnv path env of
            Just (_, Binder _ (XObj (Mod foundEnv) _ _)) ->
              Right foundEnv
            Just (_, Binder _ x) ->
              Left (EvalError ("Non module can't be saved: " ++ pretty x))
            Nothing ->
              Left (EvalError ("Can't find module at '" ++ show path ++ "'"))

saveDocs :: [(SymPath, Env)] -> StateT Context IO (Either EvalError XObj)
saveDocs pathsAndEnvs =
  do ctx <- get
     let proj = contextProj ctx
         docsDir = projectDocsDir proj
         title = projectTitle proj
     liftIO (saveDocsForEnvs docsDir title pathsAndEnvs)
     return dynamicNil
