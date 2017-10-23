module Commands where

import System.Exit (exitSuccess)
import System.Process (callCommand)
import System.Directory
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe, isJust)
import Control.Monad
import Control.Exception
--import Debug.Trace

import Parsing
import Emit
import Obj
import Types
import Infer
import Deftype
import ColorText
import Template
import Util
import Eval

data Context = Context { contextGlobalEnv :: Env
                       , contextTypeEnv :: TypeEnv
                       , contextPath :: [String]
                       , contextProj :: Project
                       , contextLastInput :: String
                       } deriving Show

data ReplCommand = Define XObj
                 | AddInclude Includer
                 | Register String XObj
                 | RegisterType String
                 | AddCFlag String
                 | AddLibraryFlag String
                 | DefineModule String [XObj] (Maybe Info)
                 | DefineType XObj [XObj]
                 | DefineMacro String XObj XObj
                 | DefineDynamic String XObj XObj
                 | DefineAlias String XObj
                 | Eval XObj
                 | Expand XObj
                 | Type XObj
                 | GetInfo XObj
                 | InstantiateTemplate XObj XObj
                 | Use SymPath XObj
                 | ProjectSet String String
                 | DoNothing
                 | ReplMacroError String
                 | ReplTypeError String
                 | ReplParseError String
                 | ReplCodegenError String
                 | Load FilePath
                 | Reload
                 | BuildExe
                 | RunExe
                 | Cat
                 | Print String
                 | ListBindingsInEnv
                 | DisplayProject
                 | Help String
                 | Quit
                 | ListOfCommands [ReplCommand]
                 deriving Show

consumeExpr :: Context -> XObj -> ReplCommand
consumeExpr (Context globalEnv typeEnv _ _ _) xobj =
  case expandAll globalEnv xobj of
    Left err -> ReplMacroError (show err)
    Right expanded -> 
      case annotate typeEnv globalEnv (setFullyQualifiedSymbols globalEnv expanded) of
        Left err -> ReplTypeError (show err)
        Right annXObjs -> ListOfCommands (map printC annXObjs)

printC :: XObj -> ReplCommand
printC xobj =
  case checkForUnresolvedSymbols xobj of
    Left e ->
      (Print . strWithColor Red) (show e ++ ", can't print resulting code.\n")
    Right _ ->
      (Print . strWithColor Green . toC) xobj

objToCommand :: Context -> XObj -> ReplCommand
objToCommand ctx xobj =
  case obj xobj of
      Lst lst -> case lst of
                   XObj Defn _ _ : _ : _ : _ : [] -> Define xobj
                   XObj Def _ _ : _ : _ : [] -> Define xobj
                   XObj (Sym (SymPath _ "module")) _ _ : XObj (Sym (SymPath _ name)) _ _ : innerExpressions ->
                     DefineModule name innerExpressions (info xobj)
                   XObj (Sym (SymPath _ "defmodule")) _ _ : XObj (Sym (SymPath _ name)) _ _ : innerExpressions ->
                     DefineModule name innerExpressions (info xobj)
                   XObj (Sym (SymPath _ "defmacro")) _ _ : XObj (Sym (SymPath _ name)) _ _ : params@(XObj (Arr _) _ _) : body : [] ->
                     DefineMacro name params body
                   XObj (Sym (SymPath _ "defdynamic")) _ _ : XObj (Sym (SymPath _ name)) _ _ : params@(XObj (Arr _) _ _) : body : [] ->
                     DefineDynamic name params body
                   XObj (Sym (SymPath _ "deftype")) _ _ : name : rest -> DefineType name rest
                   XObj (Sym (SymPath _ "defalias")) _ _ : XObj (Sym (SymPath _ name)) _ _ : t : [] -> DefineAlias name t
                   XObj (Sym (SymPath _ "eval")) _ _ : form : [] -> Eval form
                   XObj (Sym (SymPath _ "expand")) _ _ : form : [] -> Expand form
                   XObj (Sym (SymPath _ "instantiate")) _ _ : name : signature : [] -> InstantiateTemplate name signature
                   XObj (Sym (SymPath _ "type")) _ _ : form : [] -> Type form
                   XObj (Sym (SymPath _ "info")) _ _ : form : [] -> GetInfo form
                   XObj (Sym (SymPath _ "help")) _ _ : XObj (Sym (SymPath _ chapter)) _ _ : [] -> Help chapter
                   XObj (Sym (SymPath _ "help")) _ _ : [] -> Help ""
                   XObj (Sym (SymPath _ "quit")) _ _ : [] -> Quit
                   XObj (Sym (SymPath _ "env")) _ _ : [] -> ListBindingsInEnv
                   XObj (Sym (SymPath _ "build")) _ _ : [] -> BuildExe
                   XObj (Sym (SymPath _ "run")) _ _ : [] -> RunExe
                   XObj (Sym (SymPath _ "cat")) _ _ : [] -> Cat
                   XObj (Sym (SymPath _ "use")) _ _ : XObj (Sym path) _ _ : [] -> Use path xobj
                   XObj (Sym (SymPath _ "project-set!")) _ _ : XObj (Sym (SymPath _ key)) _ _ : XObj (Str value) _ _ : [] -> ProjectSet key value
                   XObj (Sym (SymPath _ "register")) _ _ : XObj (Sym (SymPath _ name)) _ _ : t : [] -> Register name t
                   XObj (Sym (SymPath _ "register-type")) _ _ : XObj (Sym (SymPath _ name)) _ _ : [] -> RegisterType name
                   XObj (Sym (SymPath _ "local-include")) _ _ : XObj (Str file) _ _ : [] -> AddInclude (LocalInclude file)
                   XObj (Sym (SymPath _ "system-include")) _ _ : XObj (Str file) _ _ : [] -> AddInclude (SystemInclude file)
                   XObj (Sym (SymPath _ "add-cflag")) _ _ : XObj (Str flag) _ _ : [] -> AddCFlag flag
                   XObj (Sym (SymPath _ "add-lib")) _ _ : XObj (Str flag) _ _ : [] -> AddLibraryFlag flag
                   XObj (Sym (SymPath _ "project")) _ _ : [] -> DisplayProject
                   XObj (Sym (SymPath _ "load")) _ _ : XObj (Str path) _ _ : [] -> Load path
                   XObj (Sym (SymPath _ "reload")) _ _ : [] -> Reload                   
                   _ -> consumeExpr ctx xobj
      Sym (SymPath [] (':' : text)) -> ListOfCommands (mapMaybe charToCommand text)
      _ -> consumeExpr ctx xobj

charToCommand :: Char -> Maybe ReplCommand
charToCommand 'x' = Just RunExe
charToCommand 'r' = Just Reload
charToCommand 'b' = Just BuildExe
charToCommand 'c' = Just Cat
charToCommand 'e' = Just ListBindingsInEnv
charToCommand 'h' = Just (Help "")
charToCommand 'p' = Just DisplayProject
charToCommand 'q' = Just Quit
charToCommand _ = Nothing

define :: Context -> XObj -> IO Context
define ctx@(Context globalEnv typeEnv _ proj _) annXObj =
  -- Sort different kinds of definitions into the globalEnv or the typeEnv:
  case annXObj of
    XObj (Lst (XObj (Defalias _) _ _ : _)) _ _ ->
      do --putStrLnWithColor Yellow (show (getPath annXObj) ++ " : " ++ show annXObj)
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) annXObj) })
    _ ->
      do --putStrLnWithColor Blue (show (getPath annXObj) ++ " : " ++ showMaybeTy (ty annXObj))
         when (projectEchoC proj) $
           putStrLn (toC annXObj)
         return (ctx { contextGlobalEnv = envInsertAt globalEnv (getPath annXObj) annXObj })

popModulePath :: Context -> Context
popModulePath ctx = ctx { contextPath = init (contextPath ctx) }

executeCommand :: Context -> ReplCommand -> IO Context
executeCommand ctx@(Context env typeEnv pathStrings proj lastInput) cmd =

  do when (isJust (envModuleName env)) $
       compilerError ("Global env module name is " ++ fromJust (envModuleName env) ++ " (should be Nothing).")

     case cmd of

       Define xobj ->
         let innerEnv = getEnv env pathStrings
         in  case expandAll env xobj of
               Left err -> executeCommand ctx (ReplMacroError (show err))
               Right expanded ->
                 let xobjFullPath = setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj))
                     xobjFullSymbols = setFullyQualifiedSymbols innerEnv xobjFullPath
                 in case annotate typeEnv env xobjFullSymbols of
                      Left err -> executeCommand ctx (ReplTypeError (show err))
                      Right annXObjs -> foldM define ctx annXObjs

       DefineModule moduleName innerExpressions moduleInfoFromSourceLocation ->
         case lookupInEnv (SymPath pathStrings moduleName) env of
           Just (_, Binder (XObj (Mod _) _ _)) ->
             do ctxAfterModuleAdditions <- foldM folder (Context env typeEnv (pathStrings ++ [moduleName]) proj lastInput) innerExpressions
                return (popModulePath ctxAfterModuleAdditions)
           Just _ ->
             do putStrLnWithColor Red ("Can't redefine '" ++ moduleName ++ "' as module.")
                return ctx
           Nothing ->
             do let parentEnv = getEnv env pathStrings
                    innerEnv = Env (Map.fromList []) (Just parentEnv) (Just moduleName) [] ExternalEnv
                    newModule = XObj (Mod innerEnv) moduleInfoFromSourceLocation (Just ModuleTy)
                    globalEnvWithModuleAdded = envInsertAt env (SymPath pathStrings moduleName) newModule
                ctxAfterModuleDef <- foldM folder (Context globalEnvWithModuleAdded typeEnv (pathStrings ++ [moduleName]) proj lastInput) innerExpressions
                return (popModulePath ctxAfterModuleDef)

       DefineType nameXObj rest ->
         case nameXObj of
           XObj (Sym (SymPath _ typeName)) i _ ->
             case moduleForDeftype typeEnv env pathStrings typeName rest i of
               Right (typeModuleName, typeModuleXObj, deps) ->
                 let typeDefinition =
                       -- NOTE: The type binding is needed to emit the type definition and all the member functions of the type.
                       XObj (Lst (XObj Typ Nothing Nothing : XObj (Sym (SymPath pathStrings typeName)) Nothing Nothing : rest)) i (Just TypeTy)
                     ctx' = (ctx { contextGlobalEnv = envInsertAt env (SymPath pathStrings typeModuleName) typeModuleXObj
                                 , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) typeName typeDefinition)
                                 })
                 in  do ctx'' <- foldM define ctx' deps
                        return ctx''
               Left errorMessage ->
                 do putStrLnWithColor Red ("Invalid type definition for '" ++ pretty nameXObj ++ "'. " ++ errorMessage)
                    return ctx
           _ ->
             do putStrLnWithColor Red ("Invalid name for type definition: " ++ pretty nameXObj)
                return ctx

       InstantiateTemplate nameXObj typeXObj ->
         case nameXObj of
           XObj (Sym path@(SymPath _ templateName)) _ _ ->
             case xobjToTy typeXObj of
               Just actualTypeSignature ->
                 case lookupInEnv path env of
                   Just (_, Binder (XObj (Lst [XObj (Deftemplate (TemplateCreator templateCreator)) _ _, _]) _ t)) ->
                     let Just t' = t
                         nameWithTypeArgSuffix = templateName ++ polymorphicSuffix t' actualTypeSignature
                         path' = SymPath pathStrings nameWithTypeArgSuffix
                         template = templateCreator typeEnv env
                         (instName, instBinder) = instanceBinder path' actualTypeSignature template
                     in  return (ctx { contextGlobalEnv = envAddBinding env instName instBinder })
                   Just _ -> do putStrLnWithColor Red ("Can't find a template named '" ++ templateName ++ "'")
                                return ctx
                   Nothing -> do putStrLnWithColor Red ("Can't find a template named '" ++ templateName ++ "'")
                                 return ctx
               Nothing -> error ("Internal compiler error: No type signature on template '" ++ templateName ++ "'")
           _ ->
             do putStrLnWithColor Red ("Invalid name for type definition: " ++ pretty nameXObj)
                return ctx

       GetInfo xobj ->
         case xobj of
           XObj (Sym path@(SymPath _ name)) _ _ ->
             case lookupInEnv path env of
               Just (_, binder@(Binder (XObj _ (Just i) _))) ->
                 do putStrLnWithColor White (show binder ++ "\nDefined at " ++ prettyInfo i)
                    return ctx
               Just (_, binder) ->
                 do putStrLnWithColor White (show binder)
                    return ctx
               Nothing ->
                 case multiLookupALL name env of
                   [] ->
                     do putStrLnWithColor Red ("Can't find '" ++ show path ++ "'")
                        return ctx
                   binders ->
                     do mapM_ (\(env, binder@(Binder (XObj _ i _))) ->
                                 case i of
                                   Just i' -> putStrLnWithColor White (show binder ++ " Defined at " ++ prettyInfo i')
                                   Nothing -> putStrLnWithColor White (show binder))
                          binders
                        return ctx
           _ ->
             do putStrLnWithColor Red ("Can't get info from non-symbol: " ++ pretty xobj)
                return ctx
                
       Type xobj ->
         case xobj of
           XObj (Sym path@(SymPath _ name)) _ _ ->
             case lookupInEnv path env of               
               Just (_, binder) ->
                 do putStrLnWithColor White (show binder)
                    return ctx
               Nothing ->
                 case multiLookupALL name env of
                   [] ->
                     do putStrLnWithColor Red ("Can't find '" ++ show path ++ "'")
                        return ctx
                   binders ->
                     do mapM_ (\(env, binder) -> putStrLnWithColor White (show binder)) binders
                        return ctx
           _ ->
             do putStrLnWithColor Red ("Can't get the type of non-symbol: " ++ pretty xobj)
                return ctx

       Register name xobj ->
         case xobjToTy xobj of
           Just t -> let path = SymPath pathStrings name
                         binding = XObj (Lst [XObj External Nothing Nothing,
                                              XObj (Sym path) Nothing Nothing])
                                   (info xobj) (Just t)
                         env' = envInsertAt env path binding
                     in  return (ctx { contextGlobalEnv = env' })
           Nothing -> do putStrLnWithColor Red ("Can't understand type when registering '" ++ name ++ "'")
                         return ctx

       RegisterType name ->
         let path = SymPath pathStrings name
             binding = XObj (Lst [XObj ExternalType Nothing Nothing, XObj (Sym path) Nothing Nothing]) Nothing (Just TypeTy)
             typeEnv' = TypeEnv (envInsertAt (getTypeEnv typeEnv) path binding)
         in  return (ctx { contextTypeEnv = typeEnv' })

       DefineAlias name typeXObj ->
         case xobjToTy typeXObj of
           Just t ->
             let alias = defineTypeAlias name t
                 typeEnv' = TypeEnv (envInsertAt (getTypeEnv typeEnv) (SymPath [] name) alias)
             in  return (ctx { contextTypeEnv = typeEnv' })
           Nothing ->
             do putStrLnWithColor Red ("Invalid type for alias '" ++ name ++ "': " ++ pretty typeXObj ++ " at " ++ prettyInfoFromXObj typeXObj ++ ".")
                return ctx

       DefineMacro name params body ->
         let path = SymPath pathStrings name
             macro = XObj (Lst [XObj Macro Nothing Nothing, XObj (Sym path) Nothing Nothing, params, body])
                     (info body) (Just MacroTy)
         in  return (ctx { contextGlobalEnv = envInsertAt env path macro })

       DefineDynamic name params body ->
         let path = SymPath pathStrings name
             dynamic = XObj (Lst [XObj Dynamic Nothing Nothing, XObj (Sym path) Nothing Nothing, params, body])
                       (info body) (Just DynamicTy)
         in  return (ctx { contextGlobalEnv = envInsertAt env path dynamic })
         
       Eval xobj ->
         case eval env xobj of
           Left e ->
             do putStrLnWithColor Red (show e)
                return ctx
           Right evaled ->
             do putStrLnWithColor Yellow (pretty evaled)
                return ctx

       Expand xobj ->
         case expandAll env xobj of
           Left e ->
             do putStrLnWithColor Red (show e)
                return ctx
           Right expanded ->
             do putStrLnWithColor Yellow (pretty expanded)
                return ctx
                
       Use path xobj ->
         let e = getEnv env pathStrings
             useThese = envUseModules e
             e' = if path `elem` useThese then e else e { envUseModules = path : useThese }
             innerEnv = getEnv env pathStrings
         in case lookupInEnv path innerEnv of
           Just (_, Binder _) ->  return ctx { contextGlobalEnv = envReplaceEnvAt env pathStrings e' }
           Nothing -> do putStrLnWithColor Red ("Can't find a module named '" ++ show path ++ "' at " ++ prettyInfoFromXObj xobj ++ ".")
                         return ctx

       -- | A more general way to set project settings, will replace other means later probably.
       ProjectSet key value ->
         case key of
           "cflag" -> return ctx { contextProj = proj { projectCFlags = addIfNotPresent value (projectCFlags proj) } }
           "libflag" -> return ctx { contextProj = proj { projectCFlags = addIfNotPresent value (projectCFlags proj) } }
           "prompt" -> return ctx { contextProj = proj { projectPrompt = value } }
           _ ->
             do putStrLnWithColor Red ("Unrecognized key: '" ++ key ++ "'")
                return ctx

       BuildExe ->
         let src = do decl <- envToDeclarations typeEnv env
                      typeDecl <- envToDeclarations typeEnv (getTypeEnv typeEnv)
                      c <- envToC env
                      return ("//Types:\n" ++ typeDecl ++ "\n\n//Declarations:\n" ++ decl ++ "\n\n//Definitions:\n" ++ c)
         in case src of
              Left err -> do putStrLnWithColor Red ("[CODEGEN ERROR] " ++ show err)
                             return ctx
              Right okSrc -> do let incl = projectIncludesToC proj
                                    includeCorePath = " -I" ++ projectCarpDir proj ++ "/core/ "
                                    switches = " -g "
                                    flags = projectFlags proj ++ includeCorePath ++ switches
                                    outDir = projectOutDir proj
                                    outMain = outDir ++ "main.c"
                                    outExe = outDir ++ "a.out"
                                    outLib = outDir ++ "lib.so"
                                createDirectoryIfMissing False outDir
                                writeFile outMain (incl ++ okSrc)
                                case Map.lookup "main" (envBindings env) of
                                  Just _ -> do callCommand ("clang " ++ outMain ++ " -o " ++ outExe ++ " " ++ flags)
                                               putStrLn ("Compiled to '" ++ outExe ++ "'")
                                  Nothing -> do callCommand ("clang " ++ outMain ++ " -shared -o " ++ outLib ++ " " ++ flags)
                                                putStrLn ("Compiled to '" ++ outLib ++ "'")
                                return ctx

       RunExe ->
         let outDir = projectOutDir proj
             outExe = outDir ++ "a.out"
         in  do callCommand outExe
                return ctx

       Cat ->
         let outDir = projectOutDir proj
             outMain = outDir ++ "main.c"
         in  do callCommand ("cat -n " ++ outMain)
                return ctx

       Load path ->
         do contents <- readFile path
            let files = projectFiles proj
                files' = if path `elem` files
                         then files
                         else path : files
                proj' = proj { projectFiles = files' }
            executeString (ctx { contextProj = proj' }) contents path

       Reload ->
         do let paths = projectFiles proj
                f :: Context -> FilePath -> IO Context
                f context filepath = do contents <- readFile filepath
                                        executeString context contents filepath
            foldM f ctx paths

       AddInclude includer ->
         let includers = projectIncludes proj
             includers' = if includer `elem` includers
                          then includers
                          else includer : includers
             proj' = proj { projectIncludes = includers' }
         in  return (ctx { contextProj = proj' })

       AddCFlag flag ->
         let flags = projectCFlags proj
             flags' = if flag `elem` flags
                      then flags
                      else flag : flags
             proj' = proj { projectCFlags = flags' }
         in  return (ctx { contextProj = proj' })

       AddLibraryFlag flag ->
         let flags = projectLibFlags proj
             flags' = if flag `elem` flags
                      then flags
                      else flag : flags
             proj' = proj { projectLibFlags = flags' }
         in  return (ctx { contextProj = proj' })

       ReplParseError e ->
         do putStrLnWithColor Red ("[PARSE ERROR] " ++ e)
            return ctx

       ReplMacroError e ->
         do putStrLnWithColor Red ("[MACRO ERROR] " ++ e)
            return ctx

       ReplTypeError e ->
         do putStrLnWithColor Red ("[TYPE ERROR] " ++ e)
            return ctx

       ReplCodegenError e ->
         do putStrLnWithColor Red ("[CODEGEN ERROR] " ++ e)
            return ctx

       Print s ->
         do putStr s
            return ctx

       ListBindingsInEnv ->
         do putStrLn "Types:\n"
            putStrLn (prettyEnvironment (getTypeEnv typeEnv))
            putStrLn "\nGlobal environment:\n"
            putStrLn (prettyEnvironment env)
            putStrLn ""
            return ctx

       DisplayProject ->
         do print proj
            return ctx

       Quit -> exitSuccess

       Help "about" -> do putStrLn "Carp is an ongoing research project by Erik Svedäng, et al."
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
                          return ctx

       Help "language" -> do putStrLn "Special forms:"
                             putStrLn "(if <condition> <then> <else>)"
                             putStrLn "(while <condition> <body>)"
                             putStrLn "(do <statement1> <statement2> ... <exprN>)"
                             putStrLn "(let [<sym1> <expr1> <name2> <expr2> ...] <body>)"
                             --putStrLn "(fn [<args>] <body>)"
                             putStrLn "(the <type> <expression>)"
                             putStrLn "(ref <expression>)"
                             putStrLn "(address <expr>)"
                             putStrLn "(set! <var> <value>)"
                             putStrLn ""
                             putStrLn "To use functions in modules without qualifying them:"
                             putStrLn "(use <module>)"
                             putStrLn ""
                             putStrLn ("Valid non-alphanumerics: " ++ validCharacters)
                             putStrLn ""
                             putStrLn "Number literals:"
                             putStrLn "1      Int"
                             putStrLn "1.0    Double"
                             putStrLn "1.0f   Float"
                             putStrLn ""
                             putStrLn "Reader macros:"
                             putStrLn "&<expr>   (ref <expr>)"
                             putStrLn "@<expr>   (copy <expr>)"
                             putStrLn ""
                             return ctx

       Help "macros" -> do putStrLn "Some useful macros:"
                           putStrLn "(cond <condition1> <expr1> ... <else-condition>)"
                           putStrLn "(for [<var> <from> <to>] <body>)"
                           putStrLn ""
                           return ctx

       Help "structs" -> do putStrLn "A type definition will generate the following methods:"
                            putStrLn "Getters  (<method-name> (Ref <struct>))"
                            putStrLn "Setters  (set-<method-name> <struct> <new-value>)"
                            putStrLn "Updaters (update-<method-name> <struct> <new-value>)"
                            putStrLn "init (stack allocation)"
                            putStrLn "new (heap allocation)"
                            putStrLn "copy"
                            putStrLn "delete (used internally, no need to call this explicitly)"
                            putStrLn ""
                            return ctx

       Help "shortcuts" -> do putStrLn "GHC-style shortcuts at the repl:"
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
                              return ctx

       Help _ -> do putStrLn "Compiler commands:"
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
                    putStrLn "(help <chapter>)   - Available chapters: language, macros, structs, shortcuts, about."
                    putStrLn ""
                    putStrLn "To define things:"
                    putStrLn "(def <name> <constant>)           - Define a global variable."
                    putStrLn "(defn <name> [<args>] <body>)     - Define a function."
                    putStrLn "(module <name> <def1> <def2> ...) - Define a module and/or add definitions to an existing one."
                    putStrLn "(deftype <name> ...)              - Define a new type."
                    putStrLn "(register <name> <type>)          - Make an external variable or function available for usage."
                    putStrLn "(defalias <name> <type>)          - Create another name for a type."
                    putStrLn ""
                    putStrLn "C-compiler configuration:"
                    putStrLn "(system-include <file>)          - Include a system header file."
                    putStrLn "(local-include <file>)           - Include a local header file."
                    putStrLn "(add-cflag <flag>)               - Add a cflag to the compilation step."
                    putStrLn "(add-lib <flag>)                 - Add a library flag to the compilation step."
                    putStrLn "(project-set! <setting> <value>) - Change a project setting (not fully implemented)."
                    putStrLn ""
                    return ctx

       DoNothing -> return ctx

       ListOfCommands commands -> foldM executeCommand ctx commands

catcher :: Context -> IOException -> IO Context
catcher ctx err = do putStrLnWithColor Red ("[RUNTIME ERROR] " ++ show err)
                     return ctx

executeString :: Context -> String -> String -> IO Context
executeString ctx input fileName = catch exec (catcher ctx)
  where exec = case parse input fileName of
                 Left parseError -> executeCommand ctx (ReplParseError (show parseError))
                 Right xobjs -> foldM folder ctx xobjs

folder :: Context -> XObj -> IO Context
folder context xobj = executeCommand context (objToCommand context xobj)
