module StartingEnv where

import qualified Data.Set as Set
import qualified Data.Map as Map

import ColorText
import Obj
import Types
import Template
import ToTemplate
import qualified ArrayTemplates
import qualified StaticArrayTemplates
import Commands
import Parsing
import Eval
import Concretize
import Primitives
import Debug.Trace (trace)

-- | These modules will be loaded in order before any other code is evaluated.
coreModules :: String -> [String]
coreModules carpDir = [carpDir ++ "/core/Core.carp"]

-- | The array module contains functions for working with the Array type.
arrayModule :: Env
arrayModule = Env { envBindings = bindings
                  , envParent = Nothing
                  , envModuleName = Just "Array"
                  , envUseModules = []
                  , envMode = ExternalEnv
                  , envFunctionNestingLevel = 0 }
  where bindings = Map.fromList [ ArrayTemplates.templateNth
                                , ArrayTemplates.templateAllocate
                                , ArrayTemplates.templateEMap
                                , ArrayTemplates.templateEFilter
                                , ArrayTemplates.templateRaw
                                , ArrayTemplates.templateUnsafeRaw
                                , ArrayTemplates.templateAset
                                , ArrayTemplates.templateAsetBang
                                , ArrayTemplates.templateAsetUninitializedBang
                                , ArrayTemplates.templateLength
                                , ArrayTemplates.templatePushBack
                                , ArrayTemplates.templatePushBackBang
                                , ArrayTemplates.templatePopBack
                                , ArrayTemplates.templatePopBackBang
                                , ArrayTemplates.templateDeleteArray
                                , ArrayTemplates.templateCopyArray
                                , ArrayTemplates.templateStrArray
                                ]

-- | The static array module
staticArrayModule :: Env
staticArrayModule = Env { envBindings = bindings
                        , envParent = Nothing
                        , envModuleName = Just "StaticArray"
                        , envUseModules = []
                        , envMode = ExternalEnv
                        , envFunctionNestingLevel = 0 }
  where bindings = Map.fromList [ StaticArrayTemplates.templateUnsafeNth
                                , StaticArrayTemplates.templateLength
                                , StaticArrayTemplates.templateDeleteArray
                                , StaticArrayTemplates.templateAsetBang
                                , StaticArrayTemplates.templateStrArray
                                ]

-- | The Pointer module contains functions for dealing with pointers.
pointerModule :: Env
pointerModule = Env { envBindings = bindings
                    , envParent = Nothing
                    , envModuleName = Just "Pointer"
                    , envUseModules = []
                    , envMode = ExternalEnv
                    , envFunctionNestingLevel = 0 }
  where bindings = Map.fromList [ templatePointerCopy
                                ]

-- | A template function for copying (= deref:ing) any pointer.
templatePointerCopy :: (String, Binder)
templatePointerCopy = defineTemplate
  (SymPath ["Pointer"] "copy")
  (FuncTy [RefTy (PointerTy (VarTy "p")) (VarTy "q")] (PointerTy (VarTy "p")) StaticLifetimeTy)
  "copies a pointer `p`."
  (toTemplate "$p* $NAME ($p** ptrRef)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return *ptrRef;"
                        ,"}"])
  (const [])


maxArity :: Int
maxArity = 9

-- | The Function module contains functions for dealing with functions.
functionModule :: Env
functionModule = Env { envBindings = bindings
                     , envParent = Nothing
                     , envModuleName = Just "Function"
                     , envUseModules = []
                     , envMode = ExternalEnv
                     , envFunctionNestingLevel = 0 }
  where
    bindEnv env = let Just name = envModuleName env
                  in  (name, Binder emptyMeta (XObj (Mod env) Nothing Nothing))
    bindings = Map.fromList (map (bindEnv . generateInnerFunctionModule) [0..maxArity])

-- | Each arity of functions need their own module to enable copying and string representation
generateInnerFunctionModule :: Int -> Env
generateInnerFunctionModule arity =
  Env { envBindings = bindings
      , envParent = Nothing
      , envModuleName = Just ("Arity" ++ show arity)
      , envUseModules = []
      , envMode = ExternalEnv
      , envFunctionNestingLevel = 0
      }
  where
    alphabet = ['d'..'y']
    charToTyName c = [c]
    funcTy = FuncTy (take arity (map (VarTy . charToTyName) alphabet)) (VarTy "z") StaticLifetimeTy
    bindings = Map.fromList [ generateTemplateFuncCopy funcTy
                            , generateTemplateFuncDelete funcTy
                            , generateTemplateFuncStrOrPrn "str" "converts a function to a string." funcTy
                            , generateTemplateFuncStrOrPrn "prn" "converts a function to a string (internal representation)." funcTy
                            ]


-- | A template function for generating 'copy' functions for function pointers.
generateTemplateFuncCopy :: Ty -> (String, Binder)
generateTemplateFuncCopy funcTy = defineTemplate
  (SymPath ["Function"] "copy")
  (FuncTy [RefTy funcTy (VarTy "q")] (VarTy "a") StaticLifetimeTy)
  "copies a function."
  (toTemplate "$a $NAME ($a* ref)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    if(ref->env) {"
                        ,"        $a f_copy;"
                        ,"        f_copy.callback = ref->callback;"
                        ,"        f_copy.delete = ref->delete;"
                        ,"        f_copy.copy = ref->copy;"
                        ,"        f_copy.env = ((void*(*)(void*))ref->copy)(ref->env);"
                        ,"        return f_copy;"
                        ,"    } else {"
                        ,"        return *ref;"
                        ,"    }"
                        ,"}"])
  (const [])

-- | A template function for generating 'deleter' functions for function pointers.
generateTemplateFuncDelete :: Ty -> (String, Binder)
generateTemplateFuncDelete funcTy = defineTemplate
  (SymPath ["Function"] "delete")
  (FuncTy [funcTy] UnitTy StaticLifetimeTy)
  "deletes a function."
  (toTemplate "void $NAME (Lambda f)")
  (toTemplate $ unlines ["$DECL {"
                        ,"  if(f.delete) {"
                        ,"      ((void(*)(void*))f.delete)(f.env);"
                        ,"      CARP_FREE(f.env);"
                        ,"  }"
                        ,"}"])
  (const [])

-- | A template function for generating 'str' or 'prn' functions for function pointers.
generateTemplateFuncStrOrPrn :: String -> String -> Ty -> (String, Binder)
generateTemplateFuncStrOrPrn name docs funcTy = defineTemplate
  (SymPath ["Function"] name)
  (FuncTy [RefTy funcTy (VarTy "q")] StringTy StaticLifetimeTy)
  docs
  (toTemplate "String $NAME (Lambda *f)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    static String lambda = \"λ\";"
                        ,"    return String_copy(&lambda);"
                        ,"}"])
  (const [])

-- | The dynamic module contains dynamic functions only available in the repl and during compilation.
dynamicModule :: Env
dynamicModule = Env { envBindings = bindings
                    , envParent = Nothing
                    , envModuleName = Just "Dynamic"
                    , envUseModules = []
                    , envMode = ExternalEnv
                    , envFunctionNestingLevel = 0 }
  where path = ["Dynamic"]
        bindings = Map.fromList $
                    [ addCommand (SymPath path "list?") 1 commandIsList "checks whether the argument is a list." "(list? '()) ; => true"
                    , addCommand (SymPath path "array?") 1 commandIsArray "checks whether the arguments is an array." "(array? []) ; => true"
                    , addCommand (SymPath path "symbol?") 1 commandIsSymbol "checks whether the argument is a symbol." "(symbol? 'x) ; => true"
                    , addCommand (SymPath path "length") 1 commandLength "returns the length of the argument (must be an array, string or list)." "(length '(1 2 3)) ; => 3"
                    , addCommand (SymPath path "car") 1 commandCar "gets the head of a list or array." "(car '(1 2 3)) ; => 1"
                    , addCommand (SymPath path "cdr") 1 commandCdr "gets the tail of a list or array." "(cdr '(1 2 3)) ; => '(2 3)"
                    , addCommand (SymPath path "last") 1 commandLast "gets the last element of a list or array." "(last '(1 2 3)) ; => 3"
                    , addCommand (SymPath path "all-but-last") 1 commandAllButLast "gets all elements except for the last one of a list or array." "(all-but-last '(1 2 3)) ; => '(1 2)"
                    , addCommand (SymPath path "cons") 2 commandCons "adds an element to the front of an array or list" "(cons 1 '(2 3)) ; => '(1 2 3)"
                    , addCommand (SymPath path "cons-last") 2 commandConsLast "adds an element to the back of an array or list" "(cons-last 3 '(1 2)) ; => '(1 2 3)"
                    , addCommand (SymPath path "append") 2 commandAppend "appends two lists or arrays." "(append '(1 2) '(3 4)) ; => '(1 2 3 4)"
                    , addCommandConfigurable (SymPath path "array") Nothing commandArray "creates an array from a collection of elements." "(array 1 2 3) ; => [1 2 3]"
                    , addCommandConfigurable (SymPath path "list") Nothing commandList "creates an array from a collection of elements." "(list 1 2 3) ; => (1 2 3)"
                    , addCommand (SymPath path "macro-error") 1 commandMacroError "logs an error and errors out of a macro." "(macro-error \"this is wrong\")"
                    , addCommandConfigurable (SymPath path "macro-log") Nothing commandMacroLog "logs a message in a macro." "(macro-log \"this will be printed at compile time\")"
                    , addCommandConfigurable (SymPath path "str") Nothing commandStr "stringifies its arguments." "(str 1 \" \" 2 \" \" 3) ; => \"1 2 3\""
                    , addCommand (SymPath path "not") 1 commandNot "negates its boolean argument." "(not false) ; => true"
                    , addCommand (SymPath path "=") 2 commandEq "compares its arguments for equality." "(= 1 2) ; => false"
                    , addCommand (SymPath path "<") 2 commandLt "checks whether its first argument is less than its second." "(< 1 2) ; => true"
                    , addCommand (SymPath path ">") 2 commandGt "checks whether its first argument is greater than its second." "(> 1 2) ; => false"
                    , addCommand (SymPath path "+") 2 commandPlus "adds its two arguments." "(+ 1 2) ; => 3"
                    , addCommand (SymPath path "-") 2 commandMinus "subtracts its second argument from its first." "(- 1 2) ; => -1"
                    , addCommand (SymPath path "/") 2 commandDiv "divides its first argument by its second." "(/ 4 2) ; => 2"
                    , addCommand (SymPath path "*") 2 commandMul "multiplies its two arguments." "(* 2 3) ; => 6"
                    , addCommand (SymPath path "c") 1 commandC "prints the C code emitted for a binding." "(c '(+ 2 3)) ; => int _3 = Int__PLUS_(2, 3);"
                    , addCommand (SymPath path "quit") 0 commandQuit "quits the program." "(quit)"
                    , addCommand (SymPath path "cat") 0 commandCat "spits out the generated C code." "(cat)"
                    , addCommand (SymPath path "run") 0 commandRunExe "runs the built executable." "(run)"
                    , addCommand (SymPath path "build") 0 (commandBuild False) "builds the current code to an executable." "(build)"
                    , addCommand (SymPath path "reload") 0 commandReload "reloads all currently loaded files that weren’t marked as only loading once (see `load` and `load-once`)." "(reload)"
                    , addCommand (SymPath path "env") 0 commandListBindings "lists all current bindings." "(env)"
                    , addCommandConfigurable (SymPath path "help") Nothing commandHelp "prints help." "(help)"
                    , addCommand (SymPath path "project") 0 commandProject "prints the current project state." "(project)"
                    , addCommand (SymPath path "load") 1 commandLoad "loads a file into the current environment." "(load \"myfile.carp\")"
                    , addCommand (SymPath path "load-once") 1 commandLoadOnce "loads a file and prevents it from being reloaded (see `reload`)." "(load-once \"myfile.carp\")"
                    , addCommand (SymPath path "expand") 1 commandExpand "expands a macro and prints the result." "(expand '(when true 1)) ; => (if true 1 ())"
                    , addCommand (SymPath path "os") 0 commandOS "prints the operating system (as returned by the Haskell function `System.Info.os`)." "(os)"
                    , addCommand (SymPath path "system-include") 1 commandAddSystemInclude "adds a system include, i.e. a C `#include` with angle brackets (`<>`)." "(system-include \"stdint.h\")"
                    , addCommand (SymPath path "relative-include") 1 commandAddRelativeInclude "adds a relative include, i.e. a C `include` with quotes. It also prepends the current directory." "(relative-include \"myheader.h\")"
                    , addCommand (SymPath path "save-docs-internal") 1 commandSaveDocsInternal "is the internal companion command to `save-docs`. `save-docs` should be called instead." "(save-docs-internal 'Module)"
                    , addCommand (SymPath path "read-file") 1 commandReadFile "reads a file into a string." "(read-file \"myfile.txt\")"
                    , addCommand (SymPath path "write-file") 2 commandWriteFile "writes a string to a file." "(write-file \"myfile\" \"hello there!\")"
                    , addCommand (SymPath path "bit-width") 0 commandBitWidth "gets the bit width of the platform." "(bit-width) ; => your host machine’s bit width, e.g. 32 or 64"
                    , makePrim "quote" 1 "quotes any value." "(quote x) ; where x is an actual symbol" (\_ ctx [x] -> return (ctx, Right x))
                    , makeVarPrim "file" "returns the file a symbol was defined in." "(file mysymbol)" primitiveFile
                    , makeVarPrim "line" "returns the line a symbol was defined on." "(line mysymbol)" primitiveLine
                    , makeVarPrim "column" "returns the column a symbol was defined on." "(column mysymbol)" primitiveColumn
                    , makePrim "info" 1 "prints all information associated with a symbol." "(info mysymbol)" primitiveInfo
                    , makeVarPrim "register-type" "registers a new type from C." "(register-type Name <optional: members>)" primitiveRegisterType
                    , makePrim "defmacro" 3 "defines a new macro." "(defmacro name [args :rest restargs] body)" primitiveDefmacro
                    , makePrim "defndynamic" 3 "defines a new dynamic function, i.e. a function available at compile time." "(defndynamic name [args] body)" primitiveDefndynamic
                    , makePrim "defdynamic" 2 "defines a new dynamic value, i.e. a value available at compile time." "(defdynamic name value)" primitiveDefdynamic
                    , makePrim "type" 1 "prints the type of a symbol." "(type mysymbol)" primitiveType
                    , makePrim "members" 1 "returns the members of a type as an array." "(members MyType)" primitiveMembers
                    , makeVarPrim "defmodule" "defines a new module in which `expressions` are defined." "(defmodule MyModule <expressions>)" primitiveDefmodule
                    , makePrim "meta-set!" 3 "sets a new key and value pair on the meta map associated with a symbol." "(meta-set! mysymbol \"mykey\" \"myval\")" primitiveMetaSet
                    , makePrim "meta" 2 "gets the value under `\"mykey\"` in the meta map associated with a symbol. It returns `()` if the key isn’t found." "(meta mysymbol \"mykey\")" primitiveMeta
                    , makePrim "definterface" 2 "defines a new interface (which could be a function or symbol)." "(definterface mysymbol MyType)" primitiveDefinterface
                    , makeVarPrim "register" "registers a new function. This is used to define C functions and other symbols that will be available at link time." "(register name <signature> <optional: override>)" primitiveRegister
                    , makeVarPrim "deftype" "defines a new sumtype or struct." "(deftype Name <members>)" primitiveDeftype
                    , makePrim "use" 1 "uses a module, i.e. imports the symbols inside that module into the current module." "(use MyModule)" primitiveUse
                    , makePrim "eval" 1 "evaluates a list." "(eval mycode)" primitiveEval
                    , makePrim "defined?" 1 "checks whether a symbol is defined." "(defined? mysymbol)" primitiveDefined
                    , makePrim "deftemplate" 4 "defines a new C template." "(deftemplate symbol Type declString defString)" primitiveDeftemplate
                    , makePrim "implements" 2 "designates a function as an implementation of an interface." "(implements zero Maybe.zero)" primitiveImplements
                    ]
                    ++ [("String", Binder emptyMeta (XObj (Mod dynamicStringModule) Nothing Nothing))
                       ,("Symbol", Binder emptyMeta (XObj (Mod dynamicSymModule) Nothing Nothing))
                       ,("Project", Binder emptyMeta (XObj (Mod dynamicProjectModule) Nothing Nothing))
                       ]

-- | A submodule of the Dynamic module. Contains functions for working with strings in the repl or during compilation.
dynamicStringModule :: Env
dynamicStringModule = Env { envBindings = bindings
                          , envParent = Nothing
                          , envModuleName = Just "String"
                          , envUseModules = []
                          , envMode = ExternalEnv
                          , envFunctionNestingLevel = 0 }
  where path = ["Dynamic", "String"]
        bindings = Map.fromList [ addCommand (SymPath path "char-at") 2 commandCharAt "gets the nth character of a string." "(String.char-at \"hi\" 1) ; => \\i"
                                , addCommand (SymPath path "index-of") 2 commandIndexOf "gets the index of a character in a string (or returns `-1` if the character is not found)." "(index-of \"hi\" \\i) ; => 1"
                                , addCommand (SymPath path "slice") 3 commandSubstring "creates a substring from a beginning index to an end index." "(String.slice \"hello\" 1 3) ; => \"ell\""
                                , addCommand (SymPath path "length") 1 commandStringLength "gets the length of a string." "(String.length \"hi\") ; => 2"
                                , addCommand (SymPath path "concat") 1 commandStringConcat "concatenates a list of strings together." "(String.concat [\"hi \" \"there\"]) ; => \"hi there\""
                                , addCommand (SymPath path "directory") 1 commandStringDirectory "takes the basename of a string taken to be a filepath.\n\nHistorical note: this is a command because it used to power one of the `include` macros." "(String.directory \"dir/file\") ; => \"dir\""
                                ]

-- | A submodule of the Dynamic module. Contains functions for working with symbols in the repl or during compilation.
dynamicSymModule :: Env
dynamicSymModule = Env { envBindings = bindings
                       , envParent = Nothing
                       , envModuleName = Just "Symbol"
                       , envUseModules = []
                       , envMode = ExternalEnv
                       , envFunctionNestingLevel = 0 }
  where path = ["Dynamic", "Symbol"]
        bindings = Map.fromList [ addCommand (SymPath path "concat") 1 commandSymConcat "concatenates a list of symbols together." "(Symbol.concat ['x 'y 'z]) ; => 'xyz"
                                , addCommand (SymPath path "prefix") 2 commandSymPrefix "prefixes a symbol with a module." "(Symbol.prefix 'Module 'fun) ; => Module.fun"
                                , addCommand (SymPath path "from") 1 commandSymFrom "converts a variety of types to a symbol." "(Symbol.from true) ; => True"
                                , addCommand (SymPath path "str") 1 commandSymStr "converts a symbol to a string." "(Symbol.str 'x) ; => \"x\""
                                ]

-- | A submodule of the Dynamic module. Contains functions for working with the active Carp project.
dynamicProjectModule :: Env
dynamicProjectModule = Env { envBindings = bindings
                           , envParent = Nothing
                           , envModuleName = Just "Project"
                           , envUseModules = []
                           , envMode = ExternalEnv
                           , envFunctionNestingLevel = 0 }
  where path = ["Dynamic", "Project"]
        bindings = Map.fromList [ addCommand (SymPath path "config") 2 commandProjectConfig "sets a project config key." "(Project.config \"paren-balance-hints\" false)"
                                , addCommand (SymPath path "get-config") 1 commandProjectGetConfig "gets a project config value under a key." "(Project.get-config \"paren-balance-hints\")"
                                ]

-- | The global environment before any code is run.
startingGlobalEnv :: Bool -> Env
startingGlobalEnv noArray =
  Env { envBindings = bindings
      , envParent = Nothing
      , envModuleName = Nothing
      , envUseModules = [SymPath [] "String"]
      , envMode = ExternalEnv
      , envFunctionNestingLevel = 0
      }
  where bindings = Map.fromList $ [ register "NULL" (PointerTy (VarTy "a"))
                                  ]
                   ++ (if noArray then [] else [("Array", Binder emptyMeta (XObj (Mod arrayModule) Nothing Nothing))])
                   ++ [("StaticArray", Binder emptyMeta (XObj (Mod staticArrayModule) Nothing Nothing))]
                   ++ [("Pointer",  Binder emptyMeta (XObj (Mod pointerModule) Nothing Nothing))]
                   ++ [("Dynamic",  Binder emptyMeta (XObj (Mod dynamicModule) Nothing Nothing))]
                   ++ [("Function", Binder emptyMeta (XObj (Mod functionModule) Nothing Nothing))]

-- | The type environment (containing deftypes and interfaces) before any code is run.
startingTypeEnv :: Env
startingTypeEnv = Env { envBindings = bindings
                      , envParent = Nothing
                      , envModuleName = Nothing
                      , envUseModules = []
                      , envMode = ExternalEnv
                      , envFunctionNestingLevel = 0
                      }
  where bindings = Map.fromList
          [ interfaceBinder "copy" (FuncTy [RefTy (VarTy "a") (VarTy "q")] (VarTy "a") StaticLifetimeTy)
            ([SymPath ["Array"] "copy", SymPath ["Pointer"] "copy"] ++ registerFunctionFunctionsWithInterface "copy")
            builtInSymbolInfo

          , interfaceBinder "str" (FuncTy [VarTy "a"] StringTy StaticLifetimeTy)
            ((SymPath ["Array"] "str") : (SymPath ["StaticArray"] "str") : registerFunctionFunctionsWithInterface "str")
            builtInSymbolInfo

          , interfaceBinder "prn" (FuncTy [VarTy "a"] StringTy StaticLifetimeTy)
            ((SymPath ["StaticArray"] "str") : (registerFunctionFunctionsWithInterface "prn")) -- QUESTION: Where is 'prn' for dynamic Array:s registered? Can't find it... (but it is)
            builtInSymbolInfo
          ]
        builtInSymbolInfo = Info (-1) (-1) "Built-in." Set.empty (-1)

-- | Make the functions in the Function.Arity<N> modules register with the interfaces in the type Env.
registerFunctionFunctionsWithInterface :: String -> [SymPath]
registerFunctionFunctionsWithInterface interfaceName =
  map (\arity -> SymPath ["Function", "Arity" ++ show arity] interfaceName) [0..maxArity]

-- | Create a binder for an interface definition.
interfaceBinder :: String -> Ty -> [SymPath] -> Info -> (String, Binder)
interfaceBinder name t paths i = (name, Binder emptyMeta (defineInterface name t paths (Just i)))
