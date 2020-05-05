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
                                , templatePointerEqual
                                , templatePointerToRef
                                , templatePointerToValue
                                , templatePointerAdd
                                , templatePointerSub
                                , templatePointerWidth
                                , templatePointerToLong
                                , templatePointerFromLong
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

templatePointerEqual = defineTemplate
  (SymPath ["Pointer"] "eq")
  (FuncTy [PointerTy (VarTy "p"), PointerTy (VarTy "p")] BoolTy StaticLifetimeTy)
  "checks two pointers for equality."
  (toTemplate "bool $NAME ($p *p1, $p *p2)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return p1 == p2;"
                        ,"}"])
  (const [])

-- | A template function for converting pointers to ref (it's up to the user of this function to make sure that is a safe operation).
templatePointerToRef = defineTemplate
  (SymPath ["Pointer"] "to-ref")
  (FuncTy [PointerTy (VarTy "p")] (RefTy (VarTy "p") StaticLifetimeTy) StaticLifetimeTy)
  "converts a pointer to a reference type. The user will have to ensure themselves that this is a safe operation."
  (toTemplate "$p* $NAME ($p *p)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return p;"
                        ,"}"])
  (const [])


-- | A template function for converting pointers to values (it's up to the user of this function to make sure that is a safe operation).
templatePointerToValue = defineTemplate
  (SymPath ["Pointer"] "to-value")
  (FuncTy [PointerTy (VarTy "p")] (VarTy "p") StaticLifetimeTy)
  "converts a pointer to a value. The user will have to ensure themselves that this is a safe operation."
  (toTemplate "$p $NAME ($p *p)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return *p;"
                        ,"}"])
  (const [])

templatePointerAdd = defineTemplate
  (SymPath ["Pointer"] "add")
  (FuncTy [PointerTy (VarTy "p"), LongTy] (PointerTy (VarTy "p")) StaticLifetimeTy)
  "adds a long integer value to a pointer."
  (toTemplate "$p* $NAME ($p *p, Long x)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return p + x;"
                        ,"}"])
  (const [])

templatePointerSub = defineTemplate
  (SymPath ["Pointer"] "sub")
  (FuncTy [PointerTy (VarTy "p"), LongTy] (PointerTy (VarTy "p")) StaticLifetimeTy)
  "subtracts a long integer value from a pointer."
  (toTemplate "$p* $NAME ($p *p, Long x)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return p - x;"
                        ,"}"])
  (const [])

templatePointerWidth = defineTemplate
  (SymPath ["Pointer"] "width")
  (FuncTy [PointerTy (VarTy "p")] LongTy StaticLifetimeTy)
  "gets the byte size of a pointer."
  (toTemplate "Long $NAME ($p *p)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return sizeof(*p);"
                        ,"}"])
  (const [])

templatePointerToLong = defineTemplate
  (SymPath ["Pointer"] "to-long")
  (FuncTy [PointerTy (VarTy "p")] LongTy StaticLifetimeTy)
  "converts a pointer to a long integer."
  (toTemplate "Long $NAME ($p *p)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return (Long)p;"
                        ,"}"])
  (const [])

templatePointerFromLong = defineTemplate
  (SymPath ["Pointer"] "from-long")
  (FuncTy [LongTy] (PointerTy (VarTy "p")) StaticLifetimeTy)
  "converts a long integer to a pointer."
  (toTemplate "$p* $NAME (Long p)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return ($p*)p;"
                        ,"}"])
  (const [])

-- | The System module contains functions for various OS related things like timing and process control.
systemModule :: Env
systemModule = Env { envBindings = bindings
                   , envParent = Nothing
                   , envModuleName = Just "System"
                   , envUseModules = []
                   , envMode = ExternalEnv
                   , envFunctionNestingLevel = 0 }
  where bindings = Map.fromList [ templateExit ]


-- | A template function for exiting.
templateExit :: (String, Binder)
templateExit = defineTemplate
  (SymPath ["System"] "exit")
  (FuncTy [IntTy] (VarTy "a") StaticLifetimeTy)
  "exits the program."
  (toTemplate "$a $NAME (int code)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    exit(code);"
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
                    [ addCommand (SymPath path "list?") 1 commandIsList
                    , addCommand (SymPath path "array?") 1 commandIsArray
                    , addCommand (SymPath path "symbol?") 1 commandIsSymbol
                    , addCommand (SymPath path "length") 1 commandLength
                    , addCommand (SymPath path "car") 1 commandCar
                    , addCommand (SymPath path "cdr") 1 commandCdr
                    , addCommand (SymPath path "last") 1 commandLast
                    , addCommand (SymPath path "all-but-last") 1 commandAllButLast
                    , addCommand (SymPath path "cons") 2 commandCons
                    , addCommand (SymPath path "cons-last") 2 commandConsLast
                    , addCommand (SymPath path "append") 2 commandAppend
                    , addCommandConfigurable (SymPath path "array") Nothing commandArray
                    , addCommandConfigurable (SymPath path "list") Nothing commandList
                    , addCommand (SymPath path "macro-error") 1 commandMacroError
                    , addCommandConfigurable (SymPath path "macro-log") Nothing commandMacroLog
                    , addCommandConfigurable (SymPath path "str") Nothing commandStr
                    , addCommandConfigurable (SymPath path "inline-c") Nothing commandInlineC
                    , addCommand (SymPath path "not") 1 commandNot
                    , addCommand (SymPath path "=") 2 commandEq
                    , addCommand (SymPath path "<") 2 commandLt
                    , addCommand (SymPath path ">") 2 commandGt
                    , addCommand (SymPath path "+") 2 commandPlus
                    , addCommand (SymPath path "-") 2 commandMinus
                    , addCommand (SymPath path "/") 2 commandDiv
                    , addCommand (SymPath path "*") 2 commandMul
                    , addCommand (SymPath path "c") 1 commandC
                    , addCommand (SymPath path "quit") 0 commandQuit
                    , addCommand (SymPath path "cat") 0 commandCat
                    , addCommand (SymPath path "run") 0 commandRunExe
                    , addCommand (SymPath path "build") 0 (commandBuild False)
                    , addCommand (SymPath path "reload") 0 commandReload
                    , addCommand (SymPath path "env") 0 commandListBindings
                    , addCommandConfigurable (SymPath path "help") Nothing commandHelp
                    , addCommand (SymPath path "project") 0 commandProject
                    , addCommand (SymPath path "load") 1 commandLoad
                    , addCommand (SymPath path "load-once") 1 commandLoadOnce
                    , addCommand (SymPath path "expand") 1 commandExpand
                    , addCommand (SymPath path "os") 0 commandOS
                    , addCommand (SymPath path "system-include") 1 commandAddSystemInclude
                    , addCommand (SymPath path "relative-include") 1 commandAddRelativeInclude
                    , addCommand (SymPath path "save-docs-internal") 1 commandSaveDocsInternal
                    , addCommand (SymPath path "read-file") 1 commandReadFile
                    , addCommand (SymPath path "write-file") 2 commandWriteFile
                    , addCommand (SymPath path "bit-width") 0 commandBitWidth
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
        bindings = Map.fromList [ addCommand (SymPath path "char-at") 2 commandCharAt
                                , addCommand (SymPath path "index-of") 2 commandIndexOf
                                , addCommand (SymPath path "slice") 3 commandSubstring
                                , addCommand (SymPath path "length") 1 commandStringLength
                                , addCommand (SymPath path "concat") 1 commandStringConcat
                                , addCommand (SymPath path "directory") 1 commandStringDirectory
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
        bindings = Map.fromList [ addCommand (SymPath path "concat") 1 commandSymConcat
                                , addCommand (SymPath path "prefix") 2 commandSymPrefix
                                , addCommand (SymPath path "from") 1 commandSymFrom
                                , addCommand (SymPath path "str") 1 commandSymStr
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
        bindings = Map.fromList [ addCommand (SymPath path "config") 2 commandProjectConfig
                                , addCommand (SymPath path "get-config") 1 commandProjectGetConfig
                                ]

-- | A hack-ish function for converting any enum to an int.
templateEnumToInt :: (String, Binder)
templateEnumToInt = defineTemplate
  (SymPath [] "enum-to-int")
  (FuncTy [VarTy "a"] IntTy StaticLifetimeTy)
  "converts an enum `e` to an integer."
  (toTemplate "int $NAME ($a e)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return (int)e;"
                        ,"}"])
  (const [])

-- | The Unsafe module contains dangerous functions
unsafeModule :: Env
unsafeModule = Env { envBindings = bindings
                   , envParent = Nothing
                   , envModuleName = Just "Unsafe"
                   , envUseModules = []
                   , envMode = ExternalEnv
                   , envFunctionNestingLevel = 0 }
  where bindings = Map.fromList [ templateCoerce, templateLeak ]

-- | A template for coercing (casting) a type to another type
templateCoerce :: (String, Binder)
templateCoerce = defineTemplate
  (SymPath ["Unsafe"] "coerce")
  (FuncTy [VarTy "b"] (VarTy "a") StaticLifetimeTy)
  "coerces a value of type b to a value of type a."
  (toTemplate "$a $NAME ($b b)")
  (toTemplate $ unlines ["$DECL {"
                        ,"   return ($a)b;"
                        ,"}"])
  (const [])

-- | A template function for preventing destructor from being run on a value (it's up to the user of this function to make sure that memory is freed).
templateLeak = defineTemplate
  (SymPath ["Unsafe"] "leak")
  (FuncTy [(VarTy "a")] UnitTy StaticLifetimeTy)
  "prevents a destructor from being run on a value a."
  (toTemplate "void $NAME ($a a)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    // Leak"
                        ,"}"])
  (const [])

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
  where bindings = Map.fromList $ [ register "NULL" (VarTy "a")
                                  , templateEnumToInt
                                  ]
                   ++ (if noArray then [] else [("Array", Binder emptyMeta (XObj (Mod arrayModule) Nothing Nothing))])
                   ++ [("StaticArray", Binder emptyMeta (XObj (Mod staticArrayModule) Nothing Nothing))]
                   ++ [("Pointer",  Binder emptyMeta (XObj (Mod pointerModule) Nothing Nothing))]
                   ++ [("System",   Binder emptyMeta (XObj (Mod systemModule) Nothing Nothing))]
                   ++ [("Dynamic",  Binder emptyMeta (XObj (Mod dynamicModule) Nothing Nothing))]
                   ++ [("Function", Binder emptyMeta (XObj (Mod functionModule) Nothing Nothing))]
                   ++ [("Unsafe",   Binder emptyMeta (XObj (Mod unsafeModule) Nothing Nothing))]

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
