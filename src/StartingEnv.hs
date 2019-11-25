module StartingEnv where

import qualified Data.Set as Set
import qualified Data.Map as Map

import ColorText
import Obj
import Types
import Template
import ToTemplate
import ArrayTemplates
import Commands
import Parsing
import Eval
import Concretize

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
  where bindings = Map.fromList [ templateNth
                                , templateAllocate
                                , templateEMap
                                , templateEFilter
                                , templateRaw
                                , templateAset
                                , templateAsetBang
                                , templateAsetUninitializedBang
                                , templateLength
                                , templatePushBack
                                , templatePushBackBang
                                , templatePopBack
                                , templatePopBackBang
                                , templateDeleteArray
                                , templateCopyArray
                                , templateStrArray
                                ]

-- | The Pointer module contains functions for dealing with pointers.
pointerModule :: Env
pointerModule = Env { envBindings = bindings
                    , envParent = Nothing
                    , envModuleName = Just "Pointer"
                    , envUseModules = []
                    , envMode = ExternalEnv
                    , envFunctionNestingLevel = 0 }
  where bindings = Map.fromList [ templatePointerCopy, templatePointerEqual, templatePointerToRef ]

-- | A template function for copying (= deref:ing) any pointer.
templatePointerCopy :: (String, Binder)
templatePointerCopy = defineTemplate
  (SymPath ["Pointer"] "copy")
  (FuncTy StaticLifetimeTy [RefTy (PointerTy (VarTy "p")) (VarTy "q")] (PointerTy (VarTy "p")))
  "copies a pointer `p`."
  (toTemplate "$p* $NAME ($p** ptrRef)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return *ptrRef;"
                        ,"}"])
  (const [])

templatePointerEqual = defineTemplate
  (SymPath ["Pointer"] "eq")
  (FuncTy StaticLifetimeTy [PointerTy (VarTy "p"), PointerTy (VarTy "p")] BoolTy)
  "checks two pointers for equality."
  (toTemplate "bool $NAME ($p *p1, $p *p2)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return p1 == p2;"
                        ,"}"])
  (const [])

-- | A template function for converting pointers to ref (it's up to the user of this function to make sure that is a safe operation).
templatePointerToRef = defineTemplate
  (SymPath ["Pointer"] "to-ref")
  (FuncTy StaticLifetimeTy [PointerTy (VarTy "p")] (RefTy (VarTy "p") StaticLifetimeTy))
  "converts a pointer to a reference type. The user will have to ensure themselves that this is a safe operation."
  (toTemplate "$p* $NAME ($p *p)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return p;"
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
  (FuncTy StaticLifetimeTy [IntTy] (VarTy "a"))
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
    funcTy = FuncTy StaticLifetimeTy (take arity (map (VarTy . charToTyName) alphabet)) (VarTy "z")
    bindings = Map.fromList [ generateTemplateFuncCopy funcTy
                            , generateTemplateFuncDelete funcTy
                            , generateTemplateFuncStrOrPrn "str" "converts a function to a string." funcTy
                            , generateTemplateFuncStrOrPrn "prn" "converts a function to a string (internal representation)." funcTy
                            ]


-- | A template function for generating 'copy' functions for function pointers.
generateTemplateFuncCopy :: Ty -> (String, Binder)
generateTemplateFuncCopy funcTy = defineTemplate
  (SymPath ["Function"] "copy")
  (FuncTy StaticLifetimeTy [RefTy funcTy (VarTy "q")] (VarTy "a"))
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
  (FuncTy StaticLifetimeTy [funcTy] UnitTy)
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
  (FuncTy StaticLifetimeTy [RefTy funcTy (VarTy "q")] StringTy)
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
  where bindings = Map.fromList $
                    [ addCommand "list?" 1 commandIsList
                    , addCommand "array?" 1 commandIsArray
                    , addCommand "symbol?" 1 commandIsSymbol
                    , addCommand "length" 1 commandLength
                    , addCommand "car" 1 commandCar
                    , addCommand "cdr" 1 commandCdr
                    , addCommand "last" 1 commandLast
                    , addCommand "all-but-last" 1 commandAllButLast
                    , addCommand "cons" 2 commandCons
                    , addCommand "cons-last" 2 commandConsLast
                    , addCommand "append" 2 commandAppend
                    , addCommandConfigurable "array" Nothing commandArray
                    , addCommandConfigurable "list" Nothing commandList
                    , addCommand "macro-error" 1 commandMacroError
                    , addCommandConfigurable "macro-log" Nothing commandMacroLog
                    , addCommandConfigurable "str" Nothing commandStr
                    , addCommand "not" 1 commandNot
                    , addCommand "=" 2 commandEq
                    , addCommand "<" 2 commandLt
                    , addCommand ">" 2 commandGt
                    , addCommand "+" 2 commandPlus
                    , addCommand "-" 2 commandMinus
                    , addCommand "/" 2 commandDiv
                    , addCommand "*" 2 commandMul
                    , addCommand "c" 1 commandC
                    , addCommand "quit" 0 commandQuit
                    , addCommand "cat" 0 commandCat
                    , addCommand "run" 0 commandRunExe
                    , addCommand "build" 0 (commandBuild False)
                    , addCommand "reload" 0 commandReload
                    , addCommand "env" 0 commandListBindings
                    , addCommandConfigurable "help" Nothing commandHelp
                    , addCommand "project" 0 commandProject
                    , addCommand "load" 1 commandLoad
                    , addCommand "expand" 1 commandExpand
                    , addCommand "os" 0 commandOS
                    , addCommand "system-include" 1 commandAddSystemInclude
                    , addCommand "relative-include" 1 commandAddRelativeInclude
                    , addCommand "save-docs-internal" 1 commandSaveDocsInternal
                    , addCommand "read-file" 1 commandReadFile
                    , addCommand "write-file" 2 commandWriteFile
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
  where bindings = Map.fromList [ addCommand "char-at" 2 commandCharAt
                                , addCommand "index-of" 2 commandIndexOf
                                , addCommand "substring" 3 commandSubstring
                                , addCommand "length" 1 commandStringLength
                                , addCommand "join" 1 commandStringJoin
                                , addCommand "directory" 1 commandStringDirectory
                                ]

-- | A submodule of the Dynamic module. Contains functions for working with symbols in the repl or during compilation.
dynamicSymModule :: Env
dynamicSymModule = Env { envBindings = bindings
                       , envParent = Nothing
                       , envModuleName = Just "Symbol"
                       , envUseModules = []
                       , envMode = ExternalEnv
                       , envFunctionNestingLevel = 0 }
  where bindings = Map.fromList [ addCommand "join" 1 commandSymJoin
                                , addCommand "prefix" 2 commandSymPrefix
                                , addCommand "from" 1 commandSymFrom
                                , addCommand "str" 1 commandSymStr
                                ]

-- | A submodule of the Dynamic module. Contains functions for working with the active Carp project.
dynamicProjectModule :: Env
dynamicProjectModule = Env { envBindings = bindings
                           , envParent = Nothing
                           , envModuleName = Just "Project"
                           , envUseModules = []
                           , envMode = ExternalEnv
                           , envFunctionNestingLevel = 0 }
  where bindings = Map.fromList [ addCommand "config" 2 commandProjectConfig
                                , addCommand "get-config" 1 commandProjectGetConfig
                                ]

-- | A hack-ish function for converting any enum to an int.
templateEnumToInt :: (String, Binder)
templateEnumToInt = defineTemplate
  (SymPath [] "enum-to-int")
  (FuncTy StaticLifetimeTy [VarTy "a"] IntTy)
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
  where bindings = Map.fromList [ templateCoerce ]

-- | A template for coercing (casting) a type to another type
templateCoerce :: (String, Binder)
templateCoerce = defineTemplate
  (SymPath ["Unsafe"] "coerce")
  (FuncTy StaticLifetimeTy [VarTy "b"] (VarTy "a"))
  "coerces a value of type b to a value of type a."
  (toTemplate "$a $NAME ($b b)")
  (toTemplate $ unlines ["$DECL {"
                        ,"   return ($a)b;"
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
                   ++ [("Pointer",  Binder emptyMeta (XObj (Mod pointerModule) Nothing Nothing))]
                   ++ [("System",   Binder emptyMeta (XObj (Mod systemModule) Nothing Nothing))]
                   ++ [("Dynamic",  Binder emptyMeta (XObj (Mod dynamicModule) Nothing Nothing))]
                   ++ [("Function", Binder emptyMeta (XObj (Mod functionModule) Nothing Nothing))]
                   ++ [("Unsafe", Binder emptyMeta (XObj (Mod unsafeModule) Nothing Nothing))]

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
          [ interfaceBinder "copy" (FuncTy StaticLifetimeTy [RefTy (VarTy "a") (VarTy "q")] (VarTy "a"))
            ([SymPath ["Array"] "copy", SymPath ["Pointer"] "copy"] ++ registerFunctionFunctionsWithInterface "copy")
            builtInSymbolInfo

          , interfaceBinder "str" (FuncTy StaticLifetimeTy [VarTy "a"] StringTy)
            (SymPath ["Array"] "str" : registerFunctionFunctionsWithInterface "str")
            builtInSymbolInfo

          , interfaceBinder "prn" (FuncTy StaticLifetimeTy [VarTy "a"] StringTy)
            (registerFunctionFunctionsWithInterface "prn")
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
