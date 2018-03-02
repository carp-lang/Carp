module StartingEnv where

import qualified Data.Set as Set
import qualified Data.Map as Map

import ColorText
import Obj
import Types
import Template
import ArrayTemplates
import Commands
import Parsing
import Eval

-- | These modules will be loaded in order before any other code is evaluated.
coreModules :: String -> [String]
coreModules carpDir = map (\s -> carpDir ++ "/core/" ++ s ++ ".carp") [ "Interfaces"
                                                                      , "Macros"
                                                                      , "Dynamic"
                                                                      , "Format"
                                                                      , "Int"
                                                                      , "Long"
                                                                      , "Double"
                                                                      , "Float"
                                                                      , "Array"
                                                                      , "Char"
                                                                      , "Bool"
                                                                      , "String"
                                                                      , "IO"
                                                                      , "System"
                                                                      , "Pattern"
                                                                      ]

-- | The array module contains functions for working with the Array type.
arrayModule :: Env
arrayModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "Array", envUseModules = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ templateNth
                                , templateAllocate
                                , templateEMap
                                , templateFilter
                                , templateRaw
                                , templateAset
                                , templateAsetBang
                                , templateCount
                                , templatePushBack
                                , templatePopBack
                                , templateDeleteArray
                                , templateCopyArray
                                , templateStrArray
                                , templateSort
                                ]

-- | The Pointer module contains functions for dealing with pointers.
pointerModule :: Env
pointerModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "Pointer", envUseModules = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ templatePointerCopy ]

-- | A template function for copying (= deref:ing) any pointer.
templatePointerCopy :: (String, Binder)
templatePointerCopy = defineTemplate
  (SymPath ["Pointer"] "copy")
  (FuncTy [RefTy (PointerTy (VarTy "p"))] (PointerTy (VarTy "p")))
  (toTemplate "$p* $NAME ($p** ptrRef)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    return *ptrRef;"
                        ,"}"])
  (const [])

-- | The System module contains functions for various OS related things like timing and process control.
systemModule :: Env
systemModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "System", envUseModules = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ templateExit ]

-- | A template function for exiting.
templateExit :: (String, Binder)
templateExit = defineTemplate
  (SymPath ["System"] "exit")
  (FuncTy [IntTy] (VarTy "a"))
  (toTemplate "$a $NAME (int code)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    exit(code);"
                        ,"}"])
  (const [])

-- | The dynamic module contains dynamic functions only available in the repl and during compilation.
dynamicModule :: Env
dynamicModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "Dynamic", envUseModules = [], envMode = ExternalEnv }
  where bindings = Map.fromList $
                    [ addCommand "list?" 1 commandIsList
                    , addCommand "symbol?" 1 commandIsSymbol
                    , addCommand "count" 1 commandCount
                    , addCommand "car" 1 commandCar
                    , addCommand "cdr" 1 commandCdr
                    , addCommand "last" 1 commandLast
                    , addCommand "all-but-last" 1 commandAllButLast
                    , addCommand "cons" 2 commandCons
                    , addCommand "cons-last" 2 commandConsLast
                    , addCommand "append" 2 commandAppend
                    , addCommand "macro-error" 1 commandMacroError
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
                    , addCommand "build" 0 commandBuild
                    , addCommand "reload" 0 commandReload
                    , addCommand "env" 0 commandListBindings
                    , addCommandConfigurable "help" Nothing commandHelp
                    , addCommand "project" 0 commandProject
                    , addCommand "load" 1 commandLoad
                    , addCommand "macro-log" 1 commandPrint
                    , addCommand "expand" 1 commandExpand
                    , addCommand "project-set!" 2 commandProjectSet
                    , addCommand "os" 0 commandOS
                    , addCommand "system-include" 1 commandAddSystemInclude
                    , addCommand "local-include" 1 commandAddLocalInclude
                    ]
                    ++ [("String", Binder (XObj (Mod dynamicStringModule) Nothing Nothing))
                       ,("Project", Binder (XObj (Mod dynamicProjectModule) Nothing Nothing))
                       ]

-- | A submodule of the Dynamic module. Contains functions for working with strings in the repl or during compilation.
dynamicStringModule :: Env
dynamicStringModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "String", envUseModules = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ addCommand "char-at" 2 commandCharAt
                                , addCommand "index-of" 2 commandIndexOf
                                , addCommand "substring" 3 commandSubstring
                                , addCommand "count" 1 commandStringCount
                                ]

-- | A submodule of the Dynamic module. Contains functions for working with the active Carp project.
dynamicProjectModule :: Env
dynamicProjectModule = Env { envBindings = bindings, envParent = Nothing, envModuleName = Just "Project", envUseModules = [], envMode = ExternalEnv }
  where bindings = Map.fromList [ addCommand "config" 2 commandProjectConfig
                                ]

-- | The global environment before any code is run.
startingGlobalEnv :: Bool -> Env
startingGlobalEnv noArray =
  Env { envBindings = bindings,
        envParent = Nothing,
        envModuleName = Nothing,
        envUseModules = [SymPath [] "String"],
        envMode = ExternalEnv
      }
  where bindings = Map.fromList $ [ register "and" (FuncTy [BoolTy, BoolTy] BoolTy)
                                  , register "or" (FuncTy [BoolTy, BoolTy] BoolTy)
                                  , register "not" (FuncTy [BoolTy] BoolTy)
                                  , register "NULL" (VarTy "a")
                                  ]
                   ++ (if noArray then [] else [("Array", Binder (XObj (Mod arrayModule) Nothing Nothing))])
                   ++ [("Pointer", Binder (XObj (Mod pointerModule) Nothing Nothing))]
                   ++ [("System", Binder (XObj (Mod systemModule) Nothing Nothing))]
                   ++ [("Dynamic", Binder (XObj (Mod dynamicModule) Nothing Nothing))]

-- | The type environment (containing deftypes and interfaces) before any code is run.
startingTypeEnv :: Env
startingTypeEnv = Env { envBindings = bindings
                      , envParent = Nothing
                      , envModuleName = Nothing
                      , envUseModules = []
                      , envMode = ExternalEnv
                      }
  where bindings = Map.fromList
          $ [ interfaceBinder "copy" (FuncTy [(RefTy (VarTy "a"))] (VarTy "a")) [SymPath ["Array"] "copy", SymPath ["Pointer"] "copy"] builtInSymbolInfo
            , interfaceBinder "str" (FuncTy [(VarTy "a")] StringTy) [SymPath ["Array"] "str"] builtInSymbolInfo
            , interfaceBinder "prn" (FuncTy [(VarTy "a")] StringTy) [] builtInSymbolInfo
            ]
        builtInSymbolInfo = Info (-1) (-1) "Built-in." Set.empty (-1)

-- | Create a binder for an interface definition.
interfaceBinder :: String -> Ty -> [SymPath] -> Info -> (String, Binder)
interfaceBinder name t paths i = (name, Binder (defineInterface name t paths (Just i)))
