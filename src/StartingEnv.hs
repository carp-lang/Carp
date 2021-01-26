module StartingEnv where

import qualified ArrayTemplates
import Commands
import Eval
import Info
import qualified Map
import Obj
import Primitives
import qualified Set
import qualified StaticArrayTemplates
import Template
import ToTemplate
import Types

-- | These modules will be loaded in order before any other code is evaluated.
coreModules :: String -> [String]
coreModules carpDir = [carpDir ++ "/core/Core.carp"]

-- | The array module contains functions for working with the Array type.
arrayModule :: Env
arrayModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "Array",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    bindings =
      Map.fromList
        [ ArrayTemplates.templateNth,
          ArrayTemplates.templateAllocate,
          ArrayTemplates.templateEMap,
          ArrayTemplates.templateEFilter,
          ArrayTemplates.templateRaw,
          ArrayTemplates.templateUnsafeRaw,
          ArrayTemplates.templateAset,
          ArrayTemplates.templateAsetBang,
          ArrayTemplates.templateAsetUninitializedBang,
          ArrayTemplates.templateLength,
          ArrayTemplates.templatePushBack,
          ArrayTemplates.templatePushBackBang,
          ArrayTemplates.templatePopBack,
          ArrayTemplates.templatePopBackBang,
          ArrayTemplates.templateDeleteArray,
          ArrayTemplates.templateCopyArray,
          ArrayTemplates.templateStrArray
        ]

-- | The static array module
staticArrayModule :: Env
staticArrayModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "StaticArray",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    bindings =
      Map.fromList
        [ StaticArrayTemplates.templateUnsafeNth,
          StaticArrayTemplates.templateLength,
          StaticArrayTemplates.templateDeleteArray,
          StaticArrayTemplates.templateAsetBang,
          StaticArrayTemplates.templateStrArray
        ]

-- | The Pointer module contains functions for dealing with pointers.
pointerModule :: Env
pointerModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "Pointer",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    bindings =
      Map.fromList
        [ templatePointerCopy
        ]

-- | A template function for copying (= deref:ing) any pointer.
templatePointerCopy :: (String, Binder)
templatePointerCopy =
  defineTemplate
    (SymPath ["Pointer"] "copy")
    (FuncTy [RefTy (PointerTy (VarTy "p")) (VarTy "q")] (PointerTy (VarTy "p")) StaticLifetimeTy)
    "copies a pointer `p`."
    (toTemplate "$p* $NAME ($p** ptrRef)")
    ( toTemplate $
        unlines
          [ "$DECL {",
            "    return *ptrRef;",
            "}"
          ]
    )
    (const [])

maxArity :: Int
maxArity = 9

-- | The Function module contains functions for dealing with functions.
functionModule :: Env
functionModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "Function",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    bindEnv env =
      let Just name = envModuleName env
       in (name, Binder emptyMeta (XObj (Mod env) Nothing Nothing))
    bindings = Map.fromList (map (bindEnv . generateInnerFunctionModule) [0 .. maxArity])

-- | Each arity of functions need their own module to enable copying and string representation
generateInnerFunctionModule :: Int -> Env
generateInnerFunctionModule arity =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just ("Arity" ++ show arity),
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    alphabet = ['d' .. 'y']
    charToTyName c = [c]
    funcTy = FuncTy (take arity (map (VarTy . charToTyName) alphabet)) (VarTy "z") StaticLifetimeTy
    bindings =
      Map.fromList
        [ generateTemplateFuncCopy funcTy,
          generateTemplateFuncDelete funcTy,
          generateTemplateFuncStrOrPrn "str" "converts a function to a string." funcTy,
          generateTemplateFuncStrOrPrn "prn" "converts a function to a string (internal representation)." funcTy
        ]

-- | A template function for generating 'copy' functions for function pointers.
generateTemplateFuncCopy :: Ty -> (String, Binder)
generateTemplateFuncCopy funcTy =
  defineTemplate
    (SymPath ["Function"] "copy")
    (FuncTy [RefTy funcTy (VarTy "q")] (VarTy "a") StaticLifetimeTy)
    "copies a function."
    (toTemplate "$a $NAME ($a* ref)")
    ( toTemplate $
        unlines
          [ "$DECL {",
            "    if(ref->env) {",
            "        $a f_copy;",
            "        f_copy.callback = ref->callback;",
            "        f_copy.delete = ref->delete;",
            "        f_copy.copy = ref->copy;",
            "        f_copy.env = ((void*(*)(void*))ref->copy)(ref->env);",
            "        return f_copy;",
            "    } else {",
            "        return *ref;",
            "    }",
            "}"
          ]
    )
    (const [])

-- | A template function for generating 'deleter' functions for function pointers.
generateTemplateFuncDelete :: Ty -> (String, Binder)
generateTemplateFuncDelete funcTy =
  defineTemplate
    (SymPath ["Function"] "delete")
    (FuncTy [funcTy] UnitTy StaticLifetimeTy)
    "deletes a function."
    (toTemplate "void $NAME (Lambda f)")
    ( toTemplate $
        unlines
          [ "$DECL {",
            "  if(f.delete) {",
            "      ((void(*)(void*))f.delete)(f.env);",
            "      CARP_FREE(f.env);",
            "  }",
            "}"
          ]
    )
    (const [])

-- | A template function for generating 'str' or 'prn' functions for function pointers.
generateTemplateFuncStrOrPrn :: String -> String -> Ty -> (String, Binder)
generateTemplateFuncStrOrPrn name docs funcTy =
  defineTemplate
    (SymPath ["Function"] name)
    (FuncTy [RefTy funcTy (VarTy "q")] StringTy StaticLifetimeTy)
    docs
    (toTemplate "String $NAME (Lambda *f)")
    ( toTemplate $
        unlines
          [ "$DECL {",
            "    static String lambda = \"λ\";",
            "    return String_copy(&lambda);",
            "}"
          ]
    )
    (const [])

-- | The dynamic module contains dynamic functions only available in the repl and during compilation.
dynamicModule :: Env
dynamicModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "Dynamic",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    path = ["Dynamic"]
    spath = SymPath path
    bindings =
      Map.fromList $ nullaries ++ unaries ++ binaries ++ variadics ++ unaries' ++ binaries' ++ ternaries' ++ quaternaries' ++ variadics' ++ mods
    nullaries =
      let f = addNullaryCommand . spath
       in [ f "quit" commandQuit "quits the program." "(quit)",
            f "cat" commandCat "spits out the generated C code." "(cat)",
            f "run" commandRunExe "runs the built executable." "(run)",
            f "build" (commandBuild False) "builds the current code to an executable." "(build)",
            f "reload" commandReload "reloads all currently loaded files that weren’t marked as only loading once (see `load` and `load-once`)." "(reload)",
            f "env" commandListBindings "lists all current bindings." "(env)",
            f "project" commandProject "prints the current project state." "(project)",
            f "host-arch" commandHostArch "prints the host architecture (as returned by the Haskell function `System.Info.arch`)." "(host-arch)",
            f "host-os" commandHostOS "prints the host operating system (as returned by the Haskell function `System.Info.os`)." "(host-os)",
            f "host-bit-width" commandHostBitWidth "gets the bit width of the host platform." "(host-bit-width) ; => your host machine’s bit width, e.g. 32 or 64"
          ]
    unaries =
      let f = addUnaryCommand . spath
       in [ f "parse" commandParse "parses a string into an expression" "(parse \"(+ 1 2)\") ; => (+ 1 2)",
            f "length" commandLength "returns the length of the argument (must be an array, string or list)." "(length '(1 2 3)) ; => 3",
            f "car" commandCar "gets the head of a list or array." "(car '(1 2 3)) ; => 1",
            f "cdr" commandCdr "gets the tail of a list or array." "(cdr '(1 2 3)) ; => '(2 3)",
            f "last" commandLast "gets the last element of a list or array." "(last '(1 2 3)) ; => 3",
            f "all-but-last" commandAllButLast "gets all elements except for the last one of a list or array." "(all-but-last '(1 2 3)) ; => '(1 2)",
            f "macro-error" commandMacroError "logs an error and errors out of a macro." "(macro-error \"this is wrong\")",
            f "not" commandNot "negates its boolean argument." "(not false) ; => true",
            f "c" commandC "prints the C code emitted for a binding." "(c '(+ 2 3)) ; => int _3 = Int__PLUS_(2, 3);",
            f "expand" commandExpand "expands a macro and prints the result." "(expand '(when true 1)) ; => (if true 1 ())",
            f "system-include" commandAddSystemInclude "adds a system include, i.e. a C `#include` with angle brackets (`<>`)." "(system-include \"stdint.h\")",
            f "relative-include" commandAddRelativeInclude "adds a relative include, i.e. a C `include` with quotes. It also prepends the current directory." "(relative-include \"myheader.h\")",
            f "save-docs-internal" commandSaveDocsInternal "is the internal companion command to `save-docs`. `save-docs` should be called instead." "(save-docs-internal 'Module)",
            f "read-file" commandReadFile "reads a file into a string." "(read-file \"myfile.txt\")",
            f "hash" commandHash "calculates the hash associated with a value." "(hash '('my 'value)) ; => 3175346968842793108",
            f "round" commandRound "rounds its numeric argument." "(round 2.4) ; => 2",
            f "dynamic-type" commandType "Gets the dynamic type as a string." "(dynamic-type '()) ; => \"list\""
          ]
    binaries =
      let f = addBinaryCommand . spath
       in [ f "cons" commandCons "adds an element to the front of an array or list" "(cons 1 '(2 3)) ; => '(1 2 3)",
            f "cons-last" commandConsLast "adds an element to the back of an array or list" "(cons-last 3 '(1 2)) ; => '(1 2 3)",
            f "append" commandAppend "appends two lists or arrays." "(append '(1 2) '(3 4)) ; => '(1 2 3 4)",
            f "=" commandEq "compares its arguments for equality." "(= 1 2) ; => false",
            f "<" commandLt "checks whether its first argument is less than its second." "(< 1 2) ; => true",
            f ">" commandGt "checks whether its first argument is greater than its second." "(> 1 2) ; => false",
            f "+" commandPlus "adds its two arguments." "(+ 1 2) ; => 3",
            f "-" commandMinus "subtracts its second argument from its first." "(- 1 2) ; => -1",
            f "/" commandDiv "divides its first argument by its second." "(/ 4 2) ; => 2",
            f "*" commandMul "multiplies its two arguments." "(* 2 3) ; => 6",
            f "write-file" commandWriteFile "writes a string to a file." "(write-file \"myfile\" \"hello there!\")"
          ]
    variadics =
      let f = addVariadicCommand . spath
       in [ f "array" commandArray "creates an array from a collection of elements." "(array 1 2 3) ; => [1 2 3]",
            f "run-with-args" commandRunWithArgs "runs the built executable with arguments." "(run-with-args 1 2 3)",
            f "run-exe-with-args" commandRunExeWithArgs "runs an executable with arguments." "(run-exe-with-args \"path-to-executable\" 1 2 3)",
            f "list" commandList "creates an array from a collection of elements." "(list 1 2 3) ; => (1 2 3)",
            f "macro-log" commandMacroLog "logs a message in a macro." "(macro-log \"this will be printed at compile time\")",
            f "str" commandStr "stringifies its arguments." "(str 1 \" \" 2 \" \" 3) ; => \"1 2 3\"",
            f "s-expr" commandSexpression "returns the s-expression associated with a binding. When the binding is a type, the deftype form is returned instead of the type's module by default. Pass an optional bool argument to explicitly request the module for a type instead of its definition form. If the bool is true, the module for the type will be returned. Returns an error when no definition is found for the binding." "(s-expr foo), (s-expr foo true)",
            f "load" commandLoad "loads a file into the current environment." "(load \"myfile.carp\")\n(load \"myrepo@version\" \"myfile\")",
            f "load-once" commandLoadOnce "loads a file and prevents it from being reloaded (see `reload`)." "(load-once \"myfile.carp\")\n(load \"myrepo@version\" \"myfile\")"
          ]
    unaries' =
      let f = makeUnaryPrim . spath
       in [ f "quote" (\_ ctx x -> pure (ctx, Right x)) "quotes any value." "(quote x) ; where x is an actual symbol",
            f "info" primitiveInfo "prints all information associated with a symbol." "(info mysymbol)",
            f "managed?" primitiveIsManaged "" "",
            f "members" primitiveMembers "returns the members of a type as an array." "(members MyType)",
            f "use" primitiveUse "uses a module, i.e. imports the symbols inside that module into the current module." "(use MyModule)",
            f "eval" primitiveEval "evaluates a list." "(eval mycode)",
            f "defined?" primitiveDefined "checks whether a symbol is defined." "(defined? mysymbol)",
            f "type" primitiveType "prints the type of a symbol." "(type mysymbol)",
            f "kind" primitiveKind "prints the kind of a symbol." "(kind mysymbol)"
          ]
    binaries' =
      let f = makeBinaryPrim . spath
       in [ f "defdynamic" primitiveDefdynamic "defines a new dynamic value, i.e. a value available at compile time." "(defdynamic name value)",
            f "meta" primitiveMeta "gets the value under `\"mykey\"` in the meta map associated with a symbol. It returns `()` if the key isn’t found." "(meta mysymbol \"mykey\")",
            f "definterface" primitiveDefinterface "defines a new interface (which could be a function or symbol)." "(definterface mysymbol MyType)",
            f "implements" primitiveImplements "designates a function as an implementation of an interface." "(implements zero Maybe.zero)"
          ]
    ternaries' =
      let f = makeTernaryPrim . spath
       in [ f "defmacro" primitiveDefmacro "defines a new macro." "(defmacro name [args :rest restargs] body)",
            f "defndynamic" primitiveDefndynamic "defines a new dynamic function, i.e. a function available at compile time." "(defndynamic name [args] body)",
            f "meta-set!" primitiveMetaSet "sets a new key and value pair on the meta map associated with a symbol." "(meta-set! mysymbol \"mykey\" \"myval\")"
          ]
    quaternaries' =
      let f = makeQuaternaryPrim . spath
       in [ f "deftemplate" primitiveDeftemplate "defines a new C template." "(deftemplate symbol Type declString defString)"
          ]
    variadics' =
      let f = makeVariadicPrim . spath
       in [ f "file" primitiveFile "returns the file a symbol was defined in." "(file mysymbol)",
            f "line" primitiveLine "returns the line a symbol was defined on." "(line mysymbol)",
            f "column" primitiveColumn "returns the column a symbol was defined on." "(column mysymbol)",
            f "register-type" primitiveRegisterType "registers a new type from C." "(register-type Name <optional: c-name> <optional: members>)",
            f "defmodule" primitiveDefmodule "defines a new module in which `expressions` are defined." "(defmodule MyModule <expressions>)",
            f "register" primitiveRegister "registers a new function. This is used to define C functions and other symbols that will be available at link time." "(register name <signature> <optional: override>)",
            f "deftype" primitiveDeftype "defines a new sumtype or struct." "(deftype Name <members>)",
            f "help" primitiveHelp "prints help." "(help)"
          ]
    mods =
      [ ("String", Binder emptyMeta (XObj (Mod dynamicStringModule) Nothing Nothing)),
        ("Symbol", Binder emptyMeta (XObj (Mod dynamicSymModule) Nothing Nothing)),
        ("Project", Binder emptyMeta (XObj (Mod dynamicProjectModule) Nothing Nothing)),
        ("Path", Binder emptyMeta (XObj (Mod dynamicPathModule) Nothing Nothing))
      ]

-- | A submodule of the Dynamic module. Contains functions for working with strings in the repl or during compilation.
dynamicStringModule :: Env
dynamicStringModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "String",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    path = ["Dynamic", "String"]
    bindings =
      Map.fromList $unaries ++ binaries ++ ternaries
    spath = SymPath path
    unaries =
      let f = addUnaryCommand . spath
       in [ f "length" commandStringLength "gets the length of a string." "(String.length \"hi\") ; => 2",
            f "concat" commandStringConcat "concatenates a list of strings together." "(String.concat [\"hi \" \"there\"]) ; => \"hi there\""
          ]
    binaries =
      let f = addBinaryCommand . spath
       in [ f "char-at" commandCharAt "gets the nth character of a string." "(String.char-at \"hi\" 1) ; => \\i",
            f "index-of" commandIndexOf "gets the index of a character in a string (or returns `-1` if the character is not found)." "(index-of \"hi\" \\i) ; => 1",
            f "split-on" commandStringSplitOn "split a string at separator." "(String.split-on \"-\" \"hi-there\") ; => [\"hi \" \"there\"]"
          ]
    ternaries =
      let f = addTernaryCommand . spath
       in [f "slice" commandSubstring "creates a substring from a beginning index to an end index." "(String.slice \"hello\" 1 3) ; => \"ell\""]

-- | A submodule of the Dynamic module. Contains functions for working with symbols in the repl or during compilation.
dynamicSymModule :: Env
dynamicSymModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "Symbol",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    path = ["Dynamic", "Symbol"]
    bindings = Map.fromList $unaries ++ binaries
    spath = SymPath path
    unaries =
      let f = addUnaryCommand . spath
       in [ f "concat" commandSymConcat "concatenates a list of symbols together." "(Symbol.concat ['x 'y 'z]) ; => 'xyz",
            f "from" commandSymFrom "converts a variety of types to a symbol." "(Symbol.from true) ; => True",
            f "str" commandSymStr "converts a symbol to a string." "(Symbol.str 'x) ; => \"x\""
          ]
    binaries =
      let f = addBinaryCommand . spath
       in [ f "prefix" commandSymPrefix "prefixes a symbol with a module." "(Symbol.prefix 'Module 'fun) ; => Module.fun"
          ]

-- | A submodule of the Dynamic module. Contains functions for working with the active Carp project.
dynamicProjectModule :: Env
dynamicProjectModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "Project",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    path = ["Dynamic", "Project"]
    bindings = Map.fromList $unaries ++ binaries
    spath = SymPath path
    unaries =
      let f = addUnaryCommand . spath
       in [ f "get-config" commandProjectGetConfig "gets a project config value under a key." "(Project.get-config \"paren-balance-hints\")"
          ]
    binaries =
      let f = addBinaryCommand . spath
       in [ f "config" commandProjectConfig "sets a project config key." "(Project.config \"paren-balance-hints\" false)"
          ]

-- | A submodule of the Dynamic module. Contains functions for working with paths.
dynamicPathModule :: Env
dynamicPathModule =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Just "Path",
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    path = ["Dynamic", "Path"]
    bindings = Map.fromList unaries
    spath = SymPath path
    unaries =
      let f = addUnaryCommand . spath
       in [ f "directory" commandPathDirectory "takes the basename of a string taken to be a filepath.\n\nHistorical note: this is a command because it used to power one of the `include` macros." "(Path.directory \"dir/file\") ; => \"dir\"",
            f "absolute" commandPathAbsolute "converts a filepath to absolute." "(Path.absolute \"dir/file\") ; => \"/home/foo/dir/file\""
          ]

-- | The global environment before any code is run.
startingGlobalEnv :: Bool -> Env
startingGlobalEnv noArray =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Nothing,
      envUseModules = Set.fromList [SymPath [] "String"],
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    bindings =
      Map.fromList $
        [ register "NULL" (PointerTy (VarTy "a"))
        ]
          ++ [("Array", Binder emptyMeta (XObj (Mod arrayModule) Nothing Nothing)) | not noArray]
          ++ [("StaticArray", Binder emptyMeta (XObj (Mod staticArrayModule) Nothing Nothing))]
          ++ [("Pointer", Binder emptyMeta (XObj (Mod pointerModule) Nothing Nothing))]
          ++ [("Dynamic", Binder emptyMeta (XObj (Mod dynamicModule) Nothing Nothing))]
          ++ [("Function", Binder emptyMeta (XObj (Mod functionModule) Nothing Nothing))]

-- | The type environment (containing deftypes and interfaces) before any code is run.
startingTypeEnv :: Env
startingTypeEnv =
  Env
    { envBindings = bindings,
      envParent = Nothing,
      envModuleName = Nothing,
      envUseModules = Set.empty,
      envMode = ExternalEnv,
      envFunctionNestingLevel = 0
    }
  where
    bindings =
      Map.fromList
        [ interfaceBinder
            "delete"
            (FuncTy [VarTy "a"] UnitTy StaticLifetimeTy)
            ([SymPath ["Array"] "delete", SymPath ["StaticArray"] "delete"] ++ registerFunctionFunctionsWithInterface "delete")
            builtInSymbolInfo,
          interfaceBinder
            "copy"
            (FuncTy [RefTy (VarTy "a") (VarTy "q")] (VarTy "a") StaticLifetimeTy)
            ([SymPath ["Array"] "copy", SymPath ["Pointer"] "copy"] ++ registerFunctionFunctionsWithInterface "copy")
            builtInSymbolInfo,
          interfaceBinder
            "str"
            (FuncTy [VarTy "a"] StringTy StaticLifetimeTy)
            (SymPath ["Array"] "str" : SymPath ["StaticArray"] "str" : registerFunctionFunctionsWithInterface "str")
            builtInSymbolInfo,
          interfaceBinder
            "prn"
            (FuncTy [VarTy "a"] StringTy StaticLifetimeTy)
            (SymPath ["StaticArray"] "str" : registerFunctionFunctionsWithInterface "prn") -- QUESTION: Where is 'prn' for dynamic Array:s registered? Can't find it... (but it is)
            builtInSymbolInfo
        ]
    builtInSymbolInfo = Info (-1) (-1) "Built-in." Set.empty (-1)

-- | Make the functions in the Function.Arity<N> modules register with the interfaces in the type Env.
registerFunctionFunctionsWithInterface :: String -> [SymPath]
registerFunctionFunctionsWithInterface interfaceName =
  map (\arity -> SymPath ["Function", "Arity" ++ show arity] interfaceName) [0 .. maxArity]

-- | Create a binder for an interface definition.
interfaceBinder :: String -> Ty -> [SymPath] -> Info -> (String, Binder)
interfaceBinder name t paths i = (name, Binder emptyMeta (defineInterface name t paths (Just i)))
