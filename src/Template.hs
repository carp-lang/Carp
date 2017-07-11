{-# LANGUAGE LambdaCase #-}

module Template where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Debug.Trace

import Util
import Types
import Obj
import Parsing
import Infer

-- | Templates are instructions for the compiler to generate some C-code 
-- | based on some template and the names and types to fill into the template.
-- | Templates are generic and need to be given an explicit type to generate the
-- | correct code.

-- | Example:
-- | template1 : ((Array T) -> Int) = "int length__T(<T> xs) { return xs->len; }"
-- | Given the type ((Array Float) -> Int) the following code is produced:
-- | "int length__Float(Array__Float xs) { return xs->len; }"

-- | Create a binding pair used for adding a template definition to an environment.
defineTemplate :: SymPath -> Ty -> [Token] -> [Token] -> (Ty -> [XObj]) -> (String, Binder)
defineTemplate path t declaration definition depsFunc = 
  let (SymPath _ name) = path
      template = Template t (const declaration) (const definition) depsFunc
      defLst = [XObj (Deftemplate (TemplateCreator (\_ _ -> template))) Nothing Nothing, XObj (Sym path) Nothing Nothing]
  in  (name, Binder (XObj (Lst defLst) Nothing (Just t)))

-- | The more advanced version of a template, where the code can vary depending on the type.
defineTypeParameterizedTemplate :: TemplateCreator -> SymPath -> Ty -> (String, Binder)
defineTypeParameterizedTemplate templateCreator path t = 
  let (SymPath _ name) = path
      defLst = [XObj (Deftemplate templateCreator) Nothing Nothing, XObj (Sym path) Nothing Nothing]
  in  (name, Binder (XObj (Lst defLst) Nothing (Just t)))

-- | Create a binding pair used for adding a template instantiation to an environment.
instanceBinder :: SymPath -> Ty -> Template -> (String, Binder)
instanceBinder path@(SymPath _ name) actualType template =
  let (x, _) = instantiateTemplate path actualType template
  in  (name, Binder x)

-- -- | Create a binding pair and don't discard the dependencies
instanceBinderWithDeps :: SymPath -> Ty -> Template -> ((String, Binder), [XObj])
instanceBinderWithDeps path@(SymPath _ name) actualType template =
  let (x, deps) = instantiateTemplate path actualType template
  in  ((name, Binder x), deps)

-- | Concretizes the types used in @token
--   @cName is the name of the definition, i.e. the "foo" in "void foo() { ... }"
concretizeTypesInToken :: TypeMappings -> String -> [Token] -> Token -> [Token]
concretizeTypesInToken mappings cName decl token =
  case token of
    TokDecl -> concatMap (concretizeTypesInToken mappings cName (error "Nope.")) decl
    TokName -> [TokC cName]
    TokTy t -> [TokTy (replaceTyVars mappings t)]
    _ -> [token]

-- | High-level helper function for creating templates from strings of C code.
toTemplate :: String -> [Token]
toTemplate text = case Parsec.runParser templateSyntax 0 "(template)" text of
                    Right ok -> ok
                    Left err -> compilerError (show err)
  where
    templateSyntax :: Parsec.Parsec String Int [Token]
    templateSyntax = Parsec.many parseTok
    
    parseTok = Parsec.try parseTokDecl <|>      --- $DECL
               Parsec.try parseTokName <|>      --- $NAME
               Parsec.try parseTokTyGrouped <|> --- i.e. $(Fn [Int] t)
               Parsec.try parseTokTy <|>        --- i.e. $t               
               parseTokC                        --- Anything else...
    
    parseTokDecl :: Parsec.Parsec String Int Token
    parseTokDecl = do _ <- Parsec.string "$DECL"
                      return TokDecl
    
    parseTokName :: Parsec.Parsec String Int Token
    parseTokName = do _ <- Parsec.string "$NAME"
                      return TokName
    
    parseTokC :: Parsec.Parsec String Int Token
    parseTokC = do s <- Parsec.many1 validInSymbol
                   return (TokC s)
      where validInSymbol = Parsec.choice [Parsec.letter, Parsec.digit, Parsec.oneOf validCharactersInTemplate]
            validCharactersInTemplate = " ><{}()[]|;:.,_-+*#/'^!?€%&=@\"\n\t"

    parseTokTy :: Parsec.Parsec String Int Token
    parseTokTy = do _ <- Parsec.char '$'
                    s <- Parsec.many1 Parsec.letter
                    return (toTokTy s)
    
    parseTokTyGrouped :: Parsec.Parsec String Int Token
    parseTokTyGrouped = do _ <- Parsec.char '$'
                           _ <- Parsec.char '(' 
                           Parsec.putState 1 -- One paren to close.
                           s <- fmap ('(' :) (Parsec.many parseCharBalanced)
                           -- Note: The closing paren is read by parseCharBalanced.
                           return (toTokTy s)

    parseCharBalanced :: Parsec.Parsec String Int Char
    parseCharBalanced = do balanceState <- Parsec.getState
                           if balanceState > 0
                             then Parsec.try openParen <|>
                                  Parsec.try closeParen <|>
                                  Parsec.anyChar
                             else Parsec.char '\0' -- Should always fail which will end the string.

    openParen :: Parsec.Parsec String Int Char
    openParen = do _ <- Parsec.char '('
                   Parsec.modifyState (+1)
                   return '('
                    
    closeParen :: Parsec.Parsec String Int Char
    closeParen = do _ <- Parsec.char ')'
                    Parsec.modifyState (\x -> x - 1)
                    return ')'

toTokTy :: String -> Token
toTokTy s =
  case parse s of
    Left err -> compilerError (show err)
    Right [] -> compilerError ("toTokTy got [] when parsing: '" ++ s ++ "'")
    Right [xobj] -> case xobjToTy xobj of
                      Just ok -> TokTy ok
                      Nothing -> compilerError ("toTokTy failed to convert this s-expression to a type: " ++ pretty xobj)
    Right xobjs -> compilerError ("toTokTy parsed too many s-expressions: " ++ joinWithSpace (map pretty xobjs))

----------------------------------------------------------------------------------------------------------
-- | ACTUAL TEMPLATES:

templateCopyingMap :: (String, Binder)
templateCopyingMap = defineTypeParameterizedTemplate templateCreator path t
  where fTy = FuncTy [VarTy "a"] (VarTy "b")
        aTy = StructTy "Array" [VarTy "a"]
        bTy = StructTy "Array" [VarTy "b"]
        path = SymPath ["Array"] "transform"
        t = FuncTy [fTy, aTy] bTy
        templateCreator = TemplateCreator $
          \typeEnv env -> 
            Template
            t
            (const (toTemplate "Array $NAME($(Fn [a] b) f, Array a)"))
            (\(FuncTy [(FuncTy [_] _), _] _) ->
               (toTemplate $ unlines $                
                  [ "$DECL { "
                  , "    Array b;"
                  , "    b.len = a.len;"
                  , "    b.data = CARP_MALLOC(sizeof($b) * a.len);"
                  , "    for(int i = 0; i < a.len; ++i) {"
                  , "        (($b*)b.data)[i] = f((($a*)a.data)[i]); "
                  , "    }"
                  , "    CARP_FREE(a.data);"
                  , "    return b;"
                  , "}"
                  ]))
            (\(FuncTy [ft@(FuncTy [insideTypeA] _), arrayTypeA] arrayTypeB) ->
               [defineFunctionTypeAlias ft, defineArrayTypeAlias arrayTypeA, defineArrayTypeAlias arrayTypeB] ++
                insideArrayDeleteDeps typeEnv env insideTypeA)

-- | "Endofunctor Map"
templateEMap :: (String, Binder)
templateEMap = 
  let fTy = FuncTy [VarTy "a"] (VarTy "a")
      aTy = StructTy "Array" [VarTy "a"]
      bTy = StructTy "Array" [VarTy "a"]
  in  defineTemplate
      (SymPath ["Array"] "map")
      (FuncTy [fTy, aTy] bTy)
      (toTemplate "Array $NAME($(Fn [a] a) f, Array a)")
      (toTemplate $ unlines
        ["$DECL { "
        ,"    for(int i = 0; i < a.len; ++i) {"
        ,"        (($a*)a.data)[i] = f((($a*)a.data)[i]); "
        ,"    }"
        ,"    return a;"
        ,"}"
        ])
      (\(FuncTy [t, arrayType] _) -> [defineFunctionTypeAlias t, defineArrayTypeAlias arrayType])

templateFilter :: (String, Binder)
templateFilter = defineTypeParameterizedTemplate templateCreator path t
  where
    fTy = FuncTy [VarTy "a"] BoolTy
    aTy = StructTy "Array" [VarTy "a"]
    path = SymPath ["Array"] "filter"
    t = (FuncTy [fTy, aTy] aTy)
    templateCreator = TemplateCreator $
      \typeEnv env ->
        Template
        t
        (const (toTemplate "Array $NAME($(Fn [a] Bool) predicate, Array a)"))
        (\(FuncTy [(FuncTy [insideTy] BoolTy), _] _) ->
           (toTemplate $ unlines $
            let deleter = insideArrayDeletion env insideTy
            in ["$DECL { "
               , "    int insertIndex = 0;"
               , "    for(int i = 0; i < a.len; ++i) {"
               , "        if(predicate((($a*)a.data)[i])) {"
               , "            ((($a*)a.data)[insertIndex++]) = (($a*)a.data)[i];"
               , "        } else {"
               , "        " ++ deleter
               , "        }"
               , "    }"
               , "    a.len = insertIndex;"
               , "    // NOTE: the array isn't resized for now, it probably should be?"
               , "    return a;"
               , "}"
               ]))
        (\(FuncTy [ft@(FuncTy [insideType] BoolTy), arrayType] _) ->
           [defineFunctionTypeAlias ft, defineArrayTypeAlias arrayType] ++ insideArrayDeleteDeps typeEnv env insideType)

templateReduce :: (String, Binder)
templateReduce = defineTypeParameterizedTemplate templateCreator path t
  where
    fTy = FuncTy [bTy, aTy] bTy
    arrTy = StructTy "Array" [aTy]
    aTy = VarTy "a"
    bTy = VarTy "b"
    path = SymPath ["Array"] "reduce"
    t = (FuncTy [fTy, bTy, arrTy] bTy)
    templateCreator = TemplateCreator $
      \_ _ ->
        Template
        t
        (const (toTemplate "$b $NAME($(Fn [b a] a) f, $b initial_value, Array a)"))
        (\(FuncTy [(FuncTy [_, _] _), _, _] _) ->
           (toTemplate $ unlines $
               [ "$DECL { "
               , "    $b b = initial_value;"
               , "    for(int i = 0; i < a.len; ++i) {"
               , "        b = f(b, (($a*)a.data)[i]);"
               , "    }"
               , "    CARP_FREE(a.data); // Can't call Array_delete since it will destroy the items that have been handed off to f()."
               , "    return b;"
               , "}"
               ]))
        (\(FuncTy [ft@(FuncTy [_, _] _), _, arrayType] _) ->
           [defineFunctionTypeAlias ft, defineArrayTypeAlias arrayType])

templatePushBack :: (String, Binder)
templatePushBack = 
  let aTy = StructTy "Array" [VarTy "a"]
      valTy = VarTy "a"
  in  defineTemplate
      (SymPath ["Array"] "push-back")
      (FuncTy [aTy, valTy] aTy)
      (toTemplate "Array $NAME(Array a, $a value)")
      (toTemplate $ unlines
        ["$DECL { "
        ,"    a.len++;"
        ,"    void *pre = a.data;"
        ,"    a.data = CARP_MALLOC(sizeof($a) * a.len);"
        ,"    CARP_FREE(pre);"
         --a.data = realloc(a.data, sizeof($a) * a.len);"
        ,"    (($a*)a.data)[a.len - 1] = value;"
        ,"    return a;"
        ,"}"
        ])
      (\(FuncTy [arrayType, _] _) -> [defineArrayTypeAlias arrayType])

templatePopBack :: (String, Binder)
templatePopBack = 
  let aTy = StructTy "Array" [VarTy "a"]
  in  defineTemplate
      (SymPath ["Array"] "pop-back")
      (FuncTy [aTy] aTy)
      (toTemplate "Array $NAME(Array a)")
      (toTemplate $ unlines
        ["$DECL { "
        ,"    a.len--;"
        ,"    if(a.len > 0) {"
        --,"        a.data = realloc(a.data, sizeof($a) * a.len);"
        ,"        void *pre = a.data;"
        ,"        a.data = CARP_MALLOC(sizeof($a) * a.len);"
        ,"        CARP_FREE(pre);"
        ,"    }"
        ,"    return a;"
        ,"}"
        ])
      (\(FuncTy [arrayType] _) -> [defineArrayTypeAlias arrayType])

templateNth :: (String, Binder)
templateNth =
  let t = VarTy "t"
  in defineTemplate
  (SymPath ["Array"] "nth")
  (FuncTy [RefTy (StructTy "Array" [t]), IntTy] (RefTy t))
  (toTemplate "$t* $NAME (Array *aRef, int n)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    Array a = *aRef;"
                        ,"    assert(n >= 0);"
                        ,"    assert(n < a.len);"
                        ,"    return &((($t*)a.data)[n]);"
                        ,"}"])
  (\(FuncTy [arrayType, _] _) -> [defineArrayTypeAlias arrayType])
  
templateReplicate :: (String, Binder)
templateReplicate = defineTemplate
  (SymPath ["Array"] "replicate")
  (FuncTy [IntTy, VarTy "t"] (StructTy "Array" [VarTy "t"]))
  (toTemplate "Array $NAME(int n, $t elem)")
  (toTemplate $ unlines [ "$DECL {"
                        , "    Array a; a.len = n; a.data = CARP_MALLOC(sizeof($t) * n);"
                        , "    for(int i = 0; i < n; ++i) {"
                        , "      (($t*)a.data)[i] = elem;"
                        , "    }"
                        , "    return a;"
                        , "}"])
  (\(FuncTy [_, _] arrayType) -> [defineArrayTypeAlias arrayType])

templateRepeat :: (String, Binder)
templateRepeat = defineTemplate
  (SymPath ["Array"] "repeat")
  (FuncTy [IntTy, (FuncTy [] (VarTy "t"))] (StructTy "Array" [VarTy "t"]))
  (toTemplate "Array $NAME(int n, $(Fn [] t) f)")
  (toTemplate $ unlines [ "$DECL {"
                        , "    Array a; a.len = n; a.data = CARP_MALLOC(sizeof($t) * n);"
                        , "    for(int i = 0; i < n; ++i) {"
                        , "      (($t*)a.data)[i] = f();"
                        , "    }"
                        , "    return a;"
                        , "}"])
  (\(FuncTy [_, t] arrayType) -> [defineArrayTypeAlias arrayType, defineFunctionTypeAlias t])

templateRaw :: (String, Binder)
templateRaw = defineTemplate
  (SymPath ["Array"] "raw")
  (FuncTy [StructTy "Array" [VarTy "t"]] (PointerTy (VarTy "t")))
  (toTemplate "$t* $NAME (Array a)")
  (toTemplate "$DECL { return a.data; }")
  (\(FuncTy [arrayType] _) -> [defineArrayTypeAlias arrayType])

templateAset :: (String, Binder)
templateAset = defineTemplate
  (SymPath ["Array"] "aset")
  (FuncTy [StructTy "Array" [VarTy "t"], IntTy, VarTy "t"] (StructTy "Array" [VarTy "t"]))
  (toTemplate "Array $NAME (Array a, int n, $t newValue)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    assert(n >= 0);"
                        ,"    assert(n < a.len);"
                        ,"    (($t*)a.data)[n] = newValue;"
                        ,"    return a;"
                        ,"}"])
  (\(FuncTy [arrayType, _, _] _) -> [defineArrayTypeAlias arrayType])

templateAsetBang :: (String, Binder)
templateAsetBang = defineTemplate
  (SymPath ["Array"] "aset!")
  (FuncTy [RefTy (StructTy "Array" [VarTy "t"]), IntTy, VarTy "t"] UnitTy)
  (toTemplate "void $NAME (Array *aRef, int n, $t newValue)")
  (toTemplate $ unlines ["$DECL {"
                        ,"    Array a = *aRef;"
                        ,"    assert(n >= 0);"
                        ,"    assert(n < a.len);"
                        ,"    (($t*)a.data)[n] = newValue;"
                        ,"}"])
  (\(FuncTy [arrayType, _, _] _) -> [defineArrayTypeAlias arrayType])
  
templateCount :: (String, Binder)
templateCount = defineTemplate
  (SymPath ["Array"] "count")
  (FuncTy [RefTy (StructTy "Array" [VarTy "t"])] IntTy)
  (toTemplate "int $NAME (Array *a)")
  (toTemplate "$DECL { return (*a).len; }")
  (\(FuncTy [arrayType] _) -> [defineArrayTypeAlias arrayType])

templateDeleteArray :: (String, Binder)
templateDeleteArray = defineTypeParameterizedTemplate templateCreator path t
  where templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "void $NAME (Array a)"))
             (\(FuncTy [arrayType] UnitTy) ->
                [TokDecl, TokC "{\n"] ++
                (deleteTy env arrayType) ++
                [TokC "}\n"])
             (\(FuncTy [arrayType@(StructTy "Array" [insideType])] UnitTy) ->
                defineArrayTypeAlias arrayType : insideArrayDeleteDeps typeEnv env insideType)
        path = SymPath ["Array"] "delete"
        t = (FuncTy [(StructTy "Array" [VarTy "a"])] UnitTy)
        
deleteTy :: Env -> Ty -> [Token]
deleteTy env (StructTy "Array" [innerType]) =
  [ TokC   "    for(int i = 0; i < a.len; i++) {\n"
  , TokC $ "    " ++ insideArrayDeletion env innerType
  , TokC   "    }\n"
  , TokC   "    CARP_FREE(a.data);\n"
  ]
deleteTy _ _ = []

insideArrayDeletion :: Env -> Ty -> String
insideArrayDeletion env t
  | isManaged t =
    case filter ((\(Just t') -> areUnifiable (FuncTy [t] UnitTy) t') . ty . binderXObj . snd) (multiLookupALL "delete" env) of
      [] -> "    /* Can't find any delete-function for type inside Array: '" ++ show t ++ "' */\n"
      [(_, Binder single)] ->
        let Just t' = ty single
            (SymPath pathStrings name) = getPath single
            suffix = polymorphicSuffix t' (FuncTy [t] UnitTy)
            concretizedPath = SymPath pathStrings (name ++ suffix)
        in  "    " ++ pathToC concretizedPath ++ "(((" ++ tyToC t ++ "*)a.data)[i]);\n"
      _ -> "    /* Can't find a single delete-function for type inside Array: '" ++ show t ++ "' */\n"
  | otherwise   = "    /* Ignore non-managed type inside Array: '" ++ show t ++ "' */\n"

templateNoop :: (String, Binder)
templateNoop = defineTemplate
  (SymPath [] "noop")
  (FuncTy [(PointerTy (VarTy "a"))] UnitTy)
  (toTemplate "void $NAME ($a* a)")
  (toTemplate "$DECL { }")
  (const [])







---------------------------

templateCopyArray :: (String, Binder)
templateCopyArray = defineTypeParameterizedTemplate templateCreator path t
  where templateCreator = TemplateCreator $
          \typeEnv env ->
             Template
             t
             (const (toTemplate "Array $NAME (Array* a)"))
             (\(FuncTy [(RefTy arrayType)] _) ->
                [TokDecl, TokC "{\n"] ++
                [TokC "    Array copy;\n"] ++
                [TokC "    copy.len = a->len;\n"] ++
                [TokC "    copy.data = CARP_MALLOC(sizeof(", TokTy (VarTy "a"), TokC ") * a->len);\n"] ++
                (copyTy env arrayType) ++
                [TokC "    return copy;\n"] ++
                [TokC "}\n"])
             (\case
                 (FuncTy [(RefTy arrayType@(StructTy "Array" [insideType]))] _) ->
                   defineArrayTypeAlias arrayType : insideArrayCopyDeps typeEnv env insideType
                 err ->
                   error ("CAN'T MATCH: " ++ (show err))
             )
        path = SymPath ["Array"] "copy"
        t = (FuncTy [(RefTy (StructTy "Array" [VarTy "a"]))] (StructTy "Array" [VarTy "a"]))
        
copyTy :: Env -> Ty -> [Token]
copyTy env (StructTy "Array" [innerType]) =
  [ TokC   "    for(int i = 0; i < a->len; i++) {\n"
  , TokC $ "    " ++ insideArrayCopying env innerType
  , TokC   "    }\n"
  ]
copyTy _ _ = []

insideArrayCopying :: Env -> Ty -> String
insideArrayCopying env t
  | isManaged t =
    case filter ((\(Just t') -> areUnifiable (FuncTy [(RefTy t)] t) t') . ty . binderXObj . snd) (multiLookupALL "copy" env) of
      [] -> "    /* Can't find any copy-function for type inside Array: '" ++ show t ++ "' */\n"
      [(_, Binder single)] ->
        let Just t' = ty single
            (SymPath pathStrings name) = getPath single
            suffix = polymorphicSuffix t' (FuncTy [(RefTy t)] t)
            concretizedPath = SymPath pathStrings (name ++ suffix)
        in  "    ((" ++ tyToC t ++ "*)(copy.data))[i] = " ++ pathToC concretizedPath ++ "(&(((" ++ tyToC t ++ "*)a->data)[i]));\n"
      _ -> "    /* Can't find a single copy-function for type inside Array: '" ++ show t ++ "' */\n"
  | otherwise   = "    /* Ignore non-managed type inside Array: '" ++ show t ++ "' */\n"
