-- | Module RecType defines routines for working with recursive data types.
module RecType
  (
   recursiveMembersToPointers,
   isValueRecursive,
   recursiveProductMakeBinder,
   recursiveProductInitBinder,
   recTemplateGetter,
   okRecursive,
   isRecursive,
  )
where

import Obj
import Types
import TypePredicates
import TypeError
import TypeCandidate
import TypesToC
import StructUtils
import Template
import Util
import Data.Maybe (fromJust)
import Concretize
import ToTemplate
import Validate

-- | Returns true if a type candidate is recursive.
isRecursive :: TypeCandidate -> Bool
isRecursive candidate =
  let memberTypes = concat $ map snd (typemembers candidate)
      vars = variables candidate
      name = typename candidate
   in any (check name vars) memberTypes
  where check :: String -> [Ty] -> Ty -> Bool
        check name vars t = isDirectRecursion name vars t || isIndirectRecursion name vars t

isDirectRecursion :: String -> [Ty] -> Ty -> Bool
isDirectRecursion name vars (StructTy (ConcreteNameTy (SymPath [] n)) rest) =
  (n == name && vars == rest)
isDirectRecursion name vars (RecTy t) = isDirectRecursion name vars t
isDirectRecursion _ _ _ = False

isIndirectRecursion :: String -> [Ty] -> Ty -> Bool
isIndirectRecursion name vars t@(StructTy _ rest) =
  not (isDirectRecursion name vars t) && any (isDirectRecursion name vars) rest
isIndirectRecursion name vars (PointerTy t) = isDirectRecursion name vars t
isIndirectRecursion name vars (RefTy t _) = isDirectRecursion name vars t
isIndirectRecursion _ _ _ = False

--------------------------------------------------------------------------------
-- Base indirection recursion

-- | Returns true if a candidate type definition is a valid instance of recursivity.
-- Types have valid recursion if they refer to themselves through indirection.
okRecursive :: TypeCandidate -> Either TypeError ()
okRecursive candidate =
  let name = typename candidate
      vars = variables candidate
      memberTypes = concat $ map snd (typemembers candidate)
      recursives = (filter (isIndirectRecursion name vars) memberTypes)
      ty = StructTy (ConcreteNameTy (SymPath [] name)) vars
      constraints = map (recInterfaceConstraints ty) recursives
   in validateInterfaceConstraints (candidate {interfaceConstraints = concat constraints})

-- | Generates interface constraints for a recursive type.
-- The recursive portion of recursive types must be wrapped in a type F that supports indirection.
-- We enforce this with two interfaces:
--   allocate: Heap allocates a value T and wraps it in type F<T>
--   indirect: Returns T from a heap allocated F<T>
recInterfaceConstraints :: Ty -> Ty -> [InterfaceConstraint]
recInterfaceConstraints recTy t =
  [ InterfaceConstraint "indirect" [(FuncTy [t] recTy StaticLifetimeTy)],
    InterfaceConstraint "alloc" [(FuncTy [recTy] t StaticLifetimeTy)]
  ]

--------------------------------------------------------------------------------
-- **Value recursion sugar**
--
-- By default, all types may only be recursive using indirection.
-- However, it can be slightly inconvenient to have to account for indirection when working with recursive types, e.g. using the box type:
--
--   (deftype IntList [head Int tail (Box IntList)])
--   (IntList.init 2 (Box.init (IntList.init 1 (Box.init (IntList.init 0 Nil)))))
--
-- So, we also support syntactic sugar called "value recursion" that emulates recursive data type support in functional languages
--
--   (deftype IntList [head Int tail IntList])
--   (IntList.init 2 (IntList.init 1 (IntList.make 0)))
--
-- Under the hood, the recursive type is wrapped in a Box (a heap allocated, memory-managed pointer).
-- But we generate initers and other functions for recursive types such that
-- all the box wrapping/unwrapping is handled by the compiler instead of the
-- user.

-- | Returns true if this type is a "value-recursive" type.
isValueRecursive :: Ty -> XObj -> Bool
isValueRecursive structTy@(StructTy _ _) (XObj (Arr members) _ _) =
  any go members
  where go :: XObj -> Bool
        go (XObj (Lst xs) _ _) = any go xs
        go xobj = case xobjTy xobj of
                    Just (RecTy rec) -> rec == structTy
                    _ -> False
isValueRecursive _ _ = False

-- | Converts member xobjs in a type definition that refer to the type into pointers.
recursiveMembersToPointers :: Ty -> XObj -> XObj
recursiveMembersToPointers rec (XObj (Arr members) ai at) =
  (XObj (Arr (map go members)) ai at)
  where go :: XObj -> XObj
        go x = case xobjToTy x of
                 Just s@(StructTy _ _) -> convert s
                 _ -> x
          where convert inner = if inner == rec
                                  then (XObj (Lst [XObj (Sym (SymPath [] "RecTy") Symbol) (xobjInfo x) (Just (RecTy rec)), (XObj (Sym (getStructPath rec) Symbol) (xobjInfo x) (Just rec))]) (xobjInfo x) (Just (RecTy rec)))
                                  else x
recursiveMembersToPointers rec (XObj (Lst [name, arr@(XObj (Arr _) _ _)]) li lt) =
  (XObj (Lst [name, (recursiveMembersToPointers rec arr)]) li lt)
recursiveMembersToPointers _ xobj = xobj

--------------------------------------------------------------------------------
-- Value recursive product types

recursiveProductMakeBinder :: [String] -> Ty -> [XObj] -> Either TypeError (String, Binder)
recursiveProductMakeBinder insidePath structTy@(StructTy (ConcreteNameTy _) _) [XObj (Arr membersXObjs) _ _] =
  Right $
    instanceBinder
      (SymPath insidePath "make")
      (FuncTy (initArgListTypes membersXObjs) structTy StaticLifetimeTy)
      (recursiveProductMake StackAlloc structTy membersXObjs)
      ("creates a `" ++ show structTy ++ "`.")
  where initArgListTypes :: [XObj] -> [Ty]
        initArgListTypes xobjs =
          map (fromJust . xobjToTy . snd) (remove (isRecType . fromJust . xobjToTy . snd) (pairwise xobjs))
recursiveProductMakeBinder _ _ _ = error "TODO"

recursiveProductInitBinder :: [String] -> Ty -> [XObj] -> Either TypeError (String, Binder)
recursiveProductInitBinder insidePath structTy@(StructTy (ConcreteNameTy _) _) [XObj (Arr membersXObjs) _ _] =
  Right $
    instanceBinder
      (SymPath insidePath "init")
      (FuncTy (initArgListTypes membersXObjs) structTy StaticLifetimeTy)
      (recursiveProductInit StackAlloc structTy membersXObjs)
      ("creates a `" ++ show structTy ++ "`.")
  where initArgListTypes :: [XObj] -> [Ty]
        initArgListTypes xobjs =
          map (fixRec . fromJust . xobjToTy . snd) (pairwise xobjs)
        fixRec (RecTy t) = t
        fixRec (StructTy name rest) = (StructTy name (map fixRec rest))
        fixRec t = t
recursiveProductInitBinder _ _ _ = error "TODO"

-- | The template for the 'make' and 'new' functions for a concrete deftype.
recursiveProductInit :: AllocationMode -> Ty -> [XObj] -> Template
recursiveProductInit allocationMode originalStructTy@(StructTy (ConcreteNameTy _) _) membersXObjs =
  let pairs = memberXObjsToPairs membersXObjs
      unitless = remove (isUnit . snd)
      unrec = map go . unitless
      go (x, (RecTy t)) = (x, t)
      go (x, t) = (x, t)
   in Template
        (FuncTy (map snd (unrec pairs)) (VarTy "p") StaticLifetimeTy)
        ( \(FuncTy _ concreteStructTy _) ->
            let mappings = unifySignatures originalStructTy concreteStructTy
                correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                memberPairs = memberXObjsToPairs correctedMembers
             in (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg (unrec memberPairs)) ++ ")")
        )
        ( \(FuncTy _ concreteStructTy _) ->
            let mappings = unifySignatures originalStructTy concreteStructTy
                correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
             in productInitTokens allocationMode (show originalStructTy) correctedMembers
        )
        (\FuncTy {} -> [])
  where memberArg :: (String, Ty) -> String
        memberArg (memberName, memberTy) =
          tyToCLambdaFix (templatizeTy memberTy) ++ " " ++ memberName
        templatizeTy :: Ty -> Ty
        templatizeTy (VarTy vt) = VarTy ("$" ++ vt)
        templatizeTy (FuncTy argTys retTy ltTy) = FuncTy (map templatizeTy argTys) (templatizeTy retTy) (templatizeTy ltTy)
        templatizeTy (StructTy name tys) = StructTy name (map templatizeTy tys)
        templatizeTy (RefTy t lt) = RefTy (templatizeTy t) (templatizeTy lt)
        templatizeTy (PointerTy t) = PointerTy (templatizeTy t)
        templatizeTy t = t
recursiveProductInit _ _ _ = error "concreteinit"

productInitTokens :: AllocationMode -> String -> [XObj] -> [Token]
productInitTokens allocationMode typeName membersXObjs =
  let pairs = (memberXObjsToPairs membersXObjs)
   in toTemplate $
        unlines
          [ "$DECL {",
            case allocationMode of
              StackAlloc -> "    $p instance;"
              HeapAlloc  -> "    $p *instance = CARP_MALLOC(sizeof(" ++ typeName ++ "));",
            assignments pairs,
            "    return instance;",
            "}"
          ]
  where
    assignments ps = go (remove (isUnit . snd) ps)
      where
        go [] = ""
        go xobjs = joinLines $ assign allocationMode <$> xobjs
        -- indirected recursion
        assign _ (name, (StructTy tyName [(RecTy _)])) =
          "    instance" ++ "." ++ name ++ " = " ++ "CARP_MALLOC(sizeof(" ++ typeName ++ "));\n" ++
          "    *instance." ++ name ++ " = " ++ show tyName ++ "__indirect(name);\n"
        assign _ (name, (RecTy _)) =
          "    instance" ++ "." ++ name ++ " = " ++ "CARP_MALLOC(sizeof(" ++ typeName ++ "));\n"
          ++ "    *instance." ++ name ++ " = " ++ name ++ ";\n"
        assign alloc (name, _) =
          let accessor = case alloc of
                           StackAlloc -> "."
                           HeapAlloc ->  "->"
           in "    instance" ++ accessor ++ name ++ " = " ++ name ++ ";"

-- | The template for the 'make' and 'new' functions for a concrete deftype.
recursiveProductMake :: AllocationMode -> Ty -> [XObj] -> Template
recursiveProductMake allocationMode originalStructTy@(StructTy (ConcreteNameTy _) _) membersXObjs =
  let pairs = memberXObjsToPairs membersXObjs
      unitless = remove (isRecType . snd) . remove (isUnit . snd)
   in Template
        (FuncTy (map snd (unitless pairs)) (VarTy "p") StaticLifetimeTy)
        ( \(FuncTy _ concreteStructTy _) ->
            let mappings = unifySignatures originalStructTy concreteStructTy
                correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
                memberPairs = memberXObjsToPairs correctedMembers
             in (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg (unitless memberPairs)) ++ ")")
        )
        ( \(FuncTy _ concreteStructTy _) ->
            let mappings = unifySignatures originalStructTy concreteStructTy
                correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
             in productMakeTokens allocationMode (show originalStructTy) correctedMembers
        )
        (\FuncTy {} -> [])
  where memberArg :: (String, Ty) -> String
        memberArg (memberName, memberTy) =
          tyToCLambdaFix (templatizeTy memberTy) ++ " " ++ memberName
        templatizeTy :: Ty -> Ty
        templatizeTy (VarTy vt) = VarTy ("$" ++ vt)
        templatizeTy (FuncTy argTys retTy ltTy) = FuncTy (map templatizeTy argTys) (templatizeTy retTy) (templatizeTy ltTy)
        templatizeTy (StructTy name tys) = StructTy name (map templatizeTy tys)
        templatizeTy (RefTy t lt) = RefTy (templatizeTy t) (templatizeTy lt)
        templatizeTy (PointerTy t) = PointerTy (templatizeTy t)
        templatizeTy t = t
recursiveProductMake _ _ _ = error "concreteinit"

productMakeTokens :: AllocationMode -> String -> [XObj] -> [Token]
productMakeTokens allocationMode typeName membersXObjs =
  let pairs = (memberXObjsToPairs membersXObjs)
   in toTemplate $
        unlines
          [ "$DECL {",
            case allocationMode of
              StackAlloc -> "    $p instance;"
              HeapAlloc  -> "    $p instance = CARP_MALLOC(sizeof(" ++ typeName ++ "));",
            assignments pairs,
            "    return instance;",
            "}"
          ]
  where
    assignments ps = go (remove (isUnit . snd) ps)
      where
        go [] = ""
        go xobjs = joinLines $ assign allocationMode <$> xobjs
        assign alloc (name, ty) =
          let accessor = case alloc of
                           StackAlloc -> "."
                           HeapAlloc ->  "->"
           in if isRecType ty
                then "    instance" ++ accessor ++ name ++ " = " ++ "NULL ;"
                else "    instance" ++ accessor ++ name ++ " = " ++ name ++ ";"

-- | The template for getters of recursive types.
recTemplateGetter :: String -> Ty -> Template
recTemplateGetter member (RecTy t) =
  Template
    (FuncTy [RefTy (VarTy "p") (VarTy "q")] (RefTy t (VarTy "q")) StaticLifetimeTy)
    (const (toTemplate ((tyToC (PointerTy t)) ++ " $NAME($(Ref p) p)")))
    (const $ toTemplate ("$DECL { return p->" ++ member ++"; }\n"))
    (const [])
recTemplateGetter _ _ = error "rectemplate getter"

