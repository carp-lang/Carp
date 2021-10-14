module RecType
  (
   recursiveMembersToPointers,
   isRecursive,
   recursiveProductMakeBinder,
   recursiveProductInitBinder,
   recTemplateGetter,
  )
where

import Obj
import Types
import TypePredicates
import TypeError
import TypesToC
import StructUtils
import Template
import Util
import Data.Maybe (fromJust)
import Concretize
import ToTemplate

isRecursive :: Ty -> XObj -> Bool
isRecursive structTy@(StructTy _ _) (XObj (Arr members) _ _) =
  any go members
  where go :: XObj -> Bool
        go xobj = case xobjTy xobj of
                    Just (RecTy rec) -> rec == structTy
                    _ -> False
isRecursive _ _ = False

-- | Converts member xobjs in a type definition that refer to the type into pointers
recursiveMembersToPointers :: Ty -> XObj -> XObj
recursiveMembersToPointers rec (XObj (Arr members) ai at) =
  (XObj (Arr (map go members)) ai at)
  where go :: XObj -> XObj
        go x@(XObj (Sym spath _) i _) = if show spath == tyname
                                          then (XObj (Lst [XObj (Sym (SymPath [] "RecTy") Symbol) i (Just (RecTy rec)), x]) i (Just (RecTy rec)))
                                          else x
        go x = x
        tyname = getStructName rec
recursiveMembersToPointers _ xobj = xobj

--------------------------------------------------------------------------------
-- Recursive product types

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
      (recursiveProductInit HeapAlloc structTy membersXObjs)
      ("creates a `" ++ show structTy ++ "`.")
  where initArgListTypes :: [XObj] -> [Ty]
        initArgListTypes xobjs =
          map (fixRec . fromJust . xobjToTy . snd) (pairwise xobjs)
        fixRec (RecTy t) = t
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
            "    return *instance;",
            "}"
          ]
  where
    assignments ps = go (remove (isUnit . snd) ps)
      where
        go [] = ""
        go xobjs = joinLines $ assign allocationMode <$> xobjs
        assign _ (name, (RecTy _)) =
          "    instance" ++ "->" ++ name ++ " = " ++ "CARP_MALLOC(sizeof(" ++ typeName ++ "));\n"
          ++ "    *instance->" ++ name ++ " = " ++ name ++ ";\n"
          -- ++ "    instance" ++ "->" ++ name ++ " = " ++ "&" ++ name ++ ";\n"
          -- ++ "    " ++ typeName ++"_delete(" ++ name ++ ");"
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

