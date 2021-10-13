module RecType
  (
   recursiveMembersToPointers,
   isRecursive,
   recursiveProductMakeBinder,
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

---- | Generate a list of types from a deftype declaration.
--initArgListTypes :: [XObj] -> [Ty]
--initArgListTypes xobjs =
--  map (fromJust . xobjToTy . snd) (pairwise xobjs)

--tokensForRecInit :: AllocationMode -> String -> [XObj] -> [Token]
--tokensForRecInit allocationMode typeName membersXObjs =
--  toTemplate $
--    unlines
--      [ "$DECL {",
--        case allocationMode of
--          StackAlloc -> case unitless of
--            -- if this is truly a memberless struct, init it to 0;
--            -- This can happen, e.g. in cases where *all* members of the struct are of type Unit.
--            -- Since we do not generate members for Unit types.
--            [] -> "    $p instance = {};"
--            _ -> "    $p instance;"
--          HeapAlloc -> "    $p instance = CARP_MALLOC(sizeof(" ++ typeName ++ "));",
--        assignments membersXObjs,
--        recAssignment recmembers,
--        "    return instance;",
--        "}"
--      ]
--  where
--    recmembers = filter (isRecType . snd) (memberXObjsToPairs membersXObjs)
--    assignments [] = "    instance.__dummy = 0;"
--    assignments _ = go unitless
--      where
--        go [] = ""
--        go xobjs = joinLines $ memberAssignment allocationMode . fst <$> xobjs
--    unitless = remove isRecType (remove (isUnit . snd) (memberXObjsToPairs membersXObjs))
--    recAssignment xs = 
--
--memberAssignment :: AllocationMode -> String -> String
--memberAssignment allocationMode memberName = "    instance" ++ sep ++ memberName ++ " = " ++ memberName ++ ";"
--  where
--    sep = case allocationMode of
--      StackAlloc -> "."
--      HeapAlloc -> "->"


--
---- | The template for the 'init' and 'new' functions for a generic deftype.
--genericInit :: AllocationMode -> [String] -> Ty -> [XObj] -> (String, Binder)
--genericInit allocationMode pathStrings originalStructTy@(StructTy (ConcreteNameTy _) _) membersXObjs =
--  defineTypeParameterizedTemplate templateCreator path t docs
--  where
--    path = SymPath pathStrings "init"
--    t = FuncTy (map snd (memberXObjsToPairs membersXObjs)) originalStructTy StaticLifetimeTy
--    docs = "creates a `" ++ show originalStructTy ++ "`."
--    templateCreator = TemplateCreator $
--      \typeEnv env ->
--        Template
--          (FuncTy (map snd (memberXObjsToPairs membersXObjs)) (VarTy "p") StaticLifetimeTy)
--          ( \(FuncTy _ concreteStructTy _) ->
--              let mappings = unifySignatures originalStructTy concreteStructTy
--                  correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
--                  memberPairs = memberXObjsToPairs correctedMembers
--               in (toTemplate $ "$p $NAME(" ++ joinWithComma (map memberArg (remove (isUnit . snd) memberPairs)) ++ ")")
--          )
--          ( \(FuncTy _ concreteStructTy _) ->
--              let mappings = unifySignatures originalStructTy concreteStructTy
--                  correctedMembers = replaceGenericTypeSymbolsOnMembers mappings membersXObjs
--               in tokensForInit allocationMode (show originalStructTy) correctedMembers
--          )
--          ( \(FuncTy _ concreteStructTy _) ->
--              case concretizeType typeEnv env concreteStructTy of
--                Left _ -> []
--                Right ok -> ok
--          )
--genericInit _ _ _ _ = error "genericinit"
