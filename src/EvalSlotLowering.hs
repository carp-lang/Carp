module EvalSlotLowering
  ( slotifyCallableLocals,
    findUnresolvedLocalRefs,
  )
where

import EvalBound (BoundRef (..), refToSymPath)
import EvalIR (EvalIR (..))
import qualified Map
import SymPath (SymPath (..))
import Util (pairwise)

slotifyCallableLocals :: [String] -> Maybe String -> EvalIR -> EvalIR
slotifyCallableLocals proper restName =
  rewrite baseSlots
  where
    properSlots = zip proper [0 ..]
    restSlots =
      case restName of
        Nothing -> []
        Just rest -> [(rest, length proper)]
    baseSlots = Map.fromList (properSlots ++ restSlots)
    rewrite env ir =
      case ir of
        IRSymbol ref mode i t ->
          case ref of
            BoundUnresolved (SymPath [] name) ->
              case Map.lookup name env of
                Just slot -> IRSymbol (BoundLocalSlot slot) mode i t
                Nothing -> ir
            _ -> ir
        IRLiteral _ -> ir
        IRArray xs i t -> IRArray (map (rewrite env) xs) i t
        IRStaticArray xs i t -> IRStaticArray (map (rewrite env) xs) i t
        IRIf a b c i t -> IRIf (rewrite env a) (rewrite env b) (rewrite env c) i t
        IRLet bindings body i t ->
          let (bindings', bodyEnv, _) = rewriteLetBindings env bindings
           in IRLet bindings' (rewrite bodyEnv body) i t
        -- Nested fn has its own parameter scope and is lowered separately.
        IRFn args body i t -> IRFn args body i t
        IRDo xs i t -> IRDo (map (rewrite env) xs) i t
        IRWhile cond body i t -> IRWhile (rewrite env cond) (rewrite env body) i t
        IRWith sym forms i t -> IRWith (rewrite env sym) (map (rewrite env) forms) i t
        IRSet target value i t -> IRSet (rewriteSetTarget env target) (rewrite env value) i t
        IRCall fun args i t -> IRCall (rewrite env fun) (map (rewrite env) args) i t
        IRList xs i t -> IRList (map (rewrite env) xs) i t
    rewriteSetTarget env target =
      case target of
        IRSymbol (BoundUnresolved (SymPath [] _)) _ _ _ -> target
        _ -> rewrite env target
    rewriteLetBindings env bindingsIR =
      case bindingsIR of
        IRArray flat i t ->
          let pairs = pairwise flat
              nextSlot0 = nextFreeSlot env
              (rewrittenPairs, env', names) = rewritePairs env nextSlot0 pairs
              flat' = concatMap (\(a, b) -> [a, b]) rewrittenPairs
           in (IRArray flat' i t, env', names)
        _ -> (rewrite env bindingsIR, env, [])
    rewritePairs env _ [] = ([], env, [])
    rewritePairs env nextSlot ((nameIR, valueIR) : rest) =
      let valueIR' = rewrite env valueIR
          (restPairs, restEnv, restNames) = rewritePairs nextEnv nextSlot' rest
       in ((nameIR, valueIR') : restPairs, restEnv, maybe restNames (: restNames) maybeName)
      where
        maybeName = bindingName nameIR
        (nextEnv, nextSlot') =
          case maybeName of
            Just n -> (Map.insert n nextSlot env, nextSlot + 1)
            Nothing -> (env, nextSlot)
    nextFreeSlot env
      | null (Map.elems env) = 0
      | otherwise = 1 + maximum (Map.elems env)
    bindingName nameIR =
      case nameIR of
        IRSymbol ref _ _ _ ->
          case refToSymPath ref of
            SymPath [] n -> Just n
            _ -> Nothing
        _ -> Nothing

findUnresolvedLocalRefs :: [String] -> Maybe String -> EvalIR -> [String]
findUnresolvedLocalRefs proper restName =
  go baseSlots
  where
    properSlots = zip proper [0 ..]
    restSlots =
      case restName of
        Nothing -> []
        Just rest -> [(rest, length proper)]
    baseSlots = Map.fromList (properSlots ++ restSlots)
    go env ir =
      case ir of
        IRSymbol ref _ _ _ ->
          case ref of
            BoundUnresolved (SymPath [] name)
              | Map.member name env -> [name]
            _ -> []
        IRLiteral _ -> []
        IRArray xs _ _ -> concatMap (go env) xs
        IRStaticArray xs _ _ -> concatMap (go env) xs
        IRIf a b c _ _ -> go env a ++ go env b ++ go env c
        IRLet bindings body _ _ ->
          let (bindingIssues, bodyEnv) = goBindings env bindings
           in bindingIssues ++ go bodyEnv body
        IRFn _ _ _ _ -> []
        IRDo xs _ _ -> concatMap (go env) xs
        IRWhile cond body _ _ -> go env cond ++ go env body
        IRWith sym forms _ _ -> go env sym ++ concatMap (go env) forms
        IRSet target value _ _ -> goSetTarget env target ++ go env value
        IRCall fun args _ _ -> go env fun ++ concatMap (go env) args
        IRList xs _ _ -> concatMap (go env) xs
    goSetTarget env target =
      case target of
        IRSymbol (BoundUnresolved (SymPath [] _)) _ _ _ -> []
        _ -> go env target
    goBindings env bindingsIR =
      case bindingsIR of
        IRArray flat _ _ ->
          let pairs = pairwise flat
           in goPairs env pairs
        _ -> (go env bindingsIR, env)
    goPairs env [] = ([], env)
    goPairs env ((nameIR, valueIR) : rest) =
      let thisIssues = go env valueIR
          env' =
            case bindingName nameIR of
              Just n -> Map.insert n (nextFreeSlot env) env
              Nothing -> env
          (restIssues, finalEnv) = goPairs env' rest
       in (thisIssues ++ restIssues, finalEnv)
    nextFreeSlot env
      | null (Map.elems env) = 0
      | otherwise = 1 + maximum (Map.elems env)
    bindingName nameIR =
      case nameIR of
        IRSymbol ref _ _ _ ->
          case refToSymPath ref of
            SymPath [] n -> Just n
            _ -> Nothing
        _ -> Nothing
