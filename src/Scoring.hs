module Scoring (scoreBinder) where

import Types
import Obj

-- | The score is used for sorting the bindings before emitting them.
-- | A lower score means appearing earlier in the emitted file.
scoreBinder :: TypeEnv -> Binder -> (Int, Binder)
scoreBinder typeEnv b@(Binder (XObj (Lst (XObj x _ _ : XObj (Sym _ _) _ _ : _)) _ _)) =
  case x of
    Defalias aliasedType ->
      let selfName = ""
      in  (depthOfType typeEnv selfName aliasedType, b)
    Typ (StructTy structName varTys) ->
      case lookupInEnv (SymPath [] structName) (getTypeEnv typeEnv) of
        Just (_, Binder typedef) -> let depth = ((depthOfDeftype typeEnv typedef varTys), b)
                                    in  --trace ("depth of " ++ structName ++ ": " ++ show depth)
                                        depth
        Nothing -> error ("Can't find user defined type '" ++ structName ++ "' in type env.")
    _ ->
      (500, b)
scoreBinder _ b@(Binder (XObj (Mod _) _ _)) =
  (1000, b)
scoreBinder _ x = error ("Can't score: " ++ show x)

depthOfDeftype :: TypeEnv -> XObj -> [Ty] -> Int
depthOfDeftype typeEnv (XObj (Lst (_ : XObj (Sym (SymPath _ selfName) _) _ _ : rest)) _ _) varTys =
  case concatMap expandCase rest of
    [] -> 100
    xs -> (maximum xs) + 1
  where
    expandCase :: XObj -> [Int]
    expandCase (XObj (Arr arr) _ _) =
      let members = memberXObjsToPairs arr
          depthsFromMembers = map (depthOfType typeEnv selfName . snd) members
          depthsFromVarTys = map (depthOfType typeEnv selfName) varTys
      in depthsFromMembers ++ depthsFromVarTys
    expandCase _ = error "Malformed case in typedef."
depthOfDeftype _ xobj _ =
  error ("Can't get dependency depth from " ++ show xobj)

depthOfType :: TypeEnv -> String -> Ty -> Int
depthOfType typeEnv selfName = visitType
  where
    visitType :: Ty -> Int
    visitType t@(StructTy name varTys) = depthOfStructType (tyToC t) varTys
    visitType (FuncTy argTys retTy) =
      -- trace ("Depth of args of " ++ show argTys ++ ": " ++ show (map (visitType . Just) argTys))
      maximum (visitType retTy : map visitType argTys) + 1
    visitType (PointerTy p) = visitType p
    visitType (RefTy r) = visitType r
    visitType _ = 100

    depthOfStructType :: String -> [Ty] -> Int
    depthOfStructType name varTys =
      case name of
        "Array" -> depthOfVarTys
        _ | name == selfName -> 30
          | otherwise ->
              case lookupInEnv (SymPath [] name) (getTypeEnv typeEnv) of
                Just (_, Binder typedef) -> (depthOfDeftype typeEnv typedef varTys) + 1
                Nothing -> --trace ("Unknown type: " ++ name) $
                           depthOfVarTys -- The problem here is that generic types don't generate
                                         -- their definition in time so we get nothing for those.
                                         -- Instead, let's try the type vars.
      where depthOfVarTys =
              case map (depthOfType typeEnv name) varTys of
                [] -> 50
                xs -> (maximum xs) + 1
