module Scoring (scoreTypeBinder, scoreValueBinder) where

import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromJust)

import Types
import Obj
import Lookup

-- | Scoring of types.
-- | The score is used for sorting the bindings before emitting them.
-- | A lower score means appearing earlier in the emitted file.
scoreTypeBinder :: TypeEnv -> Binder -> (Int, Binder)
scoreTypeBinder typeEnv b@(Binder _ (XObj (Lst (XObj x _ _ : XObj (Sym _ _) _ _ : _)) _ _)) =
  case x of
    Defalias aliasedType ->
      let selfName = ""
      -- we add 1 here because deftypes generate aliases that
      -- will at least have the same score as the type, but
      -- need to come after. the increment represents this dependency
      in  (depthOfType typeEnv Set.empty selfName aliasedType + 1, b)
    Typ (StructTy structName varTys) ->
      case lookupInEnv (SymPath [] structName) (getTypeEnv typeEnv) of
        Just (_, Binder _ typedef) -> let depth = (depthOfDeftype typeEnv Set.empty typedef varTys, b)
                                    in  --trace ("depth of " ++ structName ++ ": " ++ show depth)
                                        depth
        Nothing -> error ("Can't find user defined type '" ++ structName ++ "' in type env.")
    DefSumtype (StructTy structName varTys) ->
      -- DUPLICATION FROM ABOVE:
      case lookupInEnv (SymPath [] structName) (getTypeEnv typeEnv) of
        Just (_, Binder _ typedef) -> let depth = (depthOfDeftype typeEnv Set.empty typedef varTys, b)
                                    in  --trace ("depth of " ++ structName ++ ": " ++ show depth)
                                        depth
        Nothing -> error ("Can't find user defined type '" ++ structName ++ "' in type env.")
    _ ->
      (500, b)
scoreTypeBinder _ b@(Binder _ (XObj (Mod _) _ _)) =
  (1000, b)
scoreTypeBinder _ x = error ("Can't score: " ++ show x)

depthOfDeftype :: TypeEnv -> Set.Set Ty -> XObj -> [Ty] -> Int
depthOfDeftype typeEnv visited (XObj (Lst (_ : XObj (Sym (SymPath _ selfName) _) _ _ : rest)) _ _) varTys =
  case concatMap expandCase rest of
    [] -> 100
    xs -> maximum xs + 1
  where
    depthsFromVarTys = map (depthOfType typeEnv visited selfName) varTys

    expandCase :: XObj -> [Int]
    expandCase (XObj (Arr arr) _ _) =
      let members = memberXObjsToPairs arr
          depthsFromMembers = map (depthOfType typeEnv visited selfName . snd) members
      in depthsFromMembers ++ depthsFromVarTys
    expandCase (XObj (Lst [XObj{}, XObj (Arr sumtypeCaseTys) _ _]) _ _) =
      let depthsFromCaseTys = map (depthOfType typeEnv visited selfName . fromJust . xobjToTy) sumtypeCaseTys
      in depthsFromCaseTys ++ depthsFromVarTys
    expandCase (XObj (Sym _ _) _ _) =
      []
    expandCase _ = error "Malformed case in typedef."
depthOfDeftype _ _ xobj _ =
  error ("Can't get dependency depth from " ++ show xobj)

depthOfType :: TypeEnv -> Set.Set Ty -> String -> Ty -> Int
depthOfType typeEnv visited selfName theType =
  if theType `elem` visited
  then 0
  else visitType theType
  where
    visitType :: Ty -> Int
    visitType t@(StructTy name varTys) = depthOfStructType (tyToC t) varTys
    visitType (FuncTy argTys retTy) =
      -- trace ("Depth of args of " ++ show argTys ++ ": " ++ show (map (visitType . Just) argTys))
      maximum (visitType retTy : fmap visitType argTys) + 1
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
                Just (_, Binder _ typedef) -> depthOfDeftype typeEnv (Set.insert theType visited) typedef varTys + 1
                Nothing -> --trace ("Unknown type: " ++ name) $
                           depthOfVarTys -- The problem here is that generic types don't generate
                                         -- their definition in time so we get nothing for those.
                                         -- Instead, let's try the type vars.
      where depthOfVarTys =
              case fmap (depthOfType typeEnv visited name) varTys of
                [] -> 50
                xs -> maximum xs + 1



-- | Scoring of value bindings ('def' and 'defn')
-- | The score is used for sorting the bindings before emitting them.
-- | A lower score means appearing earlier in the emitted file.
scoreValueBinder :: Env -> Set.Set SymPath -> Binder -> (Int, Binder)
scoreValueBinder globalEnv _ binder@(Binder _ (XObj (Lst (XObj (External _) _ _ : _)) _ _)) =
  (0, binder)
scoreValueBinder globalEnv visited binder@(Binder _ (XObj (Lst [XObj Def  _ _, XObj (Sym path Symbol) _ _, body]) _ _)) =
  (scoreBody globalEnv visited body, binder)
scoreValueBinder globalEnv visited binder@(Binder _ (XObj (Lst [XObj Defn _ _, XObj (Sym path Symbol) _ _, _, body]) _ _)) =
  (scoreBody globalEnv visited body, binder)
scoreValueBinder _ _ binder =
  (0, binder)

scoreBody :: Env -> Set.Set SymPath -> XObj -> Int
scoreBody globalEnv visited root = visit root
  where
    visit xobj =
      case obj xobj of
        (Lst _) ->
          visitList xobj
        (Arr _) ->
          visitArray xobj
        (Sym path (LookupGlobal _ _)) ->
          if Set.member path visited
          then 0
          else case lookupInEnv path globalEnv of
                 Just (_, foundBinder) ->
                   let (score, _) = scoreValueBinder globalEnv (Set.insert path visited) foundBinder
                   in  score + 1
                 Nothing ->
                   error ("Failed to lookup '" ++ show path ++ "'.")
        _ -> 0
    visitList (XObj (Lst []) _ _) =
      0
    visitList (XObj (Lst xobjs) _ _) =
      maximum (fmap visit xobjs)
    visitArray (XObj (Arr []) _ _) =
      0
    visitArray (XObj (Arr xobjs) _ _) =
      maximum (fmap visit xobjs)
