module Scoring (scoreTypeBinder, scoreValueBinder) where

import Debug.Trace
import qualified Data.Set as Set

import Types
import Obj
import Lookup

-- | Scoring of types.
-- | The score is used for sorting the bindings before emitting them.
-- | A lower score means appearing earlier in the emitted file.
scoreTypeBinder :: TypeEnv -> Binder -> (Int, Binder)
scoreTypeBinder typeEnv b@(Binder (XObj (Lst (XObj x _ _ : XObj (Sym _ _) _ _ : _)) _ _)) =
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
scoreTypeBinder _ b@(Binder (XObj (Mod _) _ _)) =
  (1000, b)
scoreTypeBinder _ x = error ("Can't score: " ++ show x)

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
                Just (_, Binder typedef) -> (depthOfDeftype typeEnv typedef varTys) + 1
                Nothing -> --trace ("Unknown type: " ++ name) $
                           depthOfVarTys -- The problem here is that generic types don't generate
                                         -- their definition in time so we get nothing for those.
                                         -- Instead, let's try the type vars.
      where depthOfVarTys =
              case fmap (depthOfType typeEnv name) varTys of
                [] -> 50
                xs -> (maximum xs) + 1



-- | Scoring of value bindings ('def' and 'defn')
-- | The score is used for sorting the bindings before emitting them.
-- | A lower score means appearing earlier in the emitted file.
scoreValueBinder :: Env -> Set.Set SymPath -> Binder -> (Int, Binder)
scoreValueBinder env _ binder@(Binder (XObj (Lst ((XObj (External _) _ _) : _)) _ _)) =
  (0, binder)
scoreValueBinder env visited binder@(Binder (XObj (Lst [(XObj Def  _ _), XObj (Sym path Symbol) _ _, body]) _ _)) =
  (scoreBody env visited body, binder)
scoreValueBinder env visited binder@(Binder (XObj (Lst [(XObj Defn _ _), XObj (Sym path Symbol) _ _, _, body]) _ _)) =
  (scoreBody env visited body, binder)
scoreValueBinder _ _ b@(Binder (XObj (Mod _) _ _)) =
  (1000, b)
scoreValueBinder _ _ binder =
  (0, binder) -- error ("Can't score " ++ show binder)

scoreBody :: Env -> Set.Set SymPath -> XObj -> Int
scoreBody env visited root = visit root
  where
    visit xobj =
      case obj xobj of
        (Lst _) ->
          visitList xobj
        (Arr _) ->
          visitArray xobj
        (Sym path LookupGlobal) ->
          if Set.member path visited
          then 0
          else case lookupInEnv path env of
                 Just (_, foundBinder) ->
                   let (score, _) = scoreValueBinder env (Set.insert path visited) foundBinder
                   in  score + 1
                 Nothing ->
                   -- error ("Failed to lookup '" ++ show path ++ "'.")
                   0
        _ -> 0
    visitList (XObj (Lst []) _ _) =
      0
    visitList (XObj (Lst xobjs) _ _) =
      maximum (fmap visit xobjs)
    visitArray (XObj (Arr []) _ _) =
      0
    visitArray (XObj (Arr xobjs) _ _) =
      maximum (fmap visit xobjs)
