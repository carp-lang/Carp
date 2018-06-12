module Qualify where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate, foldl')
import Debug.Trace

import Types
import Obj
import Lookup
import Util

-- | Changes the symbol part of a defn (the name) to a new symbol path
-- | Example: (defn foo () 123) => (defn GreatModule.foo () 123)
setFullyQualifiedDefn :: XObj -> SymPath -> XObj
setFullyQualifiedDefn (XObj (Lst [defn, XObj _ symi symt, args, body]) i t) newPath =
  XObj (Lst [defn, XObj (Sym newPath Symbol) symi symt, args, body]) i t
setFullyQualifiedDefn (XObj (Lst [def, XObj _ symi symt, expr]) i t) newPath =
  XObj (Lst [def, XObj (Sym newPath Symbol) symi symt, expr]) i t
setFullyQualifiedDefn xobj _ = error ("Can't set new path on " ++ show xobj)

-- | Changes all symbols EXCEPT bound vars (defn names, variable names, etc) to their fully qualified paths.
-- | This must run after the 'setFullyQualifiedDefn' function has fixed the paths of all bindings in the environment.
-- | This function does NOT go into function-body scope environments and the like.
setFullyQualifiedSymbols :: TypeEnv -> Env -> Env -> XObj -> XObj
setFullyQualifiedSymbols typeEnv globalEnv env (XObj (Lst [defn@(XObj Defn _ _),
                                                 sym@(XObj (Sym (SymPath _ functionName) _) _ _),
                                                 args@(XObj (Arr argsArr) _ _),
                                                 body])
                                      i t) =
  -- For self-recursion, there must be a binding to the function in the inner env.
  -- Note: This inner env is ephemeral since it is not stored in a module or global scope.
  let functionEnv = Env Map.empty (Just env) Nothing [] InternalEnv
      envWithSelf = extendEnv functionEnv functionName sym
      envWithArgs = foldl' (\e arg@(XObj (Sym (SymPath _ argSymName) _) _ _) -> extendEnv e argSymName arg) envWithSelf argsArr
  in  XObj (Lst [defn, sym, args, setFullyQualifiedSymbols typeEnv globalEnv envWithArgs body]) i t
setFullyQualifiedSymbols typeEnv globalEnv env (XObj (Lst [the@(XObj The _ _), typeXObj, value]) i t) =
  let value' = setFullyQualifiedSymbols typeEnv globalEnv env value
  in  XObj (Lst [the, typeXObj, value']) i t
setFullyQualifiedSymbols typeEnv globalEnv env (XObj (Lst [def@(XObj Def _ _), sym, expr]) i t) =
  let expr' = setFullyQualifiedSymbols typeEnv globalEnv env expr
  in  XObj (Lst [def, sym, expr']) i t
setFullyQualifiedSymbols typeEnv globalEnv env (XObj (Lst [letExpr@(XObj Let _ _), bind@(XObj (Arr bindings) bindi bindt), body]) i t) =
  if even (length bindings)
  then let innerEnv = Env Map.empty (Just env) (Just "LET") [] InternalEnv
           (innerEnv', bindings') =
             foldl' (\(e, bs) (s@(XObj (Sym (SymPath _ binderName) _) _ _), o) ->
                       let qualified = setFullyQualifiedSymbols typeEnv globalEnv e o
                       in (extendEnv e binderName s, bs ++ [s, qualified]))
                    (innerEnv, []) (pairwise bindings)
           newBody = setFullyQualifiedSymbols typeEnv globalEnv innerEnv' body
       in  XObj (Lst [letExpr, XObj (Arr bindings') bindi bindt, newBody]) i t
  else XObj (Lst [letExpr, bind, body]) i t -- Leave it untouched for the compiler to find the error.
setFullyQualifiedSymbols typeEnv globalEnv env (XObj (Lst [XObj With _ _, XObj (Sym path _) _ _, expression]) _ _) =
  let useThese = envUseModules env
      env' = if path `elem` useThese then env else env { envUseModules = path : useThese }
  in  setFullyQualifiedSymbols typeEnv globalEnv env' expression
setFullyQualifiedSymbols typeEnv globalEnv env (XObj (Lst xobjs) i t) =
  -- TODO: Perhaps this general case can be sufficient? No need with all the cases above..?
  let xobjs' = map (setFullyQualifiedSymbols typeEnv globalEnv env) xobjs
  in  XObj (Lst xobjs') i t
setFullyQualifiedSymbols typeEnv globalEnv localEnv xobj@(XObj (Sym path _) i t) =
  case path of
    -- Unqualified:
    SymPath [] name ->
      case lookupInEnv path (getTypeEnv typeEnv) of
        Just found ->
          -- Found an interface with the same path!
          -- Have to ensure it's not a local variable with the same name as the interface
          case lookupInEnv path localEnv of
            Just (foundEnv, _) ->
              if envIsExternal foundEnv
              then createInterfaceSym name
              else doesNotBelongToAnInterface False localEnv
            Nothing ->
              --trace ("Will turn '" ++ show path ++ "' " ++ prettyInfoFromXObj xobj ++ " into an interface symbol.")
                createInterfaceSym name
        Nothing ->
          doesNotBelongToAnInterface False localEnv
    -- Qualified:
    _ ->
      doesNotBelongToAnInterface False localEnv
  where
    createInterfaceSym name =
      XObj (InterfaceSym name) i t
    doesNotBelongToAnInterface :: Bool -> Env -> XObj
    doesNotBelongToAnInterface finalRecurse theEnv =
      case multiLookupQualified path theEnv of
          [] -> case envParent theEnv of
                  Just p ->
                    doesNotBelongToAnInterface False p
                  Nothing ->
                    -- | OBS! The environment with no parent is the global env but it's an old one without the latest bindings!
                    if finalRecurse
                    then xobj -- This was the TRUE global env, stop here and leave 'xobj' as is.
                    else doesNotBelongToAnInterface True globalEnv
          [(_, Binder _ foundOne@(XObj (Lst ((XObj (External (Just overrideWithName)) _ _) : _)) _ _))] ->
            XObj (Sym (getPath foundOne) (LookupGlobalOverride overrideWithName)) i t
          [(e, Binder _ foundOne)] ->
            if envIsExternal e
            then XObj (Sym (getPath foundOne) LookupGlobal) i t
            else XObj (Sym (getPath foundOne) LookupLocal) i t
          multiple ->
            case filter (not . envIsExternal . fst) multiple of
            -- There is at least one local binding, use the path of that one:
              (_, Binder _ local) : _ -> XObj (Sym (getPath local) LookupLocal) i t
            -- There are no local bindings, this is allowed to become a multi lookup symbol:
              _ -> --(trace $ "Turned " ++ name ++ " into multisym: " ++ joinWithComma (map (show .getPath . binderXObj . snd) multiple))
                case path of
                  (SymPath [] name) ->
                     -- Create a MultiSym!
                    XObj (MultiSym name (map (getPath . binderXObj . snd) multiple)) i t
                  pathWithQualifiers ->
                    -- The symbol IS qualified but can't be found, should produce an error later during compilation.
                    trace ("PROBLEMATIC: " ++ show path) (XObj (Sym pathWithQualifiers LookupGlobal) i t)
setFullyQualifiedSymbols typeEnv globalEnv env xobj@(XObj (Arr array) i t) =
  let array' = map (setFullyQualifiedSymbols typeEnv globalEnv env) array
  in  XObj (Arr array') i t
setFullyQualifiedSymbols _ _ _ xobj = xobj
