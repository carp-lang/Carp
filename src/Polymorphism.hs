module Polymorphism
  ( nameOfPolymorphicFunction,
    allImplementations,
    FunctionFinderResult (..),
    findFunctionForMember,
    findFunctionForMemberIncludePrimitives,
  )
where

import Data.Either (fromRight)
import Env
import Managed
import Obj
import Types

-- | Calculate the full, mangled name of a concretized polymorphic function.
-- For example, The 'id' in "(id 3)" will become 'id__int'.
--
-- This function uses findPoly, which gives it access to *all* possible
-- environments in the given input environment (children, (modules) parents,
-- and use modules). This allows it to derive the correct name for functions
-- that may be defined in a different environment.
--
-- TODO: Environments are passed in different order here!!!
nameOfPolymorphicFunction :: TypeEnv -> Env -> Ty -> String -> Maybe SymPath
nameOfPolymorphicFunction _ env functionType functionName =
  let foundBinder =
        (findPoly env functionName functionType)
          <> (findPoly (progenitor env) functionName functionType)
   in case foundBinder of
        Right (_, (Binder _ (XObj (Lst (XObj (External (Just name)) _ _ : _)) _ _))) ->
          Just (SymPath [] name)
        Right (_, (Binder _ single)) ->
          let Just t' = xobjTy single
              (SymPath pathStrings name) = getPath single
              suffix = polymorphicSuffix t' functionType
              concretizedPath = SymPath pathStrings (name ++ suffix)
           in Just concretizedPath
        _ -> Nothing

-- | Find ALL functions with a certain name, matching a type signature.
-- When the functionName argument denotes an interface, the name will match iff either:
--   1. The name of the binding matches functionName exactly OR
--   2. The name of the binding matches one of the names in the interface's implementation paths
-- For all other functions, the name must match exactly, and in all cases, the signature must match.
allImplementations :: TypeEnv -> Env -> String -> Ty -> [(Env, Binder)]
allImplementations typeEnv env functionName functionType =
  (filter (predicate . xobjTy . binderXObj . snd) foundBindings)
  where
    predicate (Just t) =
      --trace ("areUnifiable? " ++ show functionType ++ " == " ++ show t ++ " " ++ show (areUnifiable functionType t)) $
      areUnifiable functionType t
    predicate Nothing = error "allfunctionswithnameandsignature"
    foundBindings = case getTypeBinder typeEnv functionName of
      -- this function is an interface; lookup implementations
      Right (Binder _ (XObj (Lst (XObj (Interface _ paths) _ _ : _)) _ _)) ->
        case sequence $ map (\p -> searchValue env p) (paths ++ [(SymPath [] functionName)]) of
          Right found -> found
          Left _ ->
            case findPoly env functionName functionType of
              Right r -> [r]
              Left _ -> (lookupEverywhere env functionName)
      -- just a regular function; look for it
      _ -> fromRight [] ((fmap (: []) (Env.getValue env functionName)) <> pure (lookupEverywhere env functionName))

-- | The various results when trying to find a function using 'findFunctionForMember'.
data FunctionFinderResult
  = FunctionFound String
  | FunctionNotFound String
  | FunctionIgnored
  deriving (Show)

-- | Used for finding functions like 'delete' or 'copy' for members of a Deftype (or Array).
findFunctionForMember :: TypeEnv -> Env -> String -> Ty -> (String, Ty) -> FunctionFinderResult
findFunctionForMember typeEnv env functionName functionType (memberName, memberType)
  | isManaged typeEnv env memberType =
    case allImplementations typeEnv env functionName functionType of
      [] ->
        FunctionNotFound
          ( "Can't find any '" ++ functionName ++ "' function for member '"
              ++ memberName
              ++ "' of type "
              ++ show functionType
          )
      [(_, Binder _ single)] ->
        let concretizedPath = getConcretizedPath single functionType
         in FunctionFound (pathToC concretizedPath)
      _ ->
        FunctionNotFound
          ( "Can't find a single '" ++ functionName ++ "' function for member '"
              ++ memberName
              ++ "' of type "
              ++ show functionType
          )
  | otherwise = FunctionIgnored

-- | TODO: should this be the default and 'findFunctionForMember' be the specific one
findFunctionForMemberIncludePrimitives :: TypeEnv -> Env -> String -> Ty -> (String, Ty) -> FunctionFinderResult
findFunctionForMemberIncludePrimitives typeEnv env functionName functionType (memberName, _) =
  case allImplementations typeEnv env functionName functionType of
    [] ->
      FunctionNotFound
        ( "Can't find any '" ++ functionName ++ "' function for member '"
            ++ memberName
            ++ "' of type "
            ++ show functionType
        )
    [(_, Binder _ single)] ->
      let concretizedPath = getConcretizedPath single functionType
       in FunctionFound (pathToC concretizedPath)
    _ ->
      FunctionNotFound
        ( "Can't find a single '" ++ functionName ++ "' function for member '"
            ++ memberName
            ++ "' of type "
            ++ show functionType
        )

-- | Creates a new SymPath with a suffix added to the name,
-- for differentiating the concrete version of the function from
-- its generic ancestor.
getConcretizedPath :: XObj -> Ty -> SymPath
getConcretizedPath defn functionType =
  let Just t' = xobjTy defn
      SymPath pathStrings name = getPath defn
      suffix = polymorphicSuffix t' functionType
   in SymPath pathStrings (name ++ suffix)
