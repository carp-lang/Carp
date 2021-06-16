module StructUtils where

import Interfaces
import Obj
import Polymorphism
import Types

data AllocationMode = StackAlloc | HeapAlloc

-- | The 'str'/'prn' functions for primitive types don't take refs, while other types do
-- so we need to adjust for that when finding and calling them in compound types.
-- The returned tuple contains ("" || "&", `str function type`).
memberStrCallingConvention :: String -> TypeEnv -> Env -> Ty -> (String, Ty)
memberStrCallingConvention strOrPrn typeEnv globalEnv memberTy =
  if callWithValue
    then ("", withValueSig)
    else ("&", withRefSig)
  where
    withValueSig = FuncTy [memberTy] StringTy StaticLifetimeTy
    withRefSig = FuncTy [RefTy memberTy (VarTy "w")] StringTy StaticLifetimeTy -- "w" here is dubious?
    callWithValue =
      -- If these interfaces are not implemented we assume ref signature is fine.
      -- Blitable is required to not accicentally pass a value owned by the struct to
      -- a non-ref str/prn function implemented on one of its memeber types.
      let blitable = interfaceImplementedForTy typeEnv globalEnv "blit" (FuncTy [memberTy] memberTy StaticLifetimeTy)
          strTakesValue = interfaceImplementedForTy typeEnv globalEnv strOrPrn withValueSig
       in blitable && strTakesValue

-- | Generate C code for converting a member variable to a string and appending it to a buffer.
memberPrn :: TypeEnv -> Env -> (String, Ty) -> String
memberPrn typeEnv env (memberName, memberTy) =
  let (prefix, strFuncType) = memberStrCallingConvention "prn" typeEnv env memberTy
   in case nameOfPolymorphicFunction typeEnv env strFuncType "prn" of
        Just strFunctionPath ->
          case strFuncType of
            (FuncTy [UnitTy] _ _) ->
              unlines
                [ "  temp = " ++ pathToC strFunctionPath ++ "();",
                  "  sprintf(bufferPtr, \"%s \", temp);",
                  "  bufferPtr += strlen(temp) + 1;",
                  "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                ]
            _ ->
              unlines
                [ "  temp = " ++ pathToC strFunctionPath ++ "(" ++ prefix ++ "p->" ++ memberName ++ ");",
                  "  sprintf(bufferPtr, \"%s \", temp);",
                  "  bufferPtr += strlen(temp) + 1;",
                  "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                ]
        Nothing ->
          "  // Failed to find str function for " ++ memberName ++ " : " ++ show memberTy ++ "\n"

-- | Calculate the size for prn:ing a member of a struct
memberPrnSize :: TypeEnv -> Env -> (String, Ty) -> String
memberPrnSize typeEnv env (memberName, memberTy) =
  let (prefix, strFuncType) = memberStrCallingConvention "prn" typeEnv env memberTy
   in case nameOfPolymorphicFunction typeEnv env strFuncType "prn" of
        Just strFunctionPath ->
          case strFuncType of
            (FuncTy [UnitTy] _ _) ->
              unlines
                [ "  temp = " ++ pathToC strFunctionPath ++ "(); ",
                  "  size += snprintf(NULL, 0, \"%s \", temp);",
                  "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                ]
            _ ->
              unlines
                [ "  temp = " ++ pathToC strFunctionPath ++ "(" ++ prefix ++ "p->" ++ memberName ++ "); ",
                  "  size += snprintf(NULL, 0, \"%s \", temp);",
                  "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                ]
        Nothing ->
          "  // Failed to find str function for " ++ memberName ++ " : " ++ show memberTy ++ "\n"
