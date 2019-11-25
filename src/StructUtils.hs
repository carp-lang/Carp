module StructUtils where

import Obj
import Types
import Util
import Lookup
import Polymorphism

memberInfo typeEnv memberTy =
  let refOrNotRefType = if isManaged typeEnv memberTy then RefTy memberTy (VarTy "w") else memberTy -- OBS! The VarTy "w" here is dubious
  in (refOrNotRefType, if isManaged typeEnv memberTy then "&" else "", FuncTy StaticLifetimeTy [refOrNotRefType] StringTy)

-- | Generate C code for converting a member variable to a string and appending it to a buffer.
memberPrn :: TypeEnv -> Env -> (String, Ty) -> String
memberPrn typeEnv env (memberName, memberTy) =
  let (refOrNotRefType, maybeTakeAddress, strFuncType) = memberInfo typeEnv memberTy
   in case nameOfPolymorphicFunction typeEnv env strFuncType "prn" of
        Just strFunctionPath ->
          unlines ["  temp = " ++ pathToC strFunctionPath ++ "(" ++ maybeTakeAddress ++ "p->" ++ memberName ++ ");"
                  , "  snprintf(bufferPtr, size, \"%s \", temp);"
                  , "  bufferPtr += strlen(temp) + 1;"
                  , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                  ]
        Nothing ->
          if isExternalType typeEnv memberTy
          then unlines [ "  temp = malloc(11);"
                       , "  snprintf(temp, 11, \"<external>\");"
                       , "  snprintf(bufferPtr, size, \"%s \", temp);"
                       , "  bufferPtr += strlen(temp) + 1;"
                       , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                       ]
          else "  // Failed to find str function for " ++ memberName ++ " : " ++ show memberTy ++ "\n"

-- | Calculate the size for prn:ing a member of a struct
memberPrnSize :: TypeEnv -> Env -> (String, Ty) -> String
memberPrnSize typeEnv env (memberName, memberTy) =
  let (refOrNotRefType, maybeTakeAddress, strFuncType) = memberInfo typeEnv memberTy
  in case nameOfPolymorphicFunction typeEnv env strFuncType "prn" of
       Just strFunctionPath ->
         unlines ["  temp = " ++ pathToC strFunctionPath ++ "(" ++ maybeTakeAddress ++ "p->" ++ memberName ++ "); "
                 ,"  size += snprintf(NULL, 0, \"%s \", temp);"
                 ,"  if(temp) { CARP_FREE(temp); temp = NULL; }"
                 ]
       Nothing ->
         if isExternalType typeEnv memberTy
         then unlines ["  size +=  11;"
                      ,"  if(temp) { CARP_FREE(temp); temp = NULL; }"
                      ]
         else "  // Failed to find str function for " ++ memberName ++ " : " ++ show memberTy ++ "\n"
