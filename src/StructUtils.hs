module StructUtils where

import Obj
import Types
import Util
import Lookup
import Polymorphism

-- | Generate C code for converting a member variable to a string and appending it to a buffer.
memberPrn :: TypeEnv -> Env -> (String, Ty) -> String
memberPrn typeEnv env (memberName, memberTy) =
  let refOrNotRefType = if isManaged typeEnv memberTy then RefTy memberTy else memberTy
      maybeTakeAddress = if isManaged typeEnv memberTy then "&" else ""
      strFuncType = FuncTy [refOrNotRefType] StringTy
   in case nameOfPolymorphicFunction typeEnv env strFuncType "prn" of
        Just strFunctionPath ->
          unlines ["  temp = " ++ pathToC strFunctionPath ++ "(" ++ maybeTakeAddress ++ "p->" ++ memberName ++ ");"
                  , "  snprintf(bufferPtr, size, \"%s \", temp);"
                  , "  bufferPtr += strlen(temp) + 1;"
                  , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                  ]
        Nothing ->
          if isExternalType typeEnv memberTy
          then unlines [ "  tempsize = snprintf(NULL, 0, \"%p\", p->" ++ memberName ++ ");"
                       , "  temp = malloc(tempsize);"
                       , "  snprintf(temp, tempsize, \"%p\", p->" ++ memberName ++ ");"
                       , "  snprintf(bufferPtr, size, \"%s \", temp);"
                       , "  bufferPtr += strlen(temp) + 1;"
                       , "  if(temp) { CARP_FREE(temp); temp = NULL; }"
                       ]
          else "  // Failed to find str function for " ++ memberName ++ " : " ++ show memberTy ++ "\n"

-- | Calculate the size for prn:ing a member of a struct
memberPrnSize :: TypeEnv -> Env -> (String, Ty) -> String
memberPrnSize typeEnv env (memberName, memberTy) =
  let refOrNotRefType = if isManaged typeEnv memberTy then RefTy memberTy else memberTy
      maybeTakeAddress = if isManaged typeEnv memberTy then "&" else ""
      strFuncType = FuncTy [refOrNotRefType] StringTy
  in case nameOfPolymorphicFunction typeEnv env strFuncType "prn" of
       Just strFunctionPath ->
         unlines ["  temp = " ++ pathToC strFunctionPath ++ "(" ++ maybeTakeAddress ++ "p->" ++ memberName ++ "); "
                 ,"  size += snprintf(NULL, 0, \"%s \", temp);"
                 ,"  if(temp) { CARP_FREE(temp); temp = NULL; }"
                 ]
       Nothing ->
         if isExternalType typeEnv memberTy
         then unlines ["  size +=  snprintf(NULL, 0, \"%p \", p->" ++ memberName ++ ");"
                      ,"  if(temp) { CARP_FREE(temp); temp = NULL; }"
                      ]
         else "  // Failed to find str function for " ++ memberName ++ " : " ++ show memberTy ++ "\n"
