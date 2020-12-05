module Context
  ( insertInGlobalEnv,
    insertInTypeEnv,
  )
where

import Lookup
import Obj
import SymPath

insertInGlobalEnv :: Context -> SymPath -> Binder -> Context
insertInGlobalEnv ctx path binder =
  let globalEnv = contextGlobalEnv ctx
   in ctx {contextGlobalEnv = envInsertAt globalEnv path binder}

insertInTypeEnv :: Context -> SymPath -> Binder -> Context
insertInTypeEnv ctx path binder =
  let typeEnv = getTypeEnv (contextTypeEnv ctx)
   in ctx {contextTypeEnv = TypeEnv (envInsertAt typeEnv path binder)}
