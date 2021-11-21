module Debug where

import qualified Map
import Obj
import SymPath
import Util

showEnvBinderValues :: Env -> String
showEnvBinderValues =
  joinLines . (map (pretty . binderXObj . snd)) . Map.toList . envBindings

showContextGlobalValues :: Context -> String
showContextGlobalValues =
  (++) "Context Global Bindings:\n" . showEnvBinderValues . contextGlobalEnv

showBinderInEnv :: Env -> SymPath -> String
showBinderInEnv e spath =
  joinLines (map pretty (filter (\p -> (getPath p) == spath) (map (binderXObj . snd) (Map.toList (envBindings e)))))
