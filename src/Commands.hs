module Commands where

import Parsing
import Emit
import Obj
import Types
import Infer
import Deftype
import ColorText
import Template
import Util
import Eval

-- -- | This function will show the resulting code of non-definitions.
-- -- | i.e. (Int.+ 2 3) => "_0 = 2 + 3"
-- consumeExpr :: Context -> XObj -> IO ReplCommand
-- consumeExpr ctx@(Context globalEnv typeEnv _ _ _ _) xobj =
--   do (expansionResult, newCtx) <- runStateT (expandAll globalEnv xobj) ctx
--      case expansionResult of
--        Left err -> return (ReplMacroError (show err))
--        Right expanded ->
--          case annotate typeEnv globalEnv (setFullyQualifiedSymbols typeEnv globalEnv expanded) of
--            Left err -> return (ReplTypeError (show err))
--            Right annXObjs -> return (ListOfCallbacks (map printC annXObjs))

-- printC :: XObj -> CommandCallback
-- printC xobj =
--   case checkForUnresolvedSymbols xobj of
--     Left e ->
--       (const (commandPrint [(XObj (Str (strWithColor Red (show e ++ ", can't print resulting code.\n"))) Nothing Nothing)]))
--     Right _ ->
--       (const (commandPrint [(XObj (Str (strWithColor Green (toC xobj))) Nothing Nothing)]))


addCommand :: String -> CommandFunctionType -> (String, Binder)
addCommand name callback =
  let path = SymPath [] name
      cmd = XObj (Lst [XObj (Command callback) (Just dummyInfo) Nothing
                      ,XObj (Sym path) Nothing Nothing
                      ])
            (Just dummyInfo) (Just DynamicTy)
  in (name, Binder cmd)
