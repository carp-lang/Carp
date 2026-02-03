module Expand (expandAll, expand, ExpansionMode (..), replaceSourceInfoOnXObj) where

import Context
import Control.Monad.State (State, evalState, get, put)
import Data.Either (fromRight)
import Data.Foldable (foldlM)
import Env
import EvalError
import Info
import Obj
import Qualify
import TypeError
import Types
import Util

-- | Used for calling back to the 'eval' function in Eval.hs
type DynamicEvaluator = Context -> XObj -> IO (Context, Either EvalError XObj)

-- | Controls how much work 'expand' does.
--
-- 'MacroExpandOnly': Only expand macros. Symbols are left as-is, commands are
-- not invoked, modules are not converted to init calls, and if-branches are
-- not wrapped in do. Used during dynamic evaluation (fn body preparation,
-- macro result expansion).
--
-- 'FullExpand': Full pre-compilation expansion. Symbols are resolved, commands
-- are invoked, modules become init calls, if-branches get do-wrappers.
data ExpansionMode = MacroExpandOnly | FullExpand

-- | Keep expanding the form until it doesn't change anymore.
-- | Note: comparing environments is tricky! Make sure they *can* be equal, otherwise this won't work at all!
expandAll :: DynamicEvaluator -> Context -> XObj -> IO (Context, Either EvalError XObj)
expandAll eval ctx root =
  do
    (ctx', fullyExpanded) <- expandAllInternal root
    pure (ctx', fmap setNewIdentifiers fullyExpanded)
  where
    expandAllInternal xobj =
      do
        (newCtx, expansionResult) <- expand FullExpand eval ctx xobj
        case expansionResult of
          Right expanded ->
            if expanded == xobj
              then pure (newCtx, Right expanded)
              else expandAll eval newCtx expanded
          err -> pure (newCtx, err)

-- | Macro expansion of a single form
expand :: ExpansionMode -> DynamicEvaluator -> Context -> XObj -> IO (Context, Either EvalError XObj)
expand mode eval ctx xobj =
  case xobjObj xobj of
    Lst _ -> expandList xobj
    Arr _ -> expandArray xobj
    StaticArr _ -> expandStaticArray xobj
    Sym _ _ -> expandSymbol xobj
    -- This case is needed to ensure we expand naked mod names to initers consistently.
    -- Consider both:
    --   (width (address &(B 2)))
    --   (width B)
    -- The first case is correct code and was handled by expandList. The second case is an error and previously resulted in a loop because
    -- module expansion wasn't handled in expandSymbol, but handling it there
    -- by ending the expansion loop breaks init expansion in the first case,
    -- since expandList calls expand.
    -- So, we have no choice but to add a case here to cut the recursion and to expand this form consistently in all places.
    Mod e _ ->
      case mode of
        MacroExpandOnly -> pure (ctx, Right xobj)
        FullExpand ->
          let pathToModule = pathToEnv e
              implicitInit = XObj (Sym (SymPath pathToModule "init") Symbol) (xobjInfo xobj) (xobjTy xobj)
           in pure (ctx, Right implicitInit)
    _ -> pure (ctx, Right xobj)
  where
    expandList :: XObj -> IO (Context, Either EvalError XObj)
    expandList (XObj (Lst xobjs) i t) =
      case mode of
        MacroExpandOnly -> expandListMacroOnly xobjs i t
        FullExpand -> expandListFull xobjs i t
    expandList _ = error "Can't expand non-list in expandList."
    -- MacroExpandOnly: only detect and expand macros, pass through everything else
    expandListMacroOnly :: [XObj] -> Maybe Info -> Maybe Ty -> IO (Context, Either EvalError XObj)
    expandListMacroOnly [] _ _ = pure (ctx, Right xobj)
    expandListMacroOnly (XObj (Sym (SymPath [] "defmodule") _) _ _ : _) _ _ =
      pure (ctx, Right xobj)
    expandListMacroOnly [XObj (Sym (SymPath [] "quote") _) _ _, _] _ _ =
      pure (ctx, Right xobj)
    expandListMacroOnly [XObj (Lst (XObj Macro _ _ : _)) _ _] _ _ =
      eval ctx xobj
    expandListMacroOnly (x@(XObj (Sym _ _) _ _) : args) i' t' = do
      (_, f) <- eval ctx x
      case f of
        Right m@(XObj (Lst (XObj Macro _ _ : _)) _ _) ->
          eval ctx (XObj (Lst (m : args)) i' t')
        _ -> do
          (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) args
          pure
            ( newCtx,
              do
                ok <- expanded
                Right (XObj (Lst (x : ok)) i' t')
            )
    expandListMacroOnly objs i' t' = do
      (newCtx, expanded) <- foldlM successiveExpand (ctx, Right []) objs
      pure
        ( newCtx,
          do
            ok <- expanded
            Right (XObj (Lst ok) i' t')
        )
    -- FullExpand: comprehensive pre-compilation expansion
    expandListFull :: [XObj] -> Maybe Info -> Maybe Ty -> IO (Context, Either EvalError XObj)
    expandListFull [] _ _ = pure (ctx, Right xobj)
    expandListFull (XObj (External _) _ _ : _) _ _ = pure (ctx, Right xobj)
    expandListFull (XObj (Instantiate _) _ _ : _) _ _ = pure (ctx, Right xobj)
    expandListFull (XObj (Deftemplate _) _ _ : _) _ _ = pure (ctx, Right xobj)
    expandListFull (XObj (Defalias _) _ _ : _) _ _ = pure (ctx, Right xobj)
    expandListFull [defnExpr@(XObj (Defn _) _ _), name, args, body] i' t' =
      do
        (ctx', expandedBody) <- expand mode eval ctx body
        pure
          ( ctx',
            do
              okBody <- expandedBody
              Right (XObj (Lst [defnExpr, name, args, okBody]) i' t')
          )
    expandListFull [defExpr@(XObj Def _ _), name, expr] i' t' =
      do
        (ctx', expandedExpr) <- expand mode eval ctx expr
        pure
          ( ctx',
            do
              okExpr <- expandedExpr
              Right (XObj (Lst [defExpr, name, okExpr]) i' t')
          )
    expandListFull [theExpr@(XObj The _ _), typeXObj, value] i' t' =
      do
        (ctx', expandedValue) <- expand mode eval ctx value
        pure
          ( ctx',
            do
              okValue <- expandedValue
              Right (XObj (Lst [theExpr, typeXObj, okValue]) i' t')
          )
    expandListFull (XObj The _ _ : _) _ _ =
      pure (evalError ctx ("I didn't understand the `the` at " ++ prettyInfoFromXObj xobj ++ ":\n\n" ++ pretty xobj ++ "\n\nIs it valid? Every `the` needs to follow the form `(the type expression)`.") Nothing)
    expandListFull [ifExpr@(XObj If _ _), condition, trueBranch, falseBranch] i' t' =
      do
        (ctx', expandedCondition) <- expand mode eval ctx condition
        (ctx'', expandedTrueBranch) <- expand mode eval ctx' trueBranch
        (nct, expandedFalseBranch) <- expand mode eval ctx'' falseBranch
        pure
          ( nct,
            do
              okCondition <- expandedCondition
              okTrueBranch <- expandedTrueBranch
              okFalseBranch <- expandedFalseBranch
              -- This is a HACK so that each branch of the if statement
              -- has a "safe place" (= a do-expression with just one element)
              -- where it can store info about its deleters. Without this,
              -- An if statement with let-expression inside will duplicate
              -- the calls to Delete when emitting code.
              let wrappedTrue =
                    case okTrueBranch of
                      XObj (Lst (XObj Do _ _ : _)) _ _ -> okTrueBranch -- Has a do-expression already
                      _ -> XObj (Lst [XObj Do Nothing Nothing, okTrueBranch]) (xobjInfo okTrueBranch) Nothing
                  wrappedFalse =
                    case okFalseBranch of
                      XObj (Lst (XObj Do _ _ : _)) _ _ -> okFalseBranch -- Has a do-expression already
                      _ -> XObj (Lst [XObj Do Nothing Nothing, okFalseBranch]) (xobjInfo okFalseBranch) Nothing
              Right (XObj (Lst [ifExpr, okCondition, wrappedTrue, wrappedFalse]) i' t')
          )
    expandListFull (letExpr@(XObj Let _ _) : XObj (Arr bindings) bindi bindt : body : _) i' t' =
      if even (length bindings)
        then do
          (ctx', bind) <- foldlM successiveExpandLR (ctx, Right []) (pairwise bindings)
          (newCtx, expandedBody) <- expand mode eval ctx' body
          pure
            ( newCtx,
              do
                okBindings <- bind
                okBody <- expandedBody
                Right (XObj (Lst [letExpr, XObj (Arr (concat okBindings)) bindi bindt, okBody]) i' t')
            )
        else
          pure
            ( evalError
                ctx
                ( "I ecountered an odd number of forms inside a `let` (`"
                    ++ pretty xobj
                    ++ "`)"
                )
                (xobjInfo xobj)
            )
    expandListFull (matchExpr@(XObj (Match _) _ _) : expr : rest) i' t'
      | null rest =
        pure (evalError ctx "I encountered a `match` without forms" (xobjInfo xobj))
      | even (length rest) =
        do
          (ctx', expandedExpr) <- expand mode eval ctx expr
          (newCtx, expandedPairs) <- foldlM successiveExpandLRMatch (ctx', Right []) (pairwise rest)
          pure
            ( newCtx,
              do
                okExpandedExpr <- expandedExpr
                okExpandedPairs <- expandedPairs
                Right (XObj (Lst (matchExpr : okExpandedExpr : concat okExpandedPairs)) i' t')
            )
      | otherwise =
        pure
          ( evalError
              ctx
              "I encountered an odd number of forms inside a `match`"
              (xobjInfo xobj)
          )
    expandListFull (doExpr@(XObj Do _ _) : expressions) i' t' =
      do
        (newCtx, expandedExpressions) <- foldlM successiveExpand (ctx, Right []) expressions
        pure
          ( newCtx,
            do
              okExpressions <- expandedExpressions
              Right (XObj (Lst (doExpr : okExpressions)) i' t')
          )
    expandListFull [withExpr@(XObj With _ _), pathExpr@(XObj (Sym _ _) _ _), expression] i' t' =
      do
        (newCtx, expandedExpression) <- expand mode eval ctx expression
        pure
          ( newCtx,
            do
              okExpression <- expandedExpression
              Right (XObj (Lst [withExpr, pathExpr, okExpression]) i' t') -- Replace the with-expression with just the expression!
          )
    expandListFull [XObj With _ _, _, _] _ _ =
      pure
        ( evalError
            ctx
            ( "I encountered the value `" ++ pretty xobj
                ++ "` inside a `with` at "
                ++ prettyInfoFromXObj xobj
                ++ ".\n\n`with` accepts only symbols."
            )
            Nothing
        )
    expandListFull (XObj With _ _ : _) _ _ =
      pure
        ( evalError
            ctx
            ( "I encountered multiple forms inside a `with` at "
                ++ prettyInfoFromXObj xobj
                ++ ".\n\n`with` accepts only one expression, except at the top level."
            )
            Nothing
        )
    expandListFull [r@(XObj Ref _ _), arg] _ _ = do
      (ctx', expandedArg) <- expand mode eval ctx arg
      case expandedArg of
        Left err -> pure (ctx, Left err)
        Right right -> pure (ctx', Right (XObj (Lst [r, right]) (xobjInfo xobj) (xobjTy xobj)))
    expandListFull (XObj Ref _ _ : _) _ _ =
      pure
        ( evalError
            ctx
            ("`ref` takes a single argument, but I got `" ++ pretty xobj ++ "`.")
            (xobjInfo xobj)
        )
    expandListFull (XObj (Mod modEnv _) _ _ : args) _ _ =
      let pathToModule = pathToEnv modEnv
          implicitInit = XObj (Sym (SymPath pathToModule "init") Symbol) (xobjInfo xobj) (xobjTy xobj)
       in expand mode eval ctx (XObj (Lst (implicitInit : args)) (xobjInfo xobj) (xobjTy xobj))
    expandListFull (f : args) i' t' =
      if isSpecialSym f
        then do
          (ctx', s) <- eval ctx f
          let sym = fromRight (error "expand: failed to expand special symbol") $ s
          expand mode eval ctx' (XObj (Lst (sym : args)) (xobjInfo xobj) (xobjTy xobj))
        else do
          (_, expandedF) <- expand mode eval ctx f
          (ctx'', expandedArgs) <- foldlM successiveExpand (ctx, Right []) args
          case expandedF of
            Right (XObj (Lst [XObj Dynamic _ _, _, XObj (Arr _) _ _, _]) _ _) ->
              eval ctx'' xobj
            Right (XObj (Lst [XObj Macro _ _, _, XObj (Arr _) _ _, _]) _ _) ->
              eval ctx'' xobj
            Right (XObj (Lst [XObj (Command (NullaryCommandFunction nullary)) _ _, _, _]) _ _) ->
              nullary ctx''
            Right (XObj (Lst [XObj (Command (UnaryCommandFunction unary)) _ _, _, _]) _ _) ->
              case expandedArgs of
                Right [x] -> unary ctx'' x
                _ -> error "expanding args"
            Right (XObj (Lst [XObj (Command (BinaryCommandFunction binary)) _ _, _, _]) _ _) ->
              case expandedArgs of
                Right [x, y] -> binary ctx'' x y
                _ -> error "expanding args"
            Right (XObj (Lst [XObj (Command (TernaryCommandFunction ternary)) _ _, _, _]) _ _) ->
              case expandedArgs of
                Right [x, y, z] -> ternary ctx'' x y z
                _ -> error "expanding args"
            Right (XObj (Lst [XObj (Command (VariadicCommandFunction variadic)) _ _, _, _]) _ _) ->
              case expandedArgs of
                Right ea -> variadic ctx'' ea
                _ -> error "expanding args"
            Right _ ->
              pure
                ( ctx'',
                  do
                    okF <- expandedF
                    okArgs <- expandedArgs
                    Right (XObj (Lst (okF : okArgs)) i' t')
                )
            Left err -> pure (ctx'', Left err)
    expandArray :: XObj -> IO (Context, Either EvalError XObj)
    expandArray (XObj (Arr xobjs) i t) =
      do
        (newCtx, evaledXObjs) <- foldlM successiveExpand (ctx, Right []) xobjs
        pure
          ( newCtx,
            do
              okXObjs <- evaledXObjs
              Right (XObj (Arr okXObjs) i t)
          )
    expandArray _ = error "Can't expand non-array in expandArray."
    expandStaticArray :: XObj -> IO (Context, Either EvalError XObj)
    expandStaticArray (XObj (StaticArr xobjs) i t) =
      do
        (newCtx, evaledXObjs) <- foldlM successiveExpand (ctx, Right []) xobjs
        pure
          ( newCtx,
            do
              okXObjs <- evaledXObjs
              Right (XObj (StaticArr okXObjs) i t)
          )
    expandStaticArray _ = error "Can't expand non-static-array in expandStaticArray."
    expandSymbol :: XObj -> IO (Context, Either EvalError XObj)
    expandSymbol sym@(XObj (Sym path@(SymPath p name) _) _ _) =
      case mode of
        MacroExpandOnly -> pure (ctx, Right xobj)
        FullExpand ->
          case p of
            [] ->
              case getBinder (contextEnv ctx :: Env) name of
                Right (Binder _ found) -> pure (ctx, Right (matchDef found))
                _ -> searchForBinder
            _ -> searchForBinder
          where
            qpath = qualifyPath ctx (SymPath [] name)
            searchForBinder =
              case lookupBinderInGlobalEnv ctx path <> lookupBinderInGlobalEnv ctx qpath of
                Right (Binder meta found) -> isPrivate meta (matchDef found) (getPath found)
                Left _ -> pure (ctx, Right xobj) -- symbols that are not found are left as-is
            isPrivate m x (SymPath p' _) =
              pure $
                if (metaIsTrue m "private") && (not (null p') && p' /= contextPath ctx)
                  then evalError ctx (show (PrivateBinding path)) (xobjInfo sym)
                  else (ctx, Right x)
            matchDef (XObj (Lst (XObj (External _) _ _ : _)) _ _) = xobj
            matchDef (XObj (Lst (XObj (Instantiate _) _ _ : _)) _ _) = xobj
            matchDef (XObj (Lst (XObj (Deftemplate _) _ _ : _)) _ _) = xobj
            matchDef (XObj (Lst (XObj (Defn _) _ _ : _)) _ _) = xobj
            matchDef (XObj (Lst (XObj Def _ _ : _)) _ _) = xobj
            matchDef (XObj (Lst (XObj MetaStub _ _ : _)) _ _) = xobj
            matchDef (XObj (Lst [XObj DefDynamic _ _, _, value]) _ _) = matchDef value
            matchDef x = x
    expandSymbol _ = pure (evalError ctx "Can't expand non-symbol in expandSymbol." Nothing)
    successiveExpand (ctx', acc) e =
      case acc of
        Left _ -> pure (ctx', acc)
        Right lst -> do
          (newCtx, expanded) <- expand mode eval ctx' e
          pure $ case expanded of
            Right err -> (newCtx, Right (lst ++ [err]))
            Left err -> (newCtx, Left err)
    successiveExpandLR (ctx', acc) (l, r) =
      case acc of
        Left _ -> pure (ctx', acc)
        Right lst -> do
          (newCtx, expandedR) <- expand mode eval ctx' r
          case expandedR of
            Right v -> pure (newCtx, Right (lst ++ [[l, v]]))
            Left err -> pure (newCtx, Left err)
    successiveExpandLRMatch a (l, r) =
      case traverseExpandLiteral l of
        Left (err, x) -> pure (evalError ctx err (xobjInfo x))
        _ -> successiveExpandLR a (l, r)
    traverseExpandLiteral (XObj (Lst objs) _ _) =
      case mapM traverseExpandLiteral objs of
        Left e -> Left e
        _ -> Right ()
    traverseExpandLiteral (XObj (Sym _ _) _ _) = Right ()
    traverseExpandLiteral other =
      Left ("I can’t use `" ++ pretty other ++ "` in match, only lists and symbols are allowed", other)

-- | Replace all the infoIdentifier:s on all nested XObj:s
setNewIdentifiers :: XObj -> XObj
setNewIdentifiers root =
  let final = evalState (visit root) 0
   in final
  where
    --trace ("ROOT: " ++ prettyTyped root ++ "FINAL: " ++ prettyTyped final) final

    visit :: XObj -> State Int XObj
    visit xobj =
      case xobjObj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        (StaticArr _) -> visitStaticArray xobj
        _ -> bumpAndSet xobj
    visitList :: XObj -> State Int XObj
    visitList (XObj (Lst xobjs) i t) =
      do
        visited <- mapM visit xobjs
        let xobj' = XObj (Lst visited) i t
        bumpAndSet xobj'
    visitList _ = error "The function 'visitList' only accepts XObjs with lists in them."
    visitArray :: XObj -> State Int XObj
    visitArray (XObj (Arr xobjs) i t) =
      do
        visited <- mapM visit xobjs
        let xobj' = XObj (Arr visited) i t
        bumpAndSet xobj'
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."
    visitStaticArray :: XObj -> State Int XObj
    visitStaticArray (XObj (StaticArr xobjs) i t) =
      do
        visited <- mapM visit xobjs
        let xobj' = XObj (StaticArr visited) i t
        bumpAndSet xobj'
    visitStaticArray _ = error "The function 'visitStaticArray' only accepts XObjs with arrays in them."
    bumpAndSet :: XObj -> State Int XObj
    bumpAndSet xobj =
      do
        counter <- get
        put (counter + 1)
        pure $ case xobjInfo xobj of
          Just i -> (xobj {xobjInfo = Just (i {infoIdentifier = counter})})
          Nothing -> xobj

-- | Replaces the file, line and column info on an XObj an all its children.
replaceSourceInfo :: FilePath -> Int -> Int -> XObj -> XObj
replaceSourceInfo newFile newLine newColumn = visit
  where
    visit :: XObj -> XObj
    visit xobj =
      case xobjObj xobj of
        (Lst _) -> visitList xobj
        (Arr _) -> visitArray xobj
        _ -> setNewInfo xobj
    visitList :: XObj -> XObj
    visitList (XObj (Lst xobjs) i t) =
      setNewInfo (XObj (Lst (map visit xobjs)) i t)
    visitList _ =
      error "The function 'visitList' only accepts XObjs with lists in them."
    visitArray :: XObj -> XObj
    visitArray (XObj (Arr xobjs) i t) =
      setNewInfo (XObj (Arr (map visit xobjs)) i t)
    visitArray _ = error "The function 'visitArray' only accepts XObjs with arrays in them."
    setNewInfo :: XObj -> XObj
    setNewInfo xobj =
      case xobjInfo xobj of
        Just i ->
          ( xobj
              { xobjInfo =
                  Just
                    ( i
                        { infoFile = newFile,
                          infoLine = newLine,
                          infoColumn = newColumn
                        }
                    )
              }
          )
        Nothing -> xobj

replaceSourceInfoOnXObj :: Maybe Info -> XObj -> XObj
replaceSourceInfoOnXObj newInfo xobj =
  case newInfo of
    Just i -> replaceSourceInfo (infoFile i) (infoLine i) (infoColumn i) xobj
    Nothing -> xobj
