module Primitives where

import Control.Monad.State.Lazy (StateT(..), get)
import qualified Data.Map as Map

import Obj
import TypeError
import Types

type Primitive = XObj -> Env -> [XObj] -> StateT Context IO (Either EvalError XObj)

makePrim :: String -> Int -> String -> Primitive -> (SymPath, Primitive)
makePrim name arity example callback =
  makePrim' name (Just arity) example callback

makeVarPrim :: String -> String -> Primitive -> (SymPath, Primitive)
makeVarPrim name example callback =
  makePrim' name Nothing example callback

makePrim' :: String -> Maybe Int -> String -> Primitive -> (SymPath, Primitive)
makePrim' name maybeArity example callback =
  let path = SymPath [] name
  in (path, wrapped)
  where wrapped =
          case maybeArity of
            Just a ->
              \x e l ->
                let ll = length l
                in (if ll /= a then err x a ll else callback x e l)
            Nothing -> callback
        err :: XObj -> Int -> Int -> StateT Context IO (Either EvalError XObj)
        err x a l = do
          ctx <- get
          return (makeEvalError ctx Nothing (
            "The primitive '" ++ name ++ "' expected " ++ show a ++
            " arguments, but got " ++ show l ++ ".\n\nExample Usage:\n```\n" ++
            example ++ "\n```\n") (info x))

primitiveFile :: Primitive
primitiveFile x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Str (infoFile info)) i t))
    Nothing ->
      return (makeEvalError ctx Nothing ("No information about object " ++ pretty x) (info x))

primitiveLine :: Primitive
primitiveLine x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Num IntTy (fromIntegral (infoLine info))) i t))
    Nothing ->
      return (makeEvalError ctx Nothing ("No information about object " ++ pretty x) (info x))

primitiveColumn :: Primitive
primitiveColumn x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Num IntTy (fromIntegral (infoColumn info))) i t))
    Nothing ->
      return (makeEvalError ctx Nothing ("No information about object " ++ pretty x) (info x))

primitives :: Map.Map SymPath Primitive
primitives = Map.fromList
  [ makePrim "quote" 1 "(quote x) ; where x is an actual symbol" (\_ _ [x] -> (return (Right x)))
  , makePrim "file" 1 "(file mysymbol)" primitiveFile
  , makePrim "line" 1 "(line mysymbol)" primitiveLine
  , makePrim "column" 1 "(column mysymbol)" primitiveColumn
  ]

