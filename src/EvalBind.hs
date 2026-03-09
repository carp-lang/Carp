module EvalBind
  ( bindExpr,
    bindProgram,
  )
where

import EvalBound
import Obj
import SymPath (SymPath)

bindProgram :: Context -> [XObj] -> [BoundExpr]
bindProgram ctx = map (bindExpr ctx)

bindExpr :: Context -> XObj -> BoundExpr
bindExpr ctx xobj@(XObj obj i t) =
  case obj of
    Sym spath mode -> BoundSymbol (classifySymbol ctx spath) mode i t
    Lst xs -> BoundList (map (bindExpr ctx) xs) i t
    Arr xs -> BoundArray (map (bindExpr ctx) xs) i t
    StaticArr xs -> BoundStaticArray (map (bindExpr ctx) xs) i t
    _ -> BoundLiteral xobj

classifySymbol :: Context -> SymPath -> BoundRef
classifySymbol _ spath = BoundUnresolved spath
