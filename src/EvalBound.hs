module EvalBound
  ( BoundRef (..),
    BoundExpr (..),
    refToSymPath,
    boundToXObj,
  )
where

import Info (Info)
import Obj
import SymPath
import Types (Ty)

-- | Reference target for a bound symbol.
data BoundRef
  = BoundLocalSlot Int
  | BoundInternal SymPath
  | BoundGlobal SymPath
  | BoundDynamic SymPath
  | BoundUnresolved SymPath
  deriving (Show, Eq, Ord)

-- | Stage-A bound representation of dynamic forms.
data BoundExpr
  = BoundSymbol BoundRef SymbolMode (Maybe Info) (Maybe Ty)
  | BoundList [BoundExpr] (Maybe Info) (Maybe Ty)
  | BoundArray [BoundExpr] (Maybe Info) (Maybe Ty)
  | BoundStaticArray [BoundExpr] (Maybe Info) (Maybe Ty)
  | BoundLiteral XObj
  deriving (Show, Eq)

-- | Convert a binding reference back into a symbol path.
refToSymPath :: BoundRef -> SymPath
refToSymPath (BoundLocalSlot slot) = SymPath [] ("$slot" ++ show slot)
refToSymPath (BoundInternal path) = path
refToSymPath (BoundGlobal path) = path
refToSymPath (BoundDynamic path) = path
refToSymPath (BoundUnresolved path) = path

-- | Lower a bound expression back into an `XObj`.
--
-- Used for diagnostics and compatibility boundaries (error reporting, form
-- reconstruction), not as a legacy evaluator escape hatch.
boundToXObj :: BoundExpr -> XObj
boundToXObj (BoundSymbol ref mode i t) = XObj (Sym (refToSymPath ref) mode) i t
boundToXObj (BoundList xs i t) = XObj (Lst (map boundToXObj xs)) i t
boundToXObj (BoundArray xs i t) = XObj (Arr (map boundToXObj xs)) i t
boundToXObj (BoundStaticArray xs i t) = XObj (StaticArr (map boundToXObj xs)) i t
boundToXObj (BoundLiteral x) = x
