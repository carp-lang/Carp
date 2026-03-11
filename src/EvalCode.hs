module EvalCode
  ( EvalInstr (..),
    EvalCode (..),
    ResolverHandle (..),
    mkEvalCode,
    SymbolId,
  )
where

import Data.Array (Array, listArray)
import EvalBound (BoundRef (..))
import EvalIR (EvalIR)
import Info (Info)
import Obj (SymbolMode, XObj)
import SymPath (SymPath)
import Types (Ty)

type SymbolId = Int

data ResolverHandle
  = RHLocalSlot Int
  | RHGlobal SymPath
  | RHDynamic SymPath
  | RHQualified SymPath
  | RHUnqualified String
  deriving (Show, Eq)

data EvalInstr
  = IPushConst XObj
  | IResolveSymbol SymbolId ResolverHandle BoundRef SymbolMode (Maybe Info) (Maybe Ty)
  | IMakeArray Int (Maybe Info) (Maybe Ty)
  | IMakeStaticArray Int (Maybe Info) (Maybe Ty)
  | IExecCallSymbol SymbolId ResolverHandle BoundRef SymbolMode [EvalIR] [EvalCode] (Maybe Info) (Maybe Ty)
  | IExecCall EvalIR [EvalIR] [EvalCode] (Maybe Info) (Maybe Ty)
  | IExecLet EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IExecFn EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IExecWhile EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IExecWith EvalIR [EvalIR] (Maybe Info) (Maybe Ty)
  | IExecSet EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IDrop
  | IJumpIfFalseRel Int (Maybe Info)
  | IJumpRel Int
  | ITrap String (Maybe Info)
  | IHalt
  deriving (Show, Eq)

data EvalCode = EvalCode
  { evalCodeList :: [EvalInstr],
    evalCodeArray :: Array Int EvalInstr,
    evalCodeLen :: Int
  }
  deriving (Show, Eq)

mkEvalCode :: [EvalInstr] -> EvalCode
mkEvalCode xs =
  let n = length xs
      arr =
        if n == 0
          then listArray (0, 0) [IHalt]
          else listArray (0, n - 1) xs
   in EvalCode xs arr n
