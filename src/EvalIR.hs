module EvalIR
  ( EvalIR (..),
    lowerExpr,
    lowerProgram,
    lowerBoundExpr,
    raiseExpr,
    raiseProgram,
    irToBoundExpr,
    lowerRoundTripEquivalent,
  )
where

import EvalBind (bindExpr)
import EvalBound
import Info (Info)
import Obj
import SymPath
import Types (Ty)

-- | Compact evaluator IR for dynamic execution planning.
--
-- This stage is intentionally side-effect free: we lower from bound forms and
-- can raise back to bound forms before execution.
data EvalIR
  = IRSymbol BoundRef SymbolMode (Maybe Info) (Maybe Ty)
  | IRLiteral XObj
  | IRArray [EvalIR] (Maybe Info) (Maybe Ty)
  | IRStaticArray [EvalIR] (Maybe Info) (Maybe Ty)
  | IRIf EvalIR EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IRLet EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IRFn EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IRDo [EvalIR] (Maybe Info) (Maybe Ty)
  | IRWhile EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IRWith EvalIR [EvalIR] (Maybe Info) (Maybe Ty)
  | IRSet EvalIR EvalIR (Maybe Info) (Maybe Ty)
  | IRCall EvalIR [EvalIR] (Maybe Info) (Maybe Ty)
  | IRList [EvalIR] (Maybe Info) (Maybe Ty)
  deriving (Show, Eq)

lowerProgram :: Context -> [XObj] -> [EvalIR]
lowerProgram ctx = map (lowerExpr ctx)

lowerExpr :: Context -> XObj -> EvalIR
lowerExpr ctx = lowerBoundExpr . bindExpr ctx

raiseProgram :: [EvalIR] -> [XObj]
raiseProgram = map raiseExpr

raiseExpr :: EvalIR -> XObj
raiseExpr = boundToXObj . irToBoundExpr

lowerRoundTripEquivalent :: Context -> XObj -> Bool
lowerRoundTripEquivalent ctx xobj =
  let bound = bindExpr ctx xobj
   in equivalentBoundExpr bound (irToBoundExpr (lowerBoundExpr bound))

lowerBoundExpr :: BoundExpr -> EvalIR
lowerBoundExpr bound =
  case bound of
    BoundSymbol ref mode i t -> IRSymbol ref mode i t
    BoundArray xs i t -> IRArray (map lowerBoundExpr xs) i t
    BoundStaticArray xs i t -> IRStaticArray (map lowerBoundExpr xs) i t
    BoundLiteral x -> IRLiteral x
    BoundList [] i t -> IRList [] i t
    BoundList (headExpr : tailExprs) i t ->
      case keywordName headExpr of
        Just "if" ->
          case tailExprs of
            [condExpr, trueExpr, falseExpr] ->
              IRIf (lowerBoundExpr condExpr) (lowerBoundExpr trueExpr) (lowerBoundExpr falseExpr) i t
            _ -> genericCall
        Just "let" ->
          case tailExprs of
            [bindingsExpr, bodyExpr] ->
              IRLet (lowerBoundExpr bindingsExpr) (lowerBoundExpr bodyExpr) i t
            _ -> genericCall
        Just "fn" ->
          case tailExprs of
            [argsExpr, bodyExpr] ->
              IRFn (lowerBoundExpr argsExpr) (lowerBoundExpr bodyExpr) i t
            _ -> genericCall
        Just "do" ->
          IRDo (map lowerBoundExpr tailExprs) i t
        Just "while" ->
          case tailExprs of
            [condExpr, bodyExpr] ->
              IRWhile (lowerBoundExpr condExpr) (lowerBoundExpr bodyExpr) i t
            _ -> genericCall
        Just "with" ->
          case tailExprs of
            (symExpr : formsExpr) ->
              IRWith (lowerBoundExpr symExpr) (map lowerBoundExpr formsExpr) i t
            _ -> genericCall
        Just "set!" ->
          case tailExprs of
            [targetExpr, valueExpr] ->
              IRSet (lowerBoundExpr targetExpr) (lowerBoundExpr valueExpr) i t
            _ -> genericCall
        _ ->
          case headExpr of
            BoundLiteral (XObj Do _ _) ->
              IRDo (map lowerBoundExpr tailExprs) i t
            _ -> genericCall
      where
        genericCall = IRCall (lowerBoundExpr headExpr) (map lowerBoundExpr tailExprs) i t

irToBoundExpr :: EvalIR -> BoundExpr
irToBoundExpr ir =
  case ir of
    IRSymbol ref mode i t -> BoundSymbol ref mode i t
    IRLiteral x -> BoundLiteral x
    IRArray xs i t -> BoundArray (map irToBoundExpr xs) i t
    IRStaticArray xs i t -> BoundStaticArray (map irToBoundExpr xs) i t
    IRIf condExpr trueExpr falseExpr i t ->
      BoundList [keyword "if", irToBoundExpr condExpr, irToBoundExpr trueExpr, irToBoundExpr falseExpr] i t
    IRLet bindingsExpr bodyExpr i t ->
      BoundList [keyword "let", irToBoundExpr bindingsExpr, irToBoundExpr bodyExpr] i t
    IRFn argsExpr bodyExpr i t ->
      BoundList [keyword "fn", irToBoundExpr argsExpr, irToBoundExpr bodyExpr] i t
    IRDo xs i t ->
      BoundList (keyword "do" : map irToBoundExpr xs) i t
    IRWhile condExpr bodyExpr i t ->
      BoundList [keyword "while", irToBoundExpr condExpr, irToBoundExpr bodyExpr] i t
    IRWith symExpr formsExpr i t ->
      BoundList (keyword "with" : irToBoundExpr symExpr : map irToBoundExpr formsExpr) i t
    IRSet targetExpr valueExpr i t ->
      BoundList [keyword "set!", irToBoundExpr targetExpr, irToBoundExpr valueExpr] i t
    IRCall f args i t ->
      BoundList (irToBoundExpr f : map irToBoundExpr args) i t
    IRList xs i t ->
      BoundList (map irToBoundExpr xs) i t
  where
    keyword name = BoundSymbol (BoundUnresolved (SymPath [] name)) Symbol Nothing Nothing

keywordName :: BoundExpr -> Maybe String
keywordName (BoundSymbol ref mode _ _) =
  case refToSymPath ref of
    SymPath [] n | mode == Symbol -> Just n
    _ -> Nothing
keywordName _ = Nothing

equivalentBoundExpr :: BoundExpr -> BoundExpr -> Bool
equivalentBoundExpr a b =
  case (a, b) of
    (BoundSymbol refA modeA _ _, BoundSymbol refB modeB _ _) ->
      modeA == modeB && refToSymPath refA == refToSymPath refB
    (BoundList xsA _ _, BoundList xsB _ _) ->
      length xsA == length xsB && and (zipWith equivalentBoundExpr xsA xsB)
    (BoundArray xsA _ _, BoundArray xsB _ _) ->
      length xsA == length xsB && and (zipWith equivalentBoundExpr xsA xsB)
    (BoundStaticArray xsA _ _, BoundStaticArray xsB _ _) ->
      length xsA == length xsB && and (zipWith equivalentBoundExpr xsA xsB)
    (BoundLiteral xA, BoundLiteral xB) ->
      pretty xA == pretty xB
    _ -> False
