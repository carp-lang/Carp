module EvalVMCore
  ( VMFrame (..),
    mkFrame,
    runFrame,
    newCodeStore,
    registerCode,
    runRegisteredCallable,
  )
where

import qualified Data.Array as Arr
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import EvalBound (BoundRef (..))
import EvalCode (EvalCode (..), EvalInstr (..))
import qualified Map
import Obj

data VMFrame = VMFrame
  { vmFrameCode :: EvalCode,
    vmFrameIP :: Int,
    vmFrameStack :: [XObj],
    vmFrameLocals :: Map.Map Int XObj
  }

mkFrame :: EvalCode -> Map.Map Int XObj -> VMFrame
mkFrame code locals =
  VMFrame
    { vmFrameCode = code,
      vmFrameIP = 0,
      vmFrameStack = [],
      vmFrameLocals = locals
    }

runFrame :: VMFrame -> Either String XObj
runFrame frame = go frame
  where
    go current =
      let code = vmFrameCode current
          pc = vmFrameIP current
          stack = vmFrameStack current
          locals = vmFrameLocals current
       in if pc < 0 || pc >= evalCodeLen code
            then Left "EvalVMCore instruction pointer out of range."
            else case evalCodeArray code Arr.! pc of
              IPushConst x ->
                go current {vmFrameIP = pc + 1, vmFrameStack = x : stack}
              IResolveSymbol _ _ ref _ _ _ ->
                case ref of
                  BoundLocalSlot slot ->
                    case Map.lookup slot locals of
                      Just value ->
                        go current {vmFrameIP = pc + 1, vmFrameStack = value : stack}
                      Nothing ->
                        Left ("EvalVMCore missing local slot: " ++ show slot)
                  _ ->
                    Left "EvalVMCore only supports BoundLocalSlot in runFrame."
              IMakeArray n _ _ ->
                case popN n stack of
                  Left err -> Left err
                  Right (items, rest) ->
                    go current {vmFrameIP = pc + 1, vmFrameStack = XObj (Arr (reverse items)) Nothing Nothing : rest}
              IMakeStaticArray n _ _ ->
                case popN n stack of
                  Left err -> Left err
                  Right (items, rest) ->
                    go current {vmFrameIP = pc + 1, vmFrameStack = XObj (StaticArr (reverse items)) Nothing Nothing : rest}
              IDrop ->
                case stack of
                  [] -> Left "EvalVMCore stack underflow on drop."
                  (_ : rest) -> go current {vmFrameIP = pc + 1, vmFrameStack = rest}
              IJumpIfFalseRel offset _ ->
                case stack of
                  [] -> Left "EvalVMCore stack underflow on conditional jump."
                  (top : rest) ->
                    case xobjObj top of
                      Bol False -> go current {vmFrameIP = pc + 1 + offset, vmFrameStack = rest}
                      Bol True -> go current {vmFrameIP = pc + 1, vmFrameStack = rest}
                      _ -> Left "EvalVMCore expected Bool on conditional jump."
              IJumpRel offset ->
                go current {vmFrameIP = pc + 1 + offset}
              IHalt ->
                case stack of
                  top : _ -> Right top
                  [] -> Right (XObj (Lst []) Nothing Nothing)
              _ ->
                Left "EvalVMCore instruction not yet supported in substrate."
    popN :: Int -> [XObj] -> Either String ([XObj], [XObj])
    popN n xs
      | n < 0 = Left "EvalVMCore negative pop length."
      | n == 0 = Right ([], xs)
      | otherwise =
        case xs of
          [] -> Left "EvalVMCore stack underflow while collecting args."
          (y : ys) ->
            case popN (n - 1) ys of
              Left e -> Left e
              Right (collected, rest) -> Right (y : collected, rest)

type CodeStore = IORef (Int, Map.Map Int EvalCode)

newCodeStore :: IO CodeStore
newCodeStore = newIORef (0, Map.empty)

registerCode :: IORef (Int, Map.Map Int EvalCode) -> EvalCode -> IO Int
registerCode store code =
  atomicModifyIORef' store $ \(nextId, entries) ->
    let cid = nextId + 1
     in ((cid, Map.insert cid code entries), cid)

runRegisteredCallable :: IORef (Int, Map.Map Int EvalCode) -> Int -> [XObj] -> IO (Either String XObj)
runRegisteredCallable store cid args = do
  (_, entries) <- readIORef store
  case Map.lookup cid entries of
    Nothing -> pure (Left ("EvalVMCore missing code id: " ++ show cid))
    Just code ->
      let locals = Map.fromList (zip [0 ..] args)
       in pure (runFrame (mkFrame code locals))
