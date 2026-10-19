-- | Memoization on the identity of immutable values.
--
-- The compiler repeatedly derives the same data from the same environment:
-- an environment only changes between top-level forms, but lookups against
-- it happen many thousands of times per form. Threading caches through all
-- of those call sites is impractical, so these helpers remember the results
-- for the most recently seen arguments instead.
--
-- This is safe because the arguments are immutable: the same heap object
-- always gives the same result. A different object, even an equal one, is
-- simply a cache miss and the result is computed as usual.
module Memo
  ( MemoCache,
    newMemoCache,
    memoOnIdentity,
    memoOnIdentity2,
  )
where

import Control.Exception (evaluate)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName

-- | Remembers the results for the most recently seen keys, newest first.
newtype MemoCache k v = MemoCache (IORef [(k, v)])

-- | How many results a cache remembers. Lookups often alternate between a
-- few environments (e.g. a local one and the global one), so one isn't enough.
cacheSize :: Int
cacheSize = 8

-- | Create a cache. Bind each one to a top-level NOINLINE value.
newMemoCache :: IO (MemoCache k v)
newMemoCache = MemoCache <$> newIORef []

lookupOrCompute :: Eq k => MemoCache k v -> k -> v -> IO v
lookupOrCompute (MemoCache ref) key result = do
  entries <- readIORef ref
  case lookup key entries of
    Just cached -> pure cached
    Nothing -> do
      writeIORef ref (take cacheSize ((key, result) : entries))
      pure result

-- | @memoOnIdentity cache f x@ is @f x@, reusing an earlier result when @x@
-- is the same object as a recently seen argument.
memoOnIdentity :: MemoCache (StableName a) v -> (a -> v) -> a -> v
memoOnIdentity cache f x = unsafePerformIO $ do
  x' <- evaluate x -- The stable names of a thunk and of its value differ.
  name <- makeStableName x'
  lookupOrCompute cache name (f x')
{-# NOINLINE memoOnIdentity #-}

-- | Like 'memoOnIdentity', for a function of two arguments.
memoOnIdentity2 :: MemoCache (StableName a, StableName b) v -> (a -> b -> v) -> a -> b -> v
memoOnIdentity2 cache f x y = unsafePerformIO $ do
  x' <- evaluate x
  y' <- evaluate y
  nameX <- makeStableName x'
  nameY <- makeStableName y'
  lookupOrCompute cache (nameX, nameY) (f x' y')
{-# NOINLINE memoOnIdentity2 #-}
