module ColorText where

import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Util

data TextColor = Blue | Red | Yellow | Green | White

strWithColor :: TextColor -> String -> String
strWithColor color str =
    if useColors
    then "\x1b[" ++ col ++ "m" ++ str ++ "\x1b[0m"
    else str
  where useColors = platform /= Windows
        col = case color of
                Red -> "31"
                Green -> "32"
                Yellow -> "33"
                Blue -> "34"
                White -> "37" -- TODO: Use 0 instead?

putStrWithColor :: TextColor -> String -> IO ()
putStrWithColor color str =
  do
    istty <- queryTerminal stdOutput
    putStr $ if istty then strWithColor color str else str

putStrLnWithColor :: TextColor -> String -> IO ()
putStrLnWithColor color str = putStrWithColor color (str ++ "\n")
