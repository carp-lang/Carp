module ColorText where

import System.Console.ANSI hiding (Blue, Red, Yellow, Green, White)
import System.IO

import Util

data TextColor = Blue | Red | Yellow | Green | White

strWithColor :: TextColor -> String -> String
strWithColor color str =
    "\x1b[" ++ col ++ "m" ++ str ++ "\x1b[0m"
  where
        col = case color of
                Red -> "31"
                Green -> "32"
                Yellow -> "33"
                Blue -> "34"
                White -> "37" -- TODO: Use 0 instead?

putStrWithColor :: TextColor -> String -> IO ()
putStrWithColor color str =
  do
    istty <- hSupportsANSIColor stdout
    putStr $ if istty then strWithColor color str else str

putStrLnWithColor :: TextColor -> String -> IO ()
putStrLnWithColor color str = putStrWithColor color (str ++ "\n")
