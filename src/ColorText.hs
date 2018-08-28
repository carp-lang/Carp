module ColorText where

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
                White -> "37"

putStrWithColor :: TextColor -> String -> IO ()
putStrWithColor color str = putStr (strWithColor color str)

putStrLnWithColor :: TextColor -> String -> IO ()
putStrLnWithColor color str = putStrWithColor color (str ++ "\n")
