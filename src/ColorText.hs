module ColorText where

data TextColor = Blue | Red | Yellow | Green | White

strWithColor :: TextColor -> String -> String
strWithColor color str = "\x1b[" ++ col ++ "m" ++ str ++ "\x1b[37m"
  where col = case color of
                Red -> "31"
                Green -> "32"
                Yellow -> "33"
                Blue -> "34"
                White -> "37"

putStrWithColor :: TextColor -> String -> IO ()
putStrWithColor color str = putStr (strWithColor color str)

putStrLnWithColor :: TextColor -> String -> IO ()
putStrLnWithColor color str = putStrWithColor color (str ++ "\n")
