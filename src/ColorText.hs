module ColorText where

import System.Console.ANSI hiding (Blue, Green, Red, White, Yellow)
import System.Exit (exitFailure)
import System.IO
import Util

data TextColor = Blue | Red | Yellow | Green | White

strWithColor :: TextColor -> String -> String
strWithColor color str =
  case platform of
    Windows -> str
    _ -> "\x1b[" ++ col ++ "m" ++ str ++ "\x1b[0m"
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
    putStr $ if istty && platform /= Windows then strWithColor color str else str

putStrLnWithColor :: TextColor -> String -> IO ()
putStrLnWithColor color str = putStrWithColor color (str ++ "\n")

labelStr :: String -> String -> String
labelStr label str = "[" ++ label ++ "] " ++ str

emitWarning :: String -> IO ()
emitWarning str = putStrLnWithColor Blue (labelStr "WARNING" str)

emitErrorWithLabel :: String -> String -> IO ()
emitErrorWithLabel label str = putStrLnWithColor Red (labelStr label str)

emitError :: String -> IO ()
emitError = emitErrorWithLabel "ERROR"

emitErrorBare :: String -> IO ()
emitErrorBare = putStrLnWithColor Red

emitErrorAndExit :: String -> IO a
emitErrorAndExit str = do
  _ <- emitError str
  exitFailure
