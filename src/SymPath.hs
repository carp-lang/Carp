module SymPath
  ( SymPath (..),
    mangle,
    pathToC,
    consPath,
  )
where

import Data.Char (isAscii, ord)
import qualified Data.Map as Map
import Util

-- | The path to a binding
data SymPath = SymPath [String] String deriving (Ord, Eq)

instance Show SymPath where
  show (SymPath modulePath symName) =
    if null modulePath
      then symName
      else joinWithPeriod modulePath ++ "." ++ symName

mangle :: String -> String
mangle = ureplace . sreplace . creplace
  where
    creplace =
      replaceChars
        ( Map.fromList
            [ ('+', "_PLUS_"),
              ('-', "_MINUS_"),
              ('*', "_MUL_"),
              ('/', "_DIV_"),
              ('<', "_LT_"),
              ('>', "_GT_"),
              ('?', "_QMARK_"),
              ('!', "_BANG_"),
              ('=', "_EQ_")
            ]
        )
    sreplace =
      replaceStrings
        ( Map.fromList
            [ ("auto", "_AUTO_"),
              ("break", "_BREAK_"),
              ("case", "_CASE_"),
              ("const", "_CONST_"),
              ("char", "_CHAR_"),
              ("continue", "_CONTINUE_"),
              ("default", "_DEFAULT_"),
              ("do", "_DO_"),
              ("double", "_DOUBLE_"),
              ("else", "_ELSE_"),
              ("enum", "_ENUM_"),
              ("extern", "_EXTERN"),
              ("float", "_FLOAT_"),
              ("for", "_FOR"),
              ("goto", "_GOTO_"),
              ("if", "_IF_"),
              ("int", "_INT_"),
              ("long", "_LONG_"),
              ("register", "_REGISTER_"),
              ("return", "_RETURN_"),
              ("short", "_SHORT_"),
              ("signed", "_SIGNED_"),
              ("sizeof", "_SIZEOF_"),
              ("static", "_STATIC_"),
              ("struct", "_STRUCT_"),
              ("switch", "_SWITCH_"),
              ("typedef", "_TYPEDEF_"),
              ("union", "_UNION_"),
              ("unsigned", "_UNSIGNED_"),
              ("volatile", "_VOLATILE_"),
              ("void", "_VOID_"),
              ("while", "_WHILE_")
            ]
        )
    ureplace = concatMap (\c -> if isAscii c then pure c else "_U" ++ show (ord c) ++ "U_")

pathToC :: SymPath -> String
pathToC (SymPath modulePath name) =
  concatMap ((++ "_") . mangle) modulePath ++ mangle name

-- | Add qualifying strings to beginning of a path.
consPath :: [String] -> SymPath -> SymPath
consPath qualifyers (SymPath stringPaths name) =
  SymPath (qualifyers ++ stringPaths) name
