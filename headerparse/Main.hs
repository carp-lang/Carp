{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Console.CmdArgs
import System.IO (readFile, hGetContents)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import Data.Char (toLower, isUpper)
import Util
import Types
import Obj

data Args = Args { sourcePath :: String
                 , prefixToRemove :: String
                 , kebabCase :: Bool
                 } deriving (Show, Data, Typeable)

main = do parsedArgs <- cmdArgs (Args { sourcePath = def &= argPos 0
                                      , prefixToRemove = def
                                      , kebabCase = False
                                      }
                                 &= summary "Carp Header Parse 0.0.1")
          let path = sourcePath parsedArgs
          if path /= ""
            then do source <- readFile path
                    putStrLn (joinWith "\n" (map pretty (parseHeaderFile path source
                                                         (prefixToRemove parsedArgs)
                                                         (kebabCase parsedArgs))))
            else print parsedArgs

parseHeaderFile :: FilePath -> String -> String -> Bool -> [XObj]
parseHeaderFile path src prefix kebab =
  case Parsec.runParser cSyntax () path src of
    Left err -> error (show err)
    Right ok -> concat ok
  where
        cSyntax :: Parsec.Parsec String () [[XObj]]
        cSyntax = Parsec.sepBy line (Parsec.char '\n')

        line :: Parsec.Parsec String () [XObj]
        line = Parsec.try prefixedFunctionPrototype <|>
               Parsec.try functionPrototype <|>
               discarded

        prefixedFunctionPrototype :: Parsec.Parsec String () [XObj]
        prefixedFunctionPrototype = do Parsec.many spaceOrTab
                                       _ <- Parsec.many1 identifierChar
                                       functionPrototype

        functionPrototype :: Parsec.Parsec String () [XObj]
        functionPrototype = do Parsec.many spaceOrTab
                               returnTypeString <- Parsec.many1 identifierChar
                               stars1 <- stars
                               Parsec.many1 spaceOrTab
                               stars2 <- stars
                               name <- Parsec.many1 identifierChar
                               Parsec.many spaceOrTab
                               Parsec.char '('
                               argTypeStrings <- Parsec.sepBy arg (Parsec.char ',')
                               Parsec.char ')'
                               Parsec.many spaceOrTab
                               Parsec.char ';'
                               Parsec.many spaceOrTab
                               let carpName =
                                     (if kebab then (toKebab . lowerFirst) else id)
                                       (if prefix == "" then name else removePrefix prefix name)
                                   emitName = name
                               return [XObj (Lst ([ (XObj (Sym (SymPath [] "register") Symbol) Nothing Nothing)
                                                  , (XObj (Sym (SymPath [] carpName) Symbol) Nothing Nothing)
                                                  , toTypeXObj argTypeStrings (returnTypeString, length stars1 + length stars2)
                                                  ] ++
                                                  if prefix == ""
                                                  then []
                                                  else [(XObj (Str emitName) Nothing Nothing)]
                                                 )) Nothing Nothing]

        arg :: Parsec.Parsec String () (String, Int)
        arg = do Parsec.many spaceOrTab
                 _ <- Parsec.option "" $ do Parsec.string "const"
                                            Parsec.many spaceOrTab
                 argTypeAsString <- Parsec.many1 identifierChar
                 stars1 <- stars
                 Parsec.many1 spaceOrTab
                 stars2 <- stars
                 _ <- Parsec.many1 identifierChar
                 Parsec.many spaceOrTab
                 return (argTypeAsString, length stars1 + length stars2)

        stars :: Parsec.Parsec String () String
        stars = Parsec.many (Parsec.char '*')

        spaceOrTab :: Parsec.Parsec String () Char
        spaceOrTab = Parsec.choice [Parsec.char ' ', Parsec.char '\t']

        discarded :: Parsec.Parsec String () [XObj]
        discarded = do discardedLine <- Parsec.many (Parsec.noneOf "\n")
                       return []
                       --return [(XObj (Str ("DISCARDED: " ++ discardedLine)) Nothing Nothing)]

toTypeXObj :: [(String, Int)] -> (String, Int) -> XObj
toTypeXObj argTypeStrings returnTypeString =
  (XObj (Lst [ (XObj (Sym (SymPath [] "λ") Symbol) Nothing Nothing)
             , (XObj (Arr (map (tyToXObj . cTypeToCarpType) argTypeStrings)) Nothing Nothing)
             , (XObj (Sym (SymPath [] (show (cTypeToCarpType returnTypeString))) Symbol) Nothing Nothing)
             ]) Nothing Nothing)

cTypeToCarpType :: (String, Int) -> Ty
cTypeToCarpType ("char", 0) = CharTy
cTypeToCarpType ("int", 0) = IntTy
cTypeToCarpType ("bool", 0) = BoolTy
cTypeToCarpType ("long", 0) = LongTy
cTypeToCarpType ("double", 0) = DoubleTy
cTypeToCarpType ("float", 0) = FloatTy
cTypeToCarpType ("void", 0) = UnitTy
cTypeToCarpType (s, 0) = (StructTy s [])
cTypeToCarpType (x, stars) = (PointerTy (cTypeToCarpType (x, stars - 1)))

identifierChar :: Parsec.Parsec String () Char
identifierChar = Parsec.choice [Parsec.letter, Parsec.digit, Parsec.char '_']

removePrefix :: String -> String -> String
removePrefix prefix s =
  case Parsec.runParser match () "" s of
    Left err -> s
    Right ok -> ok
  where match =
          do _ <- Parsec.string prefix
             Parsec.many1 identifierChar

lowerFirst :: String -> String
lowerFirst (c : cs) = toLower c : cs

toKebab :: String -> String
toKebab [] = []
toKebab (c : cs) = (if isUpper c then ['-', toLower c] else [c]) ++ toKebab cs
