{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Console.CmdArgs
import System.IO (readFile)
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import Util
import Types
import Obj

data Args = Args { sourcePath :: String
                 } deriving (Show, Data, Typeable)

main = do parsedArgs <- cmdArgs (Args { sourcePath = def &= argPos 0 }
                                 &= summary "Carp Header Parse 0.0.1")
          let path = sourcePath parsedArgs
          if path /= ""
            then do source <- readFile path
                    putStrLn (joinWith "\n" (map pretty (parseHeaderFile path source)))
            else print parsedArgs

parseHeaderFile :: FilePath -> String -> [XObj]
parseHeaderFile path src =
  case Parsec.runParser cSyntax () path src of
    Left err -> error (show err)
    Right ok -> concat ok
  where
        cSyntax :: Parsec.Parsec String () [[XObj]]
        cSyntax = Parsec.sepBy line (Parsec.char '\n')

        line :: Parsec.Parsec String () [XObj]
        line = Parsec.try functionPrototype <|> discarded

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
                               return [XObj (Lst [ (XObj (Sym (SymPath [] "register") Symbol) Nothing Nothing)
                                                 , (XObj (Sym (SymPath [] name) Symbol) Nothing Nothing)
                                                 , toTypeXObj argTypeStrings (returnTypeString, length stars1 + length stars2)
                                                 ]) Nothing Nothing]

        arg :: Parsec.Parsec String () (String, Int)
        arg = do Parsec.many spaceOrTab
                 argTypeAsString <- Parsec.many1 identifierChar
                 stars1 <- stars
                 Parsec.many1 spaceOrTab
                 stars2 <- stars
                 _ <- Parsec.many1 identifierChar
                 Parsec.many spaceOrTab
                 return (argTypeAsString, length stars1 + length stars2)

        stars :: Parsec.Parsec String () String
        stars = Parsec.many (Parsec.char '*')

        identifierChar :: Parsec.Parsec String () Char
        identifierChar = Parsec.choice [Parsec.letter, Parsec.digit, Parsec.char '_']

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
