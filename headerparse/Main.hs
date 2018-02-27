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

          -- do _ <- Parsec.many Parsec.anyChar
          --            return [(XObj (Sym (SymPath [] "foobar") Symbol) Nothing Nothing)]

        line :: Parsec.Parsec String () [XObj]
        line = Parsec.try functionPrototype <|> discarded

          --Parsec.choice [functionPrototype, discarded]

        functionPrototype :: Parsec.Parsec String () [XObj]
        functionPrototype = do Parsec.many spaceOrTab
                               returnTypeString <- Parsec.many1 identifierChar
                               Parsec.many spaceOrTab
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
                                                 , toTypeXObj argTypeStrings returnTypeString
                                                 ]) Nothing Nothing]

        arg :: Parsec.Parsec String () String
        arg = do Parsec.many spaceOrTab
                 argTypeAsString <- Parsec.many1 identifierChar
                 Parsec.many1 spaceOrTab
                 _ <- Parsec.many1 identifierChar
                 Parsec.many spaceOrTab
                 return argTypeAsString

        identifierChar :: Parsec.Parsec String () Char
        identifierChar = Parsec.choice [Parsec.letter, Parsec.digit, Parsec.char '_']

        spaceOrTab :: Parsec.Parsec String () Char
        spaceOrTab = Parsec.choice [Parsec.char ' ', Parsec.char '\t']

        discarded :: Parsec.Parsec String () [XObj]
        discarded = do discardedLine <- Parsec.many (Parsec.noneOf "\n")
                       return []
                       --return [(XObj (Str ("DISCARDED: " ++ discardedLine)) Nothing Nothing)]

toTypeXObj :: [String] -> String -> XObj
toTypeXObj argTypeStrings returnTypeString =
  (XObj (Lst [ (XObj (Sym (SymPath [] "λ") Symbol) Nothing Nothing)
             , (XObj (Arr (map (tyToXObj . cTypeToCarpType) argTypeStrings)) Nothing Nothing)
             , (XObj (Sym (SymPath [] (show (cTypeToCarpType returnTypeString))) Symbol) Nothing Nothing)
             ]) Nothing Nothing)

cTypeToCarpType :: String -> Ty
cTypeToCarpType "char" = CharTy
cTypeToCarpType "int" = IntTy
cTypeToCarpType "bool" = BoolTy
cTypeToCarpType "long" = LongTy
cTypeToCarpType "double" = DoubleTy
cTypeToCarpType "float" = FloatTy
cTypeToCarpType "void" = UnitTy
cTypeToCarpType s = (StructTy s [])
