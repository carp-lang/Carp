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
    Right ok -> ok
  where
        cSyntax :: Parsec.Parsec String () [XObj]
        cSyntax = do _ <- Parsec.many Parsec.anyChar
                     return [(XObj (Sym (SymPath [] "foobar") Symbol) Nothing Nothing)]
