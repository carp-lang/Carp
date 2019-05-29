{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Console.CmdArgs
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
               Parsec.try define <|>
               discarded

        define :: Parsec.Parsec String () [XObj]
        define = do Parsec.many spaceOrTab
                    Parsec.string "#define"
                    Parsec.many spaceOrTab
                    name <- Parsec.many1 identifierChar
                    argList <- Parsec.optionMaybe argList
                    Parsec.many spaceOrTab
                    _ <- defineBody
                    Parsec.many spaceOrTab
                    -- OBS! Never kebab
                    case argList of
                      Nothing ->
                        let tyXObj =
                              XObj (Sym (SymPath [] "a") Symbol) Nothing Nothing
                        in return (createRegisterForm name tyXObj prefix False)
                      Just args ->
                        let argsTy = genTypes (length args)
                            tyXObj = toFnTypeXObj argsTy ("a", 0)
                        in return (createRegisterForm name tyXObj prefix False)
            where argList = do
                    _ <- Parsec.char '('
                    args <- Parsec.sepBy
                              (Parsec.many spaceOrTab >>
                               Parsec.many1 identifierChar)
                              (Parsec.char ',')
                    _ <- Parsec.char ')'
                    return args
                  genTypes 0 = []
                  genTypes n = (("a" ++ show n), 0) : genTypes (n - 1)

        defineBody :: Parsec.Parsec String () ()
        defineBody = do s <- Parsec.many (Parsec.noneOf "\\\n")
                        ending <- Parsec.optionMaybe (Parsec.string "\\\\\n")
                        case ending of
                          Nothing ->
                            do c <- Parsec.optionMaybe (Parsec.noneOf "\n")
                               case c of
                                 Just _  -> defineBody
                                 Nothing -> return ()
                          Just _ -> defineBody

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
                               argTypeStrings <- Parsec.try voidArg <|>
                                                 argList
                               Parsec.many spaceOrTab
                               Parsec.char ';'
                               Parsec.many (Parsec.noneOf "\n")
                               let tyXObj = toFnTypeXObj argTypeStrings (returnTypeString, length stars1 + length stars2)
                               return (createRegisterForm name tyXObj prefix kebab)

        voidArg :: Parsec.Parsec String () [(String, Int)]
        voidArg = do _ <- Parsec.string "(void)"
                     return []

        argList :: Parsec.Parsec String () [(String, Int)]
        argList = do Parsec.char '('
                     args <- Parsec.sepBy arg (Parsec.char ',')
                     Parsec.char ')'
                     return args

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

createRegisterForm :: String -> XObj -> String -> Bool -> [XObj]
createRegisterForm name tyXObj prefix kebab =
  let carpName = (if kebab then (toKebab . lowerFirst) else id)
                   (if prefix == "" then name else removePrefix prefix name)
      emitName = name
  in [XObj (Lst ([ (XObj (Sym (SymPath [] "register") Symbol) Nothing Nothing)
                 , (XObj (Sym (SymPath [] carpName) Symbol) Nothing Nothing)
                 , tyXObj
                 ] ++
                  if prefix == ""
                  then []
                  else [(XObj (Str emitName) Nothing Nothing)]
                )) Nothing Nothing]

toFnTypeXObj :: [(String, Int)] -> (String, Int) -> XObj
toFnTypeXObj argTypeStrings returnTypeString =
  (XObj (Lst [ (XObj (Sym (SymPath [] "λ") Symbol) Nothing Nothing)
             , (XObj (Arr (map (tyToXObj . cTypeToCarpType) argTypeStrings)) Nothing Nothing)
             , (XObj (Sym (SymPath [] (show (cTypeToCarpType returnTypeString))) Symbol) Nothing Nothing)
             ]) Nothing Nothing)

toTypeXObj :: (String, Int) -> XObj
toTypeXObj typeString =
  (XObj (Sym (SymPath [] (show (cTypeToCarpType typeString))) Symbol) Nothing Nothing)

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
