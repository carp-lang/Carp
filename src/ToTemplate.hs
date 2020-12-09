module ToTemplate where

import Obj
import Parsing
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import Util

-- | High-level helper function for creating templates from strings of C code.
toTemplate :: String -> [Token]
toTemplate text = case Parsec.runParser templateSyntax 0 "(template)" text of
  Right ok -> ok
  Left err -> error (show err)
  where
    templateSyntax :: Parsec.Parsec String Int [Token]
    templateSyntax = Parsec.many parseTok
    parseTok =
      Parsec.try parseTokDecl
        <|> Parsec.try parseTokName --- $DECL
        <|> Parsec.try parseTokTyGrouped --- $NAME
        <|> Parsec.try parseTokTyRawGrouped --- i.e. $(Fn [Int] t)
        <|> Parsec.try parseTokTy
        <|> parseTokC --- i.e. $t
        --- Anything else...
    parseTokDecl :: Parsec.Parsec String Int Token
    parseTokDecl = do
      _ <- Parsec.string "$DECL"
      pure TokDecl
    parseTokName :: Parsec.Parsec String Int Token
    parseTokName = do
      _ <- Parsec.string "$NAME"
      pure TokName
    parseTokC :: Parsec.Parsec String Int Token
    parseTokC = do
      s <- Parsec.many1 validInSymbol
      pure (TokC s)
      where
        validInSymbol = Parsec.choice [Parsec.letter, Parsec.digit, Parsec.oneOf validCharactersInTemplate]
        validCharactersInTemplate = " ><{}()[]|;:.,_-+*#/'^!?€%&=@\"\n\t\\"
    parseTokTy :: Parsec.Parsec String Int Token
    parseTokTy = do
      _ <- Parsec.char '$'
      s <- Parsec.many1 Parsec.letter
      pure (toTokTy Normal s)
    parseTokTyGrouped :: Parsec.Parsec String Int Token
    parseTokTyGrouped = do
      _ <- Parsec.char '$'
      toTokTy Normal <$> parseGrouping
    parseTokTyRawGrouped :: Parsec.Parsec String Int Token
    parseTokTyRawGrouped = do
      _ <- Parsec.char '§'
      toTokTy Raw <$> parseGrouping
    parseGrouping :: Parsec.Parsec String Int String
    parseGrouping = do
      _ <- Parsec.char '('
      Parsec.putState 1 -- One paren to close.
      fmap ('(' :) (Parsec.many parseCharBalanced)
    -- Note: The closing paren is read by parseCharBalanced.

    parseCharBalanced :: Parsec.Parsec String Int Char
    parseCharBalanced = do
      balanceState <- Parsec.getState
      if balanceState > 0
        then
          Parsec.try openParen
            <|> Parsec.try closeParen
            <|> Parsec.anyChar
        else Parsec.char '\0' -- Should always fail which will end the string.
    openParen :: Parsec.Parsec String Int Char
    openParen = do
      _ <- Parsec.char '('
      Parsec.modifyState (+ 1)
      pure '('
    closeParen :: Parsec.Parsec String Int Char
    closeParen = do
      _ <- Parsec.char ')'
      Parsec.modifyState (\x -> x - 1)
      pure ')'

-- | Converts a string containing a type to a template token ('TokTy').
-- | i.e. the string "(Array Int)" becomes (TokTy (StructTy "Array" IntTy)).
toTokTy :: TokTyMode -> String -> Token
toTokTy mode s =
  case parse s "" of
    Left err -> error (show err)
    Right [] -> error ("toTokTy got [] when parsing: '" ++ s ++ "'")
    Right [xobj] -> case xobjToTy xobj of
      Just ok -> TokTy ok mode
      Nothing -> error ("toTokTy failed to convert this s-expression to a type: " ++ pretty xobj)
    Right xobjs -> error ("toTokTy parsed too many s-expressions: " ++ joinWithSpace (map pretty xobjs))

templateLiteral :: String -> (a -> [Token])
templateLiteral = const . toTemplate

multilineTemplate :: [String] -> [Token]
multilineTemplate = toTemplate . unlines

templateReturn :: String -> [Token]
templateReturn x =
  multilineTemplate
    [ "$DECL { ",
      "    return " ++ x ++ ";",
      "}"
    ]
