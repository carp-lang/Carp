
module StringUtils (unescapeString) where

import Debug.Trace
import Data.Either
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import Util


unescapeString :: String -> String
unescapeString s =
  case Parsec.parse unescapeStringParser "" s of
    Left err -> trace (show err) "<ERROR>"
    Right v -> v


unescapeStringParser :: Parsec.Parsec String ps String
unescapeStringParser =
    Parsec.many (Parsec.try escaped <|> simple)
  where
    escaped = do
      _ <- Parsec.char '\\'
      c <- Parsec.oneOf ['\\', 'n']
      case c of
        '\\' -> return '\\'
        'n' -> return '\n'

    simple = Parsec.anyChar
