{-# LANGUAGE FlexibleContexts #-}

module Parsing (parse, validCharacters, balance) where

import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
-- import Text.Parsec.Error (newErrorMessage, Message(..))
-- import Text.Parsec.Pos (newPos)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Obj
import Types
import Util
import Control.Monad.Error.Class (throwError)

import Debug.Trace

newtype ParseState = ParseState { parseInfo :: Info }

createInfo :: Parsec.Parsec String ParseState (Maybe Info)
createInfo = do i <- fmap parseInfo Parsec.getState
                return (Just i)

maybeSigned :: Parsec.Parsec String ParseState (Maybe Info, String)
maybeSigned = do i <- createInfo
                 sign <- Parsec.optionMaybe (Parsec.char '-')
                 digits <- Parsec.many1 Parsec.digit
                 let num = maybe "" (\x -> [x]) sign ++ digits
                 incColumn (length num)
                 return (i, num)

double :: Parsec.Parsec String ParseState XObj
double = do (i, num) <- maybeSigned
            _ <- Parsec.char '.'
            incColumn 1
            decimals <- Parsec.many1 Parsec.digit
            incColumn (length decimals)
            return (XObj (Num DoubleTy (read (num ++ "." ++ decimals))) i Nothing)

float :: Parsec.Parsec String ParseState XObj
float = do (i, num) <- maybeSigned
           _ <- Parsec.char '.'
           incColumn 1
           decimals <- Parsec.many1 Parsec.digit
           incColumn (length decimals)
           _ <- Parsec.char 'f'
           incColumn 1
           return (XObj (Num FloatTy (read (num ++ "." ++ decimals))) i Nothing)

floatNoPeriod :: Parsec.Parsec String ParseState XObj
floatNoPeriod =
  do (i, num) <- maybeSigned
     _ <- Parsec.char 'f'
     incColumn 1
     return (XObj (Num FloatTy (read num)) i Nothing)

integer :: Parsec.Parsec String ParseState XObj
integer = do (i, num) <- maybeSigned
             return (XObj (Num IntTy (read num)) i Nothing)

long :: Parsec.Parsec String ParseState XObj
long = do (i, num) <- maybeSigned
          _ <- Parsec.char 'l'
          incColumn 1
          return (XObj (Num LongTy (read num)) i Nothing)

number :: Parsec.Parsec String ParseState XObj
number = Parsec.try float <|>
         Parsec.try floatNoPeriod <|>
         Parsec.try double <|>
         Parsec.try long <|>
         Parsec.try integer

string :: Parsec.Parsec String ParseState XObj
string = do i <- createInfo
            _ <- Parsec.char '"'
            strL <- Parsec.many (Parsec.try escaped <|> simple)
            let str = concat strL
            _ <- Parsec.char '"'
            incColumn (length str + 2)
            incLine (countLinebreaks str)
            return (XObj (Str str) i Nothing)
  where simple = do c <- Parsec.noneOf ['"']
                    return [c]
        countLinebreaks = foldr (\x sum -> if x == '\n' then sum+1 else sum) 0

parseInternalPattern :: Parsec.Parsec String ParseState String
parseInternalPattern = do maybeAnchor <- Parsec.optionMaybe (Parsec.char '^')
                          str <- Parsec.many (Parsec.try patternEscaped <|>
                                              Parsec.try bracketClass <|>
                                              Parsec.try capture <|>
                                              simple)
                          maybeEnd <- Parsec.optionMaybe (Parsec.char '$')
                          return $ unwrapMaybe maybeAnchor ++ concat str ++
                                   unwrapMaybe maybeEnd
    where unwrapMaybe (Just c) = [c]
          unwrapMaybe (Nothing) = []
          simple :: Parsec.Parsec String ParseState String
          simple = do char <- Parsec.noneOf "^$()[]\\\""
                      return [char]
          patternEscaped :: Parsec.Parsec String ParseState String
          patternEscaped = do
            _ <- Parsec.char '\\'
            c <- Parsec.oneOf ['1', '2', '3', '4', '5', '6', '7', '8', '9',
                               'a', 'c', 'd', 'g', 'l', 'p', 's', 'u', 'w',
                               'x', 'n', 't', 'b', 'f', '[', ']', '\\', '$',
                               '(', ')', '^', '"', '*', '.']
            case c of
              'b' -> do c1 <- Parsec.noneOf ['"']
                        c2 <- Parsec.noneOf ['"']
                        return ['\\', c, c1, c2]
              'f' -> do str <- bracketClass
                        return $ '\\' : c : str
              _   -> return ['\\', c]
          capture :: Parsec.Parsec String ParseState String
          capture = do
            opening <- Parsec.char '('
            str <- Parsec.many (Parsec.try patternEscaped <|>
                                Parsec.try bracketClass <|>
                                simple)
            closing <- Parsec.char ')'
            return $ "(" ++ concat str ++ ")"
          range :: Parsec.Parsec String ParseState String
          range = do
            begin <- Parsec.alphaNum
            _ <- Parsec.char '-'
            end <- Parsec.alphaNum
            return [begin, '-', end]
          bracketClass :: Parsec.Parsec String ParseState String
          bracketClass = do
            opening <- Parsec.char '['
            maybeAnchor <- Parsec.optionMaybe (Parsec.char '^')
            str <- Parsec.many (Parsec.try range <|>
                                Parsec.try patternEscaped <|>
                                Parsec.many1 (Parsec.noneOf "-^$()[]\\\""))
            closing <- Parsec.char ']'
            return $ "[" ++ unwrapMaybe maybeAnchor ++ concat str ++ "]"


pattern :: Parsec.Parsec String ParseState XObj
pattern = do i <- createInfo
             _ <- Parsec.char '#'
             _ <- Parsec.char '"'
             str <- parseInternalPattern
             _ <- Parsec.char '"'
             incColumn (length str + 2)
             return (XObj (Pattern $ treat str) i Nothing)
  -- auto-escaping backslashes
  where treat :: String -> String
        treat [] = []
        treat ('\\':r) = "\\\\" ++ treat r
        treat (x:r) = x : treat r

escaped :: Parsec.Parsec String ParseState [Char]
escaped =  do
    _ <- Parsec.char '\\'
    c <- Parsec.oneOf ['\\', '\"']
    case c of
      '\\' -> return "\\\\"
      '\"' -> return "\""

escapedQuoteChar :: Parsec.Parsec String ParseState Char
escapedQuoteChar = do c <- Parsec.string "\""
                      incColumn 2
                      return '\"'

escapedSpaceChar :: Parsec.Parsec String ParseState Char
escapedSpaceChar = do c <- Parsec.string "space"
                      incColumn 5
                      return ' '

escapedNewlineChar :: Parsec.Parsec String ParseState Char
escapedNewlineChar = do c <- Parsec.string "newline"
                        incColumn 7
                        return '\n'

escapedTabChar :: Parsec.Parsec String ParseState Char
escapedTabChar = do c <- Parsec.string "tab"
                    incColumn 3
                    return '\t'

aChar :: Parsec.Parsec String ParseState XObj
aChar = do i <- createInfo
           _ <- Parsec.char '\\'
           c <- Parsec.try escapedQuoteChar <|>
                Parsec.try escapedNewlineChar <|>
                Parsec.try escapedTabChar <|>
                Parsec.try escapedSpaceChar <|>
                Parsec.anyChar
           incColumn 2
           return (XObj (Chr c) i Nothing)

{-# ANN validCharacters "HLint: ignore Use String" #-}
validCharacters :: [Char]
validCharacters = "+-*/?!><=_:\9580\9559"

symbolSegment :: Parsec.Parsec String ParseState String
symbolSegment = do sym <- Parsec.many1 validInSymbol
                   incColumn (length sym)
                   return sym
  where validInSymbol = Parsec.choice [Parsec.letter, Parsec.digit, Parsec.oneOf validCharacters]

period :: Parsec.Parsec String ParseState ()
period = do Parsec.char '.'
            incColumn 1
            return ()

symbol :: Parsec.Parsec String ParseState XObj
symbol = do i <- createInfo
            segments <- Parsec.sepBy1 symbolSegment period
            if length segments > 1
            -- if it’s qualified, it can’t be a special form
            then return (XObj
                          (Sym (SymPath (init segments) (last segments))
                               Symbol)
                          i Nothing)
            else
              case last segments of
                "defn" -> return (XObj Defn i Nothing)
                "def" -> return (XObj Def i Nothing)
                -- TODO: What about the other def- forms?
                "do" -> return (XObj Do i Nothing)
                "while" -> return (XObj While i Nothing)
                "fn" -> return (XObj (Fn Nothing Set.empty) i Nothing)
                "let" -> return (XObj Let i Nothing)
                "break" -> return (XObj Break i Nothing)
                "if" -> return (XObj If i Nothing)
                "match" -> return (XObj Match i Nothing)
                "true" -> return (XObj (Bol True) i Nothing)
                "false" -> return (XObj (Bol False) i Nothing)
                "address" -> return (XObj Address i Nothing)
                "set!" -> return (XObj SetBang i Nothing)
                "the" -> return (XObj The i Nothing)
                "ref" -> return (XObj Ref i Nothing)
                "deref" -> return (XObj Deref i Nothing)
                "with" -> return (XObj With i Nothing)
                name   -> return (XObj (Sym (SymPath (init segments) name) Symbol) i Nothing)

atom :: Parsec.Parsec String ParseState XObj
atom = Parsec.choice [number, pattern, string, aChar, symbol]

incColumn :: Int -> Parsec.Parsec String ParseState ()
incColumn x = do s <- Parsec.getState
                 let i = parseInfo s
                     line = infoLine i
                     column = infoColumn i
                     identifier = infoIdentifier i
                     file = infoFile i
                     newInfo = Info line (column + x) file (Set.fromList []) identifier
                 Parsec.putState (s { parseInfo = newInfo })
                 return ()

incLine :: Int -> Parsec.Parsec String ParseState ()
incLine x = do s <- Parsec.getState
               let i = parseInfo s
                   line = infoLine i
                   column = infoColumn i
                   identifier = infoIdentifier i
                   file = infoFile i
                   newInfo = Info (line + x) column file (Set.fromList []) identifier
               Parsec.putState (s { parseInfo = newInfo })
               return ()

comment :: Parsec.Parsec String ParseState ()
comment = do _ <- Parsec.char ';'
             _ <- Parsec.many (Parsec.noneOf ['\n'])
             return ()

linebreak :: Parsec.Parsec String ParseState ()
linebreak = do s <- Parsec.getState
               let i = parseInfo s
                   line = infoLine i
                   identifier = infoIdentifier i
                   file = infoFile i
                   newInfo = Info (line + 1) 1 file (Set.fromList [])  identifier
               Parsec.putState (s { parseInfo = newInfo })
               _ <- Parsec.char '\n'
               return ()

space :: Parsec.Parsec String ParseState ()
space = do incColumn 1
           _ <- Parsec.char ' '
           return ()

comma :: Parsec.Parsec String ParseState ()
comma = do incColumn 1
           _ <- Parsec.char ','
           return ()

tab :: Parsec.Parsec String ParseState ()
tab = do incColumn 1
         _ <- Parsec.char '\t'
         return ()

eof :: Parsec.Parsec String ParseState ()
eof = do _ <- Parsec.char '\0'
         return ()

emptyCharacters :: [Parsec.Parsec String ParseState ()]
emptyCharacters = [space, tab, comma, linebreak, eof, comment]

whitespace :: Parsec.Parsec String ParseState ()
whitespace = do _ <- Parsec.many1 (Parsec.choice emptyCharacters)
                return ()

whitespaceOrNothing :: Parsec.Parsec String ParseState ()
whitespaceOrNothing = do _ <- Parsec.many (Parsec.choice emptyCharacters)
                         return ()

readObjs :: Parsec.Parsec String ParseState [XObj]
readObjs = do padding <- Parsec.many whitespace
              incColumn (length padding)
              Parsec.many sexpr

array :: Parsec.Parsec String ParseState XObj
array = do i <- createInfo
           _ <- Parsec.char '['
           incColumn 1
           objs <- readObjs
           _ <- Parsec.char ']'
           incColumn 1
           return (XObj (Arr objs) i Nothing)

list :: Parsec.Parsec String ParseState XObj
list = do i <- createInfo
          _ <- Parsec.char '('
          incColumn 1
          objs <- readObjs
          _ <- Parsec.char ')'
          incColumn 1
          return (XObj (Lst objs) i Nothing)

dictionary :: Parsec.Parsec String ParseState XObj
dictionary = do i <- createInfo
                _ <- Parsec.char '{'
                incColumn 1
                objs <- readObjs
                _ <- Parsec.char '}'
                incColumn 1
                let objs' = if even (length objs) then objs else init objs -- Drop last if uneven nr of forms.
                -- TODO! Signal error here!
                --return (XObj (Dict (Map.fromList (pairwise objs'))) i Nothing)
                    pairInit = XObj (Sym (SymPath ["Pair"] "init") (LookupGlobal CarpLand AFunction)) i Nothing
                    pairs = map (\(k,v) -> XObj (Lst [pairInit, k, v]) i Nothing) (pairwise objs')
                    arrayLiteral = XObj (Arr pairs) i Nothing
                    reffedArrayLiteral = XObj (Lst [(XObj Ref i Nothing), arrayLiteral]) i Nothing
                    fromArraySymbol = XObj (Sym (SymPath ["Map"] "from-array") (LookupGlobal CarpLand AFunction)) i Nothing
                    fromArraySexp = XObj (Lst [fromArraySymbol, reffedArrayLiteral]) i Nothing
                return fromArraySexp


ref :: Parsec.Parsec String ParseState XObj
ref = do i <- createInfo
         _ <- Parsec.char '&'
         incColumn 1
         expr <- sexpr
         return (XObj (Lst [XObj Ref Nothing Nothing, expr]) i Nothing)

deref :: Parsec.Parsec String ParseState XObj
deref = do i <- createInfo
           _ <- Parsec.char '~'
           incColumn 1
           expr <- sexpr
           return (XObj (Lst [XObj Deref Nothing Nothing, expr]) i Nothing)

copy :: Parsec.Parsec String ParseState XObj
copy = do i1 <- createInfo
          i2 <- createInfo
          _ <- Parsec.char '@'
          incColumn 1
          expr <- sexpr
          return (XObj (Lst [XObj (Sym (SymPath [] "copy") Symbol) i1 Nothing, expr]) i2 Nothing)

quote :: Parsec.Parsec String ParseState XObj
quote = do i1 <- createInfo
           i2 <- createInfo
           _ <- Parsec.char '\''
           incColumn 1
           expr <- sexpr
           return (XObj (Lst [XObj (Sym (SymPath [] "quote") Symbol) i1 Nothing, expr]) i2 Nothing)

sexpr :: Parsec.Parsec String ParseState XObj
sexpr = do x <- Parsec.choice [ref, deref, copy, quote, list, array, dictionary, atom]
           _ <- whitespaceOrNothing
           return x

lispSyntax :: Parsec.Parsec String ParseState [XObj]
lispSyntax = do padding <- Parsec.many whitespace
                incColumn (length padding)
                result <- Parsec.sepBy sexpr whitespaceOrNothing
                Parsec.eof
                return result

parse :: String -> String -> Either Parsec.ParseError [XObj]
parse text fileName = let initState = ParseState (Info 1 1 fileName (Set.fromList []) 0)
                      in  Parsec.runParser lispSyntax initState fileName text



{-# ANN balance "HLint: ignore Use String" #-}
-- | For detecting the parenthesis balance in a string, i.e. "((( ))" = 1
balance :: String -> Int
balance text =
  case Parsec.runParser parenSyntax [] "(parens)" text of
    Left err -> error (show err)
    Right ok -> ok

  where
        parenSyntax :: Parsec.Parsec String [Char] Int
        parenSyntax = do _ <- Parsec.many character
                         parens <- Parsec.getState
                         return (length parens)

        character :: Parsec.Parsec String [Char] ()
        character = do c <- Parsec.anyChar
                       parens <- Parsec.getState
                       case parens of
                         [] -> push c
                         '"':xs -> case c of
                                     '\\' -> do c <- Parsec.anyChar -- consume next
                                                return ()
                                     '"' -> Parsec.putState xs -- close string
                                     _ -> return () -- inside string
                         (x:xs) -> case (x, c) of
                                     ('(', ')') -> Parsec.putState xs
                                     ('[', ']') -> Parsec.putState xs
                                     ('"', '"') -> Parsec.putState xs
                                     --('\\', _) -> Parsec.putState xs -- ignore char after '\'
                                     _ -> push c

        push :: Char -> Parsec.Parsec String String ()
        push c =
          do parens <- Parsec.getState
             case c of
               '(' -> Parsec.putState (c : parens)
               '[' -> Parsec.putState (c : parens)
               '"' -> Parsec.putState (c : parens)
               _ -> return ()
