module Repl where

import System.Console.Haskeline ( getInputLine
                                , InputT
                                , runInputT
                                , Settings(..)
                                , Completion
                                , simpleCompletion
                                , completeWordWithPrev
                                )
import Data.List (isPrefixOf)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)
import qualified Data.Map as Map

import Types
import Obj
import Util
import ColorText
import Eval
import Path
import Lookup
import Parsing (balance)

completeKeywordsAnd :: Monad m => [String ] -> String -> String -> m [Completion]
completeKeywordsAnd words _ word = return $ findKeywords word (words ++ keywords) []
  where
        findKeywords match [] res = res
        findKeywords match (x : xs) res =
          if match `isPrefixOf` x
            then findKeywords match xs (res ++ [simpleCompletion x])
            else findKeywords match xs res
        keywords = [ "Int" -- we should probably have a list of those somewhere
                   , "Float"
                   , "Double"
                   , "Bool"
                   , "String"
                   , "Char"
                   , "Array"
                   , "Fn"

                   , "def"
                   , "defn"
                   , "let"
                   , "do"
                   , "if"
                   , "while"
                   , "ref"
                   , "address"
                   , "set!"
                   , "the"

                   , "defmacro"
                   , "dynamic"
                   , "quote"
                   , "car"
                   , "cdr"
                   , "cons"
                   , "list"
                   , "array"
                   , "expand"

                   , "deftype"

                   , "register"

                   , "true"
                   , "false"
                   ]


readlineSettings :: Monad m => [String] -> IO (Settings m)
readlineSettings words = do
  historyFile <- configPath "history"
  createDirectoryIfMissing True (takeDirectory historyFile)
  return $ Settings {
    complete = completeWordWithPrev Nothing ['(', ')', '[', ']', ' ', '\t', '\n'] (completeKeywordsAnd words),
    historyFile = Just historyFile,
    autoAddHistory = True
  }

specialCommands :: Map.Map Char String
specialCommands = Map.fromList
  [ ('x', "run")
  , ('r', "reload")
  , ('b', "build")
  , ('c', "cat")
  , ('e', "env")
  , ('h', "help")
  , ('p', "project")
  , ('q', "quit")
  , ('t', "type")
  ]

treatSpecialInput :: String -> String
treatSpecialInput ":" = "(macro-error \"Unfinished special command :\")"
treatSpecialInput (':':rest) =
  let cmdAndArgs = words rest
      cmd = head cmdAndArgs
      args = tail cmdAndArgs
  in if length cmd == 1
     then makeCommand args (head cmd)
     else
       if null args
       then "(do " ++ unwords (map (makeCommand []) cmd) ++ ")"
       else "(macro-error \"Can’t have grouped special command with arguments.\")"
  where makeCommand args cmd =
         case Map.lookup cmd specialCommands of
           Just command -> "(" ++ command ++ " " ++ unwords args ++ ")"
           Nothing -> "(macro-error \"Unknown special command: :" ++ [cmd] ++ "\")"
treatSpecialInput arg = arg

repl :: Context -> String -> InputT IO Context
repl context readSoFar =
  do let prompt = strWithColor Yellow (if null readSoFar then projectPrompt (contextProj context) else "     ")
     input <- getInputLine prompt
     case input of
        Nothing -> do
          liftIO exitSuccess
          return context
        Just i -> do
          let concat = readSoFar ++ i ++ "\n"
          case balance concat of
            0 -> do let input' = if concat == "\n" then contextLastInput context else concat -- Entering an empty string repeats last input
                    context' <- liftIO $ executeString True True (resetAlreadyLoadedFiles context) (treatSpecialInput input') "REPL"
                    return $ context' { contextLastInput = input' }
            _ -> repl context concat

resetAlreadyLoadedFiles context =
  let proj = contextProj context
      proj' = proj { projectAlreadyLoaded = [] }
  in  context { contextProj = proj' }

runRepl context = do
  settings <- readlineSettings (bindingNames $ contextGlobalEnv context)
  context' <- runInputT settings (repl context "")
  runRepl context'
