module Template where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import qualified Data.Set as Set
import Debug.Trace

import Util
import Types
import Obj
import Parsing
import Infer
import Concretize

-- | Templates are instructions for the compiler to generate some C-code
-- | based on some template and the names and types to fill into the template.
-- | Templates are generic and need to be given an explicit type to generate the
-- | correct code.

-- | Example:
-- | template1 : ((Array T) -> Int) = "int length__T(<T> xs) { return xs->len; }"
-- | Given the type ((Array Float) -> Int) the following code is produced:
-- | "int length__Float(Array__Float xs) { return xs->len; }"

-- | Create a binding pair used for adding a template definition to an environment.
defineTemplate :: SymPath -> Ty -> [Token] -> [Token] -> (Ty -> [XObj]) -> (String, Binder)
defineTemplate path t declaration definition depsFunc =
  let (SymPath _ name) = path
      template = Template t (const declaration) (const definition) depsFunc
      i = Info 0 0 (show path ++ ".template") Set.empty 0
      defLst = [XObj (Deftemplate (TemplateCreator (\_ _ -> template))) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]
  in  (name, Binder (XObj (Lst defLst) (Just i) (Just t)))

-- | The more advanced version of a template, where the code can vary depending on the type.
defineTypeParameterizedTemplate :: TemplateCreator -> SymPath -> Ty -> (String, Binder)
defineTypeParameterizedTemplate templateCreator path t =
  let (SymPath _ name) = path
      defLst = [XObj (Deftemplate templateCreator) Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]
  in  (name, Binder (XObj (Lst defLst) Nothing (Just t)))

-- | Create a binding pair used for adding a template instantiation to an environment.
instanceBinder :: SymPath -> Ty -> Template -> (String, Binder)
instanceBinder path@(SymPath _ name) actualType template =
  let (x, _) = instantiateTemplate path actualType template
  in  (name, Binder x)

-- -- | Create a binding pair and don't discard the dependencies
instanceBinderWithDeps :: SymPath -> Ty -> Template -> ((String, Binder), [XObj])
instanceBinderWithDeps path@(SymPath _ name) actualType template =
  let (x, deps) = instantiateTemplate path actualType template
  in  ((name, Binder x), deps)

-- | Concretizes the types used in @token
--   @cName is the name of the definition, i.e. the "foo" in "void foo() { ... }"
concretizeTypesInToken :: TypeMappings -> String -> [Token] -> Token -> [Token]
concretizeTypesInToken mappings cName decl token =
  case token of
    TokDecl -> concatMap (concretizeTypesInToken mappings cName (error "Nope.")) decl
    TokName -> [TokC cName]
    TokTy t -> [TokTy (replaceTyVars mappings t)]
    _ -> [token]

-- | High-level helper function for creating templates from strings of C code.
toTemplate :: String -> [Token]
toTemplate text = case Parsec.runParser templateSyntax 0 "(template)" text of
                    Right ok -> ok
                    Left err -> error (show err)
  where
    templateSyntax :: Parsec.Parsec String Int [Token]
    templateSyntax = Parsec.many parseTok

    parseTok = Parsec.try parseTokDecl <|>      --- $DECL
               Parsec.try parseTokName <|>      --- $NAME
               Parsec.try parseTokTyGrouped <|> --- i.e. $(Fn [Int] t)
               Parsec.try parseTokTy <|>        --- i.e. $t
               parseTokC                        --- Anything else...

    parseTokDecl :: Parsec.Parsec String Int Token
    parseTokDecl = do _ <- Parsec.string "$DECL"
                      return TokDecl

    parseTokName :: Parsec.Parsec String Int Token
    parseTokName = do _ <- Parsec.string "$NAME"
                      return TokName

    parseTokC :: Parsec.Parsec String Int Token
    parseTokC = do s <- Parsec.many1 validInSymbol
                   return (TokC s)
      where validInSymbol = Parsec.choice [Parsec.letter, Parsec.digit, Parsec.oneOf validCharactersInTemplate]
            validCharactersInTemplate = " ><{}()[]|;:.,_-+*#/'^!?€%&=@\"\n\t"

    parseTokTy :: Parsec.Parsec String Int Token
    parseTokTy = do _ <- Parsec.char '$'
                    s <- Parsec.many1 Parsec.letter
                    return (toTokTy s)

    parseTokTyGrouped :: Parsec.Parsec String Int Token
    parseTokTyGrouped = do _ <- Parsec.char '$'
                           _ <- Parsec.char '('
                           Parsec.putState 1 -- One paren to close.
                           s <- fmap ('(' :) (Parsec.many parseCharBalanced)
                           -- Note: The closing paren is read by parseCharBalanced.
                           return (toTokTy s)

    parseCharBalanced :: Parsec.Parsec String Int Char
    parseCharBalanced = do balanceState <- Parsec.getState
                           if balanceState > 0
                             then Parsec.try openParen <|>
                                  Parsec.try closeParen <|>
                                  Parsec.anyChar
                             else Parsec.char '\0' -- Should always fail which will end the string.

    openParen :: Parsec.Parsec String Int Char
    openParen = do _ <- Parsec.char '('
                   Parsec.modifyState (+1)
                   return '('

    closeParen :: Parsec.Parsec String Int Char
    closeParen = do _ <- Parsec.char ')'
                    Parsec.modifyState (\x -> x - 1)
                    return ')'

-- | Converts a string containing a type to a template token ('TokTy').
-- | i.e. the string "(Array Int)" becomes (TokTy (StructTy "Array" IntTy)).
toTokTy :: String -> Token
toTokTy s =
  case parse s "" of
    Left err -> error (show err)
    Right [] -> error ("toTokTy got [] when parsing: '" ++ s ++ "'")
    Right [xobj] -> case xobjToTy xobj of
                      Just ok -> TokTy ok
                      Nothing -> error ("toTokTy failed to convert this s-expression to a type: " ++ pretty xobj)
    Right xobjs -> error ("toTokTy parsed too many s-expressions: " ++ joinWithSpace (map pretty xobjs))

----------------------------------------------------------------------------------------------------------
-- ACTUAL TEMPLATES

-- | This function accepts a pointer and will do nothing with it.
templateNoop :: (String, Binder)
templateNoop = defineTemplate
  (SymPath [] "noop")
  (FuncTy [PointerTy (VarTy "a")] UnitTy)
  (toTemplate "void $NAME ($a* a)")
  (toTemplate "$DECL { }")
  (const [])
