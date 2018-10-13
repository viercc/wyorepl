 This module implements various parsers needed to parse our language from
text. You can skip to read this module if you know how parsing works well.

> module Parser where

 Let's define a parser for our language. At very first, we need to import
the necessary modules.

> import           Control.Applicative        (empty)
> 
> import           Data.Char                  (isLower)
> import           Data.Void                  (Void)
> import           Text.Megaparsec
> 
> import qualified Text.Megaparsec.Char       as C
> import qualified Text.Megaparsec.Char.Lexer as L
> 
> import           Control.Exception
> import           System.IO

This module provides a parser for our Expression, Statement, and Program type,
so we import it.

> import           Language

 Preparing necessary building blocks. The type alias `Parser` is the type of
a parser (heh) and `Parser SomeType` means it's a parser which 

> -- Prepare building blocks
> 
> type Parser = Parsec Void String
> 
> space :: Parser ()
> space = L.space C.space1 (L.skipLineComment "#") empty
> 
> lexeme :: Parser a -> Parser a
> lexeme = L.lexeme space
> 
> symbol :: String -> Parser String
> symbol = L.symbol space
> 
> integer :: Parser Integer
> integer = lexeme $ L.signed (return ()) L.decimal
> 
> stringLiteral :: Parser String
> stringLiteral = lexeme $ C.char '"' *> manyTill L.charLiteral (C.char '"')
> 
> parens :: Parser a -> Parser a
> parens = between (symbol "(") (symbol ")")

Then define the syntax of our language. I don't explain how we're using
Megaparsec (parser combinator library) in detail.

> -- Expression, Statement, Program
> 
> varNameP :: Parser VarName
> varNameP = lexeme $ takeWhile1P (Just "VarName") isVarNameChar
>   where
>     isVarNameChar c = isLower c || c == '_'
> 
> literalP :: Parser Int
> literalP = fromInteger <$> integer
> 
> expressionP :: Parser Expression
> expressionP = addP
>   where
>     termP = parens addP
>       <|> Lit <$> literalP
>       <|> Var <$> varNameP
>     addP = foldr1 Add <$> sepBy1 termP (symbol "+")
> 
> statementP :: Parser Statement
> statementP =
>       Assign <$> varNameP <* symbol "=" <*> expressionP
>   <|> Print <$ symbol "PRINT" <*> expressionP
> 
> programP :: Parser Program
> programP = sepEndBy1 statementP (symbol ";")

We provide the way to apply a parser on a file here.

> -- Running parser
> parseFile :: Parser a -> FilePath -> IO (Either String a)
> parseFile p filePath =
>   handle (return . Left . showIOException) $
>     do src <- readFile filePath
>        let result = case parse (space *> p <* eof) filePath src of
>                       Left e  -> Left $ parseErrorPretty e
>                       Right a -> Right a
>        evaluate result
>        return result
>   where
>     showIOException :: IOException -> String
>     showIOException = show
