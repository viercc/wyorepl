  Finally, we reached the main topic: how to implement REPL.
The first thing we do is defining a syntax of REPL commands.
Here let me mimic the GHCi command.

> module ReplSyntax(
>   ReplCommand(..),
>   parseReplCommand,
>   helpMessage
> ) where
> 
> import Language
> 
> import Text.Megaparsec
> import Parser

 Our REPL command is either:

    <Statement>    A <Statement> to execute
    <Expression>   An <Expression> to evaluate and print it
    :load "File"   A command to load a file and run it
    :help          A command to print help message
    :quit          A command to exit the REPL

> data ReplCommand =
>     Run Statement
>   | EvalPrint Expression
>   | Load FilePath
>   | Help
>   | Quit
>   deriving (Show, Read)

 And a way to parse a line user typed to `ReplCommand` data.

> replCommandP :: Parser ReplCommand
> replCommandP =
>       Run <$> try statementP
>   <|> EvalPrint <$> try expressionP
>   <|> Load <$ symbol ":load" <*> stringLiteral
>   <|> Help <$ symbol ":help"
>   <|> Quit <$ symbol ":quit"
> 
> parseReplCommand :: String -> Either String ReplCommand
> parseReplCommand input =
>   case parse (space *> replCommandP <* eof) "UserInput" input of
>     Left err -> Left (parseErrorPretty err)
>     Right cmd -> Right cmd

> helpMessage :: String
> helpMessage = unlines
>   [ "REPL command syntax:"
>   , "    <VarName> = <Expression>"
>   , "    PRINT <Expression>"
>   , "    <Expression>"
>   , "    :load <FilePath>"
>   , "    :help"
>   , "    :quit"
>   ]
> 

 That's all! Then go to Repl.lhs to finally see the implementation of REPL.
