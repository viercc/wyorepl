 This module is the Main module, meaning it handles command line arguments,
invoking interpreter, etc. Luckily our programming language is tiny, so there's
not much of things to configure through command line.

> module Main(main) where
> 
> import System.Environment (getArgs)
> 
> import Control.Monad.Except
> 
> import Language
> import Parser
> import Repl

 Options given from command line arguments. There's only two: `isInteractive`
is a flag to specify whether we want to invoke REPL or interpret a file, and
`sourceFile` is a file of our language given to this program.

> data Option = Option
>  { isInteractive :: Bool
>  , sourceFile :: Maybe String
>  }
>  deriving (Show)
> 
> main :: IO ()
> main = 
>   do optionResult <- parseArgs
>      case optionResult of
>        Nothing -> printUsage
>        Just option ->
>          if isInteractive option
>            then interactive (sourceFile option)
>            else interpret (sourceFile option)

 Here we parse arguments manually, but "optparse-applicative" is go-to package
if you do it for more than three arguments.

> parseArgs :: IO (Maybe Option)
> parseArgs = parseArgs' option0 <$> getArgs
>   where
>     option0 = Option{ isInteractive = False, sourceFile = Nothing }
>     parseArgs' opt [] = Just opt
>     parseArgs' opt ["-i"] = Just opt{ isInteractive = True}
>     parseArgs' opt [file] = Just opt{ sourceFile = Just file }
>     parseArgs' opt ["-i", file] =
>       Just opt{ isInteractive = True
>               , sourceFile = Just file }
>     parseArgs' _   _ = Nothing

 Interpreting a file. Parse a file first, then `runProgram` the result.
It's simple!

> interpret :: Maybe FilePath -> IO ()
> interpret Nothing = printUsage
> interpret (Just filePath) =
>   do parseResult <- parseFile programP filePath
>      case parseResult of
>        Left errMsg -> putStrLn errMsg
>        Right program -> 
>          do _ <- runExceptT $ runProgram initialEnv program
>             return ()

> printUsage :: IO ()
> printUsage = putStrLn "wyorepl [-i] [FILE]"

 Finally, the function to jump to the implementation of REPL.
Look it back after you read ReplSyntax.lhs, then Repl.lhs .

> interactive :: Maybe FilePath -> IO ()
> interactive mayFilePath = runRepl $
>   do maybe (return ()) (handleCommand . Load) mayFilePath
>      repl
