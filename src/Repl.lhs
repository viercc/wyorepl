 Notice: This is the finale of this tutorial, the actual implementation of
REPL. You may want to read other modules first.

 Let's start.

> module Repl(
>   Repl,
>   ReplCommand(..),
>   runRepl,
>   repl,
>   handleCommand,
> ) where

 Among modules we use ...

> import           Control.Monad.Except
> import           Control.Monad.State

 There's `System.Console.Haskeline` which is from haskeline package. That package
provides nifty way to interact with user by reading user inputs line by line,
from console, exactly for things like REPL.

 Important features of haskeline are handling for user interrupt (Ctrl-C) and
implementation of input history (that feature which gives you previous input when
you hit ↑ key). They are very important for pleasant console UI of your REPL,
but hard to implement correctly.

> import           System.Console.Haskeline

 Importing types and functions we prepared, we can start the implementation.

> import           Language
> import           Parser
> import           ReplSyntax

 First, we define a `Repl` Monad be `StateT` over `InputT IO`. `InputT` is a
monad transformer provided by haskeline, which provides all functionalities of
haskeline.
 `StateT` part carries the state of the interpreter, `Env`. For your actual
language, this can contain more than the state of interpreter, like which file
an user is currently focusing, what an user specified for how prompt looks, etc.

> type Repl = StateT Env (InputT IO)

 Here defines the way to run `Repl` as bare `IO`. `runInputT` is the method to
run `InputT`,

> runRepl :: Repl () -> IO ()
> runRepl m = fmap fst $ runInputT defaultSettings $ runStateT m initialEnv

 The next function, `handleCommand'`, is the core part. It processes each REPL
command.

> handleCommand' :: ReplCommand -> Repl ()

 To run a <Statement>. We already have `runStatement` function from `Language`
module. Its type was:

    runStatement :: Env -> Statement -> ExceptT String IO Env

 `Env` is the state `Repl` Monad is carrying. `Statement` is what we are given.
The result is wrapped in `ExceptT`, so we must unwrap it by `runExceptT`.

> handleCommand' (Run stmt) =
>   do env <- get
>      result <- liftIO $ runExceptT $ runStatement env stmt
>      case result of
>        Left errMsg -> lift $ outputStrLn errMsg
>        Right env'  -> put env'

 To evaluate and print <Expression>. What we have is `evalExpression` function.

    evalExpression :: (Monad m) => Env -> Expression -> ExceptT String m Int

 Let `m = Identity`.

    evalExpression :: Env -> Expression -> Except String Int
    runExcept :: Except a b -> Either a b

 Then it goes the same way to `(Run stmt)` case.

> handleCommand' (EvalPrint expr) =
>   do env <- get
>      let result = runExcept $ evalExpression env expr
>      case result of
>        Left errMsg -> lift $ outputStrLn errMsg
>        Right n     -> lift $ outputStrLn $ show n

 Loading file is not too different than above two. The weapons we have for it are:

    parseFile :: Parser a -> FilePath -> IO (Either String a)
    programP :: Parser Program
    runProgram :: Env -> Program -> ExceptT String IO Env

> handleCommand' (Load filePath) =
>   do parseResult <- liftIO $ parseFile programP filePath
>      env <- get
>      case parseResult of
>        Left errMsg -> lift $ outputStrLn errMsg
>        Right program ->
>          do runResult <- liftIO $ runExceptT $ runProgram env program
>             case runResult of
>               Left errMsg -> lift $ outputStrLn errMsg
>               Right env'  -> put env'

 Help is easy.

> handleCommand' Help = lift $ outputStrLn helpMessage

 We handle Quit at the level of caller of `handleCommand'`. So here leave it
no-op.

> handleCommand' Quit = return ()
> 
> 

 What is the next? Ctrl-C! This is not needed in our example actually, but
I want it to be included in this tutorial.

 Generally speaking, there is a chance the command user typed took very long
time to execute, or not even terminates. When an user ran into it, you probably
want he/she be able to interrupt the action - by typing Ctrl-C.
 haskeline supports it.

    withInterrupt :: (MonadException m) => InputT m a -> InputT m a
    handleInterrupt :: MonadException m => m a -> m a -> m a 

 During the action surrounded by `withInterrupt`, when user puts Ctrl-C, the
exception named Interrupt is thrown instead of aborting the entire program.
`handleInterrupt` sets up exception handler for the Interrupt exception.
And yes, `InputT` is also `MonadException`.

    instance MonadException m => MonadException (InputT m)

 Combining this two functions, you can easily write interruptable action.
But, this is defined for `InputT` only. Our `Repl` Monad was `StateT` over
`InputT`. So, we prepare a wrapper function for `Repl` here.

> interruptably :: Repl () -> Repl ()
> interruptably (StateT m) =
>   StateT $ \st ->
>     let abortCase = outputStrLn "Interrupted" >>
>                     return ((), st)
>     in withInterrupt $
>          handleInterrupt abortCase (m st)

 `handleCommand` is `handleCommand'` wrapped by `interruptably`.

> handleCommand :: ReplCommand -> Repl()
> handleCommand = interruptably . handleCommand'

 Finally, main loop which accepts user input and execute the command.

> repl :: Repl ()
> repl =
>   do input <- lift $ getInputLine "> "
>      case input of
>        Nothing     -> repl
>        Just cmdStr -> case parseReplCommand cmdStr of
>          Left errMsg -> lift (outputStrLn errMsg) >> repl
>          Right Quit  -> return ()
>          Right cmd   -> handleCommand cmd >> repl

