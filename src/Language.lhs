 To start demonstrating how to write REPL, we must build it on top of an
interpreter of a programming language. This module provides very small toy
programming language for the purpose of being example.

 This programming language is like a very limited form of BASIC. It only has two
statements. One is PRINT statement. It prints out the value of <Expression> when
executed.

    PRINT <Expression>
 
Another statement is assignment statement.

    <VarName> = <Expression>

 An assignment statement sets value of the variable specified by <VarName>
to the value of <Expression>.

 <Expression> is either <VarName>, int literal, or addition of two
<Expression>s. In pseudo BNF, the syntax of expression looks like the following.

    <Expression> := <VarName>
                  | <IntLiteral>
                  | <Expression> + <Expression>
                  | "(" <Expression> ")"

Let's define them in Haskell. This is the module header.

> {-# LANGUAGE FlexibleContexts #-}
> module Language(
>  VarName,
>  Expression(..),
>   Statement(..),
>   Program,
> 
>   Env, initialEnv,
> 
>   evalExpression,
>   runStatement,
>   runProgram
> ) where

We use `Map` and `ExceptT`.

> import qualified Data.Map as Map
> 
> import Control.Monad.Except

 First, we define AST (Abstract Syntax Tree) for our language.
<Expression> was either <VarName>, <IntLiteral>, addition, or parenthesis. We
can ignore parenthesis here, because parenthesis only specifies how operators
are associated, and it is already represented in AST by how a tree is
structured.

> type VarName = String
> 
> data Expression = Var VarName | Lit Int | Add Expression Expression
>     deriving (Show, Read, Eq, Ord)

 <Statement> was assignment or PRINT statement.

> data Statement = Assign VarName Expression
>                | Print Expression
>     deriving (Show, Read, Eq, Ord)

 A program is simply a sequence of <Statement>s.

> type Program = [Statement]

 These types can represent our programming language. Writing its interpreter is
easy. (You can skip all of them if you feel they're trivial.)

 We use `Map` from variable names to its value, and call it an `Env`.

> type Env = Map.Map VarName Int
> 
> initialEnv :: Env
> initialEnv = Map.empty

 To evaluate an expression, recursively tear down our AST for <Expression>.
  * Evaluating `Lit` is just returning the value of the literal.
  * Evaluating `Var` is looking up the `Env` for the variable name.
    If the variable is defined, return its value. If not, throw an error.
    To represent it may throw an error, here we use ExceptT.
  * Evaluating `Add` is simply evaluating both side and adding the result.

> evalExpression :: (Monad m) => Env -> Expression -> ExceptT String m Int
> evalExpression env = eval
>   where
>     eval (Lit n) = return n
>     eval (Var x) = case Map.lookup x env of
>       Nothing -> throwError $ "Undefined variable " ++ show x
>       Just n  -> return n
>     eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2

 Interpreting single statement.

> runStatement :: Env -> Statement -> ExceptT String IO Env
> runStatement env (Assign x expr) =
>   do n <- evalExpression env expr
>      return (Map.insert x n env)
> runStatement env (Print expr) =
>   do n <- evalExpression env expr
>      liftIO $ print n
>      return env

 Interpreting a program is interpreting all its statements,
while accumulating each changes to `Env`. That's `foldM`.

> runProgram :: Env -> [Statement] -> ExceptT String IO Env
> runProgram = foldM runStatement

Let's test this module. On GHCi:

    >>> let run program = runExceptT (runProgram initialEnv program)
    >>> run [Print (Lit 3)]
    3
    Right (fromList [])
    >>> let doublesX = Assign "x" (Add (Var "x") (Var "x"))
    >>> run [Assign "x" (Lit 1), doublesX, doublesX, Print (Var "x")]
    4
    Right (fromList [("x",4)])

Looks okay. Let's go to Parser.lhs to see the implementation of the parser for
our language.
