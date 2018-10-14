# Write Your Own REPL

This project is meant to be a tutorial to write your own REPL, using ["haskeline" package](https://www.stackage.org/lts-12.12/package/haskeline-0.7.4.3).

Each source file is written in Literate Haskell, and intended to be read directly.

Recommented order of read is:

* [Language](src/Language.lhs)
* [Parser](src/Parser.lhs)
* [Main](src/Main.lhs)
* [ReplSyntax](src/ReplSyntax.lhs)
* [Repl](src/Repl.lhs)
* [Main](src/Main.lhs) again

## Exercise

 * Implement LOOP statement which loops enclosing statements specified times.
   Then test it in REPL. After that, input a LOOP statement which takes long
   time to execute in REPL, and try interrupting it by typing Ctrl-C.
   
   ```haskell
   data Statement = Assign VarName Expression
                  | Print Expression
                  | If Expression Program
                  | Loop Expression Program
   ```
