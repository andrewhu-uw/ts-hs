# TS-HS: a TypeScript compiler written in Haskell

Just a cool project I'm working on to learn Haskell, parser combinators, and TypeScript

## Structure

`Parser.hs` has a function `runParser` that you can use to test parse different segments of code

## Known Issues

The parser can be weird because it has some limitations. For instance, it cannot
parse multiple lines as one statement because it breaks up statements by lines and/or semicolons

In order to fix this there are a couple options, use a different parser that can handle lexed tokens
or change the parsing algorithm to support multiline expressions. I think ultimately we may have
to do both of these.

One idea for supporting multiline expressions is to try to parse one line, and if that fails, parse
that line plus the next line, but if we don't find the symbol we're looking for, then the lines
must be unrelated or incorrect, so we can safely fail. However, if we parse everything in that second
line, but haven't finished the expression, then we can recurse until the expression is completed or fails
to match with a line -- That's not a very functional paradigm, though

[Here's a good article](https://flaviocopes.com/javascript-automatic-semicolon-insertion/) about how 
semicolon rules work in JS. It looks like in order to have a standards compliant JS parser, we're going
to have to parse statements/expressions one at a time instead of breaking down a block into statements
and then parsing them individually. Unfortunately, newlines are still signficant if you want to 
have a standards compliant parser. However, we might be able to fix the edge cases during lexing to
make the parsing more generalized. The only edge cases are when `return`, `break`, `throw`, or `continue`
are on their own lines. Otherwise, we can parse statements until we fail.

## TODO

 - Add expression type checking
 - Type check functions
 - Add class declarations to symbol tables

 - Add other operators like +=
 - Add single quote string constants
 - Add function types/lambdas
 - Add readonly
 - Add yields

 - Write your own lexer/parser
