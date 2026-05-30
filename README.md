# expressionParser

## What is this repository about? 

This repository contains an F# module with a parser based on the FParsec library that is a study of how to program a parser capable to accept complex mathematical expressions.

The focus of this repo is the clarity of how to approach the parsing process. The resulting parser has no specific productive purpose.

## Features of the parser are as follows:

### Operands
For simplicity reasons only 'a' and 'b' are allowed.

### Operators
  * Prefix operators (for simplicity reasons one or more characters from "-~#")
  * Postfix operators (for simplicity reasons one or more characters from "!'/")
  * Infix operators (for simplicity reasons one or more characters from "+-*/")
  * Note: operator character sets may overlap
  * Whitespace‑sensitive operator disambiguation control
  * Multi‑character operators like ++, --, !=, <= allowed
 
### Expressions 
* Expressions can be separated by infix operators.
* Parenthesized expressions are e.g "(a + b)" are allowed
* Terms can have coordinates "a[...]", each coordinate beeing an expression.
* Terms can have arguments "a(...)", each argument beeing an expression.

### AST 
Each input is transformed into an ast.

### Other features
* Debugging the parser possible
* Inbuild operator precedence (prefix > postfix > infix)
* Full whitespace control

### Usage and testing 

```
// parse "a"
// parse "a(b,a)"
// parse "a[b,a]"
// parse "b[(a + b!)   ,   -b',a(b) ,b, a]"
// parse "a(-b ,~a)"
// parse "(a )"

let res parse "~a(b)[a, b!]' + b"
printfn "%O" res

// output
Success: Infix
  (Postfix (Prefix ("~", Coord (Call (A, [B]), [A; Postfix (B, "!")])), "'"),
   "+", B)
```
