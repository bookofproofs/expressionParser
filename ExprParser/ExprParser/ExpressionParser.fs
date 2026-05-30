module ExpressionParser

open FParsec

let debug = false // change to true if you want the parser to run in debug mode (printing in <!>)

// ============================================================================
// Debug combinator
// ============================================================================

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        if debug then 
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply
        else
            p stream

// ============================================================================
// AST
// ============================================================================

type Ast =
    | A
    | B
    | Parens of Ast
    | Prefix of string * Ast
    | Postfix of Ast * string
    | Infix of Ast * string * Ast
    | Call of Ast * Ast list      // a(...)
    | Coord of Ast * Ast list     // a[...]

// ============================================================================
// Whitespace control
// ============================================================================

// No whitespace allowed at this point
let pNoSpace : Parser<unit,unit> =
    notFollowedBy (skipMany1 (anyOf " \t\r\n")) <?> "<no whitespace>"

// At least one whitespace required, but NOT directly before ), ] or ,
let pSpace : Parser<unit,unit> =
    (skipMany1 (anyOf " \t\r\n")
     .>> notFollowedBy (anyOf "),]"))
    >>% () <?> "<significant whitespace>"

// Optional whitespace (for comma lists etc.)
let pOptSpace : Parser<unit,unit> =
    skipMany (anyOf " \t\r\n") >>% () <?> "<whitespace>"

// ============================================================================
// Operator sets
// ============================================================================

// Prefix operators: - ~ #
let pPrefixOp : Parser<string,unit> =
    many1Satisfy (fun c -> "-~#".Contains(c))

// Postfix operators: ! ' /
let pPostfixOp : Parser<string,unit> =
    many1Satisfy (fun c -> "!'/".Contains(c))

// Infix operators: + - * /
let pInfixOp : Parser<string,unit> =
    many1Satisfy (fun c -> "+-*/".Contains(c))

// ============================================================================
// Forward declaration
// ============================================================================

let pExpr, pExprRef = createParserForwardedToRef<Ast,unit>()

// ============================================================================
// Literals and base atoms
// ============================================================================

let pLiteral : Parser<Ast,unit> =
    (pchar 'a' >>% A)
    <|>
    (pchar 'b' >>% B)

// Parenthesized expression, allowing spaces inside
let pParens : Parser<Ast,unit> =
    pchar '('
    >>. pOptSpace
    >>. pExpr
    .>> pOptSpace
    .>> pchar ')'
    |>> Parens

// ============================================================================
// Expr list for arguments / coordinates
// ============================================================================

let pComma : Parser<unit,unit> =
    pOptSpace >>. pchar ',' >>. pOptSpace

let pExprListCore : Parser<Ast list,unit> =
    (pipe2
        pExpr
        (many (attempt (pComma >>. pExpr)))
        (fun first rest -> first :: rest))
    <|> preturn []

let pExprList = pExprListCore <!> "pExprList"

// ============================================================================
// Call / Coord suffixes on literals (no space allowed before '(' or '[')
// ============================================================================

let pArgs : Parser<Ast list,unit> =
    (pchar '('
     >>. pOptSpace
     >>. pExprList
     .>> pOptSpace
     .>> pchar ')') <!> "pArgs"

let pCoords : Parser<Ast list,unit> =
    (pchar '['
     >>. pOptSpace
     >>. pExprList
     .>> pOptSpace
     .>> pchar ']') <!> "pCoords"

let pCallOrCoordSuffixCore : Parser<(Ast -> Ast),unit> =
    pNoSpace >>.
    choice [
        pArgs   |>> fun args   -> fun bas -> Call(bas, args)
        pCoords |>> fun coords -> fun bas -> Coord(bas, coords)
    ]

let pCallOrCoordSuffix =
    pCallOrCoordSuffixCore <!> "pCallOrCoordSuffix"

// Literal extended by optional call/coord chains
let pOperandAtomCore : Parser<Ast,unit> =
    pipe2
        pLiteral
        (many (attempt pCallOrCoordSuffix))
        (fun lit suffixes ->
            List.fold (fun acc f -> f acc) lit suffixes
        )

let pOperandAtom =
    pOperandAtomCore <!> "pOperandAtom"

// Atom: either extended literal or parenthesized expression
let pAtomCore : Parser<Ast,unit> =
    pOperandAtom
    <|> pParens

let pAtom =
    pAtomCore <!> "pAtom"

// ============================================================================
// Precedence: postfix > prefix > infix
// (calls/coords are part of the atom, i.e. tighter than postfix/prefix)
// ============================================================================

// POSTFIX: atom postfix*
let pPostfixExprCore : Parser<Ast,unit> =
    pipe2
        pAtom
        (many (attempt (pNoSpace >>. pPostfixOp)))
        (fun expr postfixes ->
            List.fold (fun acc op -> Postfix(acc, op)) expr postfixes
        )

let pPostfixExpr =
    pPostfixExprCore <!> "pPostfixExpr"

// PREFIX: prefix* postfixExpr
let pPrefixExprCore : Parser<Ast,unit> =
    pipe2
        (many (attempt (pPrefixOp .>> pNoSpace)) <?> "<prefix symbol>")
        pPostfixExpr
        (fun prefixes expr ->
            List.foldBack (fun op acc -> Prefix(op, acc)) prefixes expr
        )

let pPrefixExpr =
    pPrefixExprCore <!> "pPrefixExpr"

// INFIX: prefixExpr (space infixOp space prefixExpr)*
let pInfixExprCore : Parser<Ast,unit> =
    pipe2
        pPrefixExpr
        (many (attempt (pSpace >>. pInfixOp .>> pSpace .>>. pPrefixExpr)) <?> "<infix symbol>")
        (fun first rest ->
            List.fold (fun acc (op, rhs) -> Infix(acc, op, rhs)) first rest
        )

let pInfixExpr =
    pInfixExprCore <!> "pInfixExpr"

pExprRef.Value <- (pInfixExpr <!> "pExpr")

// ============================================================================
// Entry point
// ============================================================================

let parse input =
    run (pExpr .>> eof) input

// ============================================================================
// For some example inputs, see failing and succeeing test cases in the ExpreParserTest project.
// ============================================================================
