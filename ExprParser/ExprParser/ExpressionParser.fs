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
    | C
    | D
    | E
    | F
    | G
    | Parens of Ast
    | Prefix of string * Ast
    | Postfix of string * Ast 
    | Infix of string * Ast * Ast
    | Call of Ast * Ast list      // a(...)
    | Coord of Ast * Ast list     // a[...]

// ============================================================================
// Whitespace control
// ============================================================================

// No whitespace allowed at this point
let pNoSpace : Parser<unit,unit> =
    notFollowedBy (skipMany1 (anyOf " \t\r\n")) <?> "<no whitespace>" <!> "<pNoSpace>"

// At least one whitespace required, but NOT directly before ), ] or ,
let pSpace : Parser<unit,unit> =
    skipMany1 (anyOf " \t\r\n")
    >>% () <?> "<significant whitespace>" <!> "<pSpace>"

// Optional whitespace (for comma lists etc.)
let pOptSpace : Parser<unit,unit> =
    skipMany (anyOf " \t\r\n") >>% () <?> "<whitespace>" <!> "<pOptSpace>"

// ============================================================================
// Operator sets
// ============================================================================

// Prefix operators: - ~ #
let pPrefixOp : Parser<string,unit> =
    many1Satisfy (fun c -> "-~#".Contains(c)) <!> "<pPrefixOp>"

// Postfix operators: ! ' /
let pPostfixOp : Parser<string,unit> =
    many1Satisfy (fun c -> "!'/".Contains(c)) <!> "<pPostfixOp>"

// Infix operators: + - * /
let pInfixOp : Parser<string,unit> =
    many1Satisfy (fun c -> "+-*/".Contains(c)) <!> "<pInfixOp>"

// ============================================================================
// Forward declaration
// ============================================================================

let pExpr, pExprRef = createParserForwardedToRef<Ast,unit>()

// ============================================================================
// Literals and base atoms
// ============================================================================

let pLiteral : Parser<Ast,unit> =
    choice [
        skipChar 'a' >>% A
        skipChar 'b' >>% B
        skipChar 'c' >>% C
        skipChar 'd' >>% D
        skipChar 'e' >>% E
        skipChar 'f' >>% F
        skipChar 'g' >>% G
    
    ] <!> "<pLiteral>"

let pLeftPar : Parser<unit,unit> = 
    skipChar '(' >>. pOptSpace

let pRightPar : Parser<unit,unit> = 
    pOptSpace >>. skipChar ')' 

let pLeftBra : Parser<unit,unit> = 
    skipChar '[' >>. pOptSpace

let pRightBra : Parser<unit,unit> = 
    pOptSpace >>. skipChar ']' 

// Parenthesized expression, allowing spaces inside
let pParens : Parser<Ast,unit> =
    pLeftPar >>. pExpr .>> pRightPar
    |>> Parens <!> "<pParens>"

// ============================================================================
// Expr list for arguments / coordinates
// ============================================================================

let pComma : Parser<unit,unit> = 
    pOptSpace >>. skipChar ',' >>. pOptSpace

let pExprList : Parser<Ast list,unit> =
    (pipe2
        pExpr
        (many (attempt (pComma >>. pExpr)))
        (fun first rest -> first :: rest))
    <|> preturn [] <!> "pExprList"

// ============================================================================
// Call / Coord suffixes on literals (no space allowed before '(' or '[')
// ============================================================================

let pArgs : Parser<Ast list,unit> =
    pLeftPar >>. pExprList .>> pRightPar <!> "pArgs"

let pCoords : Parser<Ast list,unit> =
    pLeftBra >>. pExprList .>> pRightBra <!> "pCoords"

let pCallOrCoordSuffixCore : Parser<(Ast -> Ast),unit> =
    pNoSpace >>.
    choice [
        pArgs   |>> fun args   -> fun bas -> Call(bas, args)
        pCoords |>> fun coords -> fun bas -> Coord(bas, coords)
    ]

let pCallOrCoordSuffix =
    pCallOrCoordSuffixCore <!> "pCallOrCoordSuffix"

// Literal extended by optional call/coord chains
let pOperandAtom : Parser<Ast,unit> =
    pipe2
        pLiteral
        (many (attempt pCallOrCoordSuffix))
        (fun lit suffixes ->
            List.fold (fun acc f -> f acc) lit suffixes
        ) <!> "pOperandAtom"

// Atom: either extended literal or parenthesized expression
let pAtom : Parser<Ast,unit> =
    pOperandAtom
    <|> pParens <!> "pAtom"

// ============================================================================
// Precedence: postfix > prefix > infix
// (calls/coords are part of the atom, i.e. tighter than postfix/prefix)
// ============================================================================

// POSTFIX: atom postfix*
let pPostfixExpr : Parser<Ast,unit> =
    pipe2
        pAtom
        (many (attempt (pNoSpace >>. pPostfixOp)) <?> "<postfix symbol>")
        (fun expr postfixes ->
            List.fold (fun acc op -> Postfix(op, acc)) expr postfixes
        ) <!> "pPostfixExpr"

     
// PREFIX: prefix* postfixExpr
let pPrefixExpr : Parser<Ast,unit> =
    pipe2
        (many (attempt (pPrefixOp .>> pNoSpace)) <?> "<prefix symbol>")
        pPostfixExpr
        (fun prefixes expr ->
            List.foldBack (fun op acc -> Prefix(op, acc)) prefixes expr
        ) <!> "pPrefixExpr"

// INFIX: prefixExpr (space infixOp space prefixExpr)*
let pInfixExpr : Parser<Ast,unit> =
    pipe2
        pPrefixExpr
        (many (attempt (pSpace >>. pInfixOp .>> pSpace .>>. pPrefixExpr)) )
        (fun first rest ->
            List.fold (fun oper1 (op, oper2) -> Infix(op, oper1, oper2)) first rest
        ) <!> "pInfixExpr"

pExprRef.Value <- pInfixExpr <!> "pExpr"

// ============================================================================
// Entry point
// ============================================================================

let parse input =
    run (pExpr .>> eof) input

// ============================================================================
// For some example inputs, see failing and succeeing test cases in the ExpreParserTest project.
// ============================================================================
