namespace ExprParserTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open ExpressionParser


[<TestClass>]
type TestCases () =


    [<DataRow("00", "a( b )")>]
    [<DataRow("01", "a(b)")>]
    [<DataRow("02", "a[b, a]")>]
    [<DataRow("03", "a[b]'")>]
    [<DataRow("04", "~a'")>]
    [<DataRow("05", "a")>]
    [<DataRow("06", "(a + b)! - -(a - b)")>]
    [<DataRow("07", "(a + b)! + -(a - b)")>]
    [<DataRow("08", "a! + ~b - a + b")>]
    [<DataRow("09", "(a)")>]
    [<DataRow("10", "( a)")>]
    [<DataRow("11", "(a )")>]
    [<DataRow("12", "( a )")>]
    [<DataRow("13", "a(b,a)")>]
    [<DataRow("14", "a[b,a]")>]
    [<DataRow("15", "b[(a + b!)   ,   -b',a(b) ,b, a]")>]
    [<DataRow("16", "a(-b ,~a)")>]
    [<DataRow("17", "a(-b, ~a)")>]
    [<DataRow("18", "~a(b)[a, b!]' + b")>]
    [<DataRow("19", "a[]")>]
    [<DataRow("20", "a[ ]")>]
    [<DataRow("21", "a[b ]")>]
    [<DataRow("22", "a[b]")>]
    [<DataRow("23", "a[ b]")>]
    [<DataRow("24", "a[ b ]")>]
    [<DataRow("25", "a()")>]
    [<DataRow("25", "a( )")>]
    [<DataRow("26", "a(b )")>]
    [<DataRow("27", "a(b)")>]
    [<DataRow("28", "a( b)")>]
    [<DataRow("29", "a( b )")>]
    [<DataRow("30", "a(b,c[d])")>]
    [<DataRow("31", "a( b , c[ d ] )")>]
    [<DataRow("32", "a[b(c), d]")>]
    [<DataRow("33", "a[b(c ) , d( a , b ) ]")>]
    [<DataRow("34", "a(b)[c](d)[e]")>]
    [<DataRow("35", "a(b)[c ]( d )[ e ]")>]
    [<DataRow("36", "a! - -(b)")>]   
    [<DataRow("37", "~a! + b")>]  
    [<DataRow("38", "a(b)!")>]
    [<DataRow("39", "a[b]!'")>]
    [<DataRow("40", "a(b,c)!'")>]
    [<DataRow("41", "a[b,c]'!")>]
    [<DataRow("42", "a( b , c(d,e[f]) )")>]
    [<DataRow("43", "a[b , c(d , e[f] ) ]")>]
    [<DataRow("44", "a( (a + b) , ~(a!) )")>]
    [<DataRow("45", "a[ (a + b) , ~(a!) ]")>]
    [<DataRow("46", "a( b , c )[ d , e ]")>]
    [<DataRow("47", "a( b )[ c ]( d )[ e ]")>]
    [<DataRow("48", "a( b , c )[ d , e ]!'")>]
    [<DataRow("49", "~a(b[c], d[e(f)] )")>]
    [<DataRow("50", "a + b - c * d")>]
    [<TestMethod>]
    member this.TestMethodPassing (no:string, code:string) =
        let res = parse code
        let actual = sprintf "%O" res 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    // Single operand
    [<DataRow("01", "a")>]

    // Single operator
    [<DataRow("02", "a + b")>]

    // Two operators, left‑to‑right
    [<DataRow("03", "a + b - c")>]

    // Mixed precedence (should still flatten)
    [<DataRow("04", "a + b * c")>]
    [<DataRow("05", "a * b + c")>]
    [<DataRow("06", "a + b - c * d")>]

    // Parentheses (your parser keeps Parens nodes)
    [<DataRow("07", "(a + b) * c")>]
    [<DataRow("08", "a * (b + c)")>]
    [<DataRow("09", "(a)")>]

    // Prefix operators
    [<DataRow("10", "-a + b")>]
    [<DataRow("11", "a + -b")>]

    // Postfix operators
    [<DataRow("12", "a! + b")>]
    [<DataRow("13", "a + b!")>]

    // Combined prefix + postfix
    [<DataRow("14", "-a! + b")>]

    // Long chain
    [<DataRow("15", "a + b + c + d + e + f")>]

    [<DataRow("17", "a   +    b   -   c")>]

    // Nested parentheses
    [<DataRow("19", "((a + b) - (c * d))")>]

    // Call expressions 
    [<DataRow("20", "a(b) + c")>]
    [<DataRow("21", "a(b, c) * d")>]

    // Coord expressions 
    [<DataRow("22", "a[b] + c")>]

    // Edge cases
    [<DataRow("23", "a + b * c / d - e")>]
    [<DataRow("24", "a / b / c / d")>]
    [<TestMethod>]
    member this.TestMethodPassing2 (no:string, code:string) =
        let res = parse code
        let actual = sprintf "%O" res 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00", "a/ b")>]               // no infix symbol
    [<DataRow("01", "(a + b)!! --(a - b)")>]// no infix symbol
    [<DataRow("02", "~-a(b)[a, b!]/ b")>]   // no infix symbol
    [<DataRow("03", "(a + b)! --(a - b)")>] // no infix symbol
    [<DataRow("04", "a( b , c [ d ] )")>]   // space after c
    [<DataRow("05", "a! (b)")>]             // no infix symbol
    [<DataRow("06", "a + +b")>]             // wrong prefix symbol
    [<DataRow("07", "a (b)")>]              // space before '(' not allowed
    [<DataRow("08", "a [b]")>]              // space before '[' not allowed
    [<DataRow("09", "a(b  c)")>]            // missing comma
    [<DataRow("10", "a[b  c]")>]            // missing comma
    [<DataRow("11", "a(b,)")>]              // trailing comma without expr
    [<DataRow("12", "a[,b]")>]              // leading comma without expr
    [<DataRow("13", "a(b,,c)")>]            // double comma
    [<DataRow("14", "a[b,,c]")>]            // double comma
    [<DataRow("15", "a(b")>]                // missing ')'
    [<DataRow("16", "a[b")>]                // missing ']'
    [<DataRow("17", "a(b]")>]               // mismatched delimiters
    [<DataRow("18", "a[b)")>]               // mismatched delimiters
    [<DataRow("19", "a(b]c)")>]             // extra garbage after expr
    [<DataRow("20", "a(b) c")>]             // space after call → infix expected
    [<DataRow("21", "a[b] c")>]             // space after coord → infix expected
    [<DataRow("22", "a!~")>]                // prefix missing operand
    [<DataRow("23", "a!!~")>]               // prefix missing operand after postfix chain
    [<DataRow("24", "~!a")>]                // postfix cannot precede prefix
    [<DataRow("25", "a+/b")>]               // infix operator must be surrounded by spaces
    [<DataRow("26", "a +~ b")>]             // invalid infix operator
    [<DataRow("27", "a(b : c)")>]           // invalid comma / infix operator
    [<DataRow("25", "a * b ^ c")>]          // ^ is no in infix set
    [<DataRow("26", "   a + b   ")>]        // Whitespace variations
    [<DataRow("27", "a+b")>]                // Whitespace variations
    [<TestMethod>]
    member this.TestMethodFailing (no:string, code:string) =
        let res = parse code
        let actual = sprintf "%O" res 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))


