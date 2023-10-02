module Parser

open FParsec
open System
open AbstractSyntaxTree

type UserState = {InMethod: bool; TopLevel: bool}
    with
        static member Default = {InMethod = false; TopLevel = true;}

type Parser<'a> = Parser<'a, UserState>

let str_ws str = pstring str >>. spaces1
let str str = pstring str >>. spaces
let ws = spaces
let letter = asciiLetter
let digit = digit

let p_operator : Parser<_> = anyOf "+-/*^!." // &|

let p_infixOp : Parser<_> = anyOf "+-/*." |>> function
                                             | '+' -> Add
                                             | '-' -> Sub
                                             | '/' -> Div
                                             | '*' -> Mul
                                             | '.' -> Dot

let reservedIdentifiers = ["func"; "var"; "if"; "then"; "else"; "type"; "and"; "or"; "not"; "is"; "for";]

let lt = pstring "<" <|> pstring "<="
let gt = pstring ">" <|> pstring ">="
let comparator : Parser<_> = lt <|> gt <|> pstring "=" <|> pstring "!="

let p_true : Parser<_> = stringReturn "true" (LBool(true))
let p_false : Parser<_> = stringReturn "false" (LBool(false))
let p_boolean : Parser<_> = (p_true <|> p_false)

// identifier ::= { "_" } , letter | digit , { letter | digit | "_" } ; 
let p_identifier : Parser<_> =
    let isValidFirstChar c = isLetter c || c = '_'
    let isValidChar c = isLetter c  || isDigit c || c = '_'
    let isValidIdentifier s =
        if reservedIdentifiers |> List.exists((=) s) then fail ("'" + s + "' is a reserved word.")
        else preturn s
    (many1Satisfy2L isValidFirstChar isValidChar "identifier") >>= isValidIdentifier
        

// integer ::= digit , { digit } ;
let p_integer : Parser<_> =
    let isValidChar c = isDigit c
    many1SatisfyL isValidChar "integer" |>> fun x -> LInteger(Convert.ToInt32(x))

// float ::= digit , { digit } , "." , digit , { digit } ;
let p_float : Parser<_> =
    // Basic int parser, but takes a value for the error message
    let p_int s = many1SatisfyL isDigit s
    pipe2 (p_int "float" .>>? skipChar '.') (p_int "fractional part") (fun x y -> LFloat(Convert.ToDouble(x + "." + y)))

// Attempts to parse a float, if that fails, tries to parses an int
let p_number : Parser<_> = p_float <|> p_integer

// '"' , { letter | whitespace | digit | symbol | operator | comparator } , '"' ;
let p_string : Parser<_> =
    // Parses any character that isn't a backslash or a quote
    let parseNormalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    // Helper called whenever an escaped character is found, converts from a single char to the valid escaped char
    let handleEscapeChar = function
                            | 'n' -> '\n'
                            | 't' -> '\t'
                            | c -> c
    // Parses an escaped character, first taking the '\', then parsing either a '\', '"', 'n', or 't'
    let parseEscapedChar = pstring "\\" >>. (anyOf "\\nt\"" |>> handleEscapeChar)
    let pquote = pstring "\""
    // Parses a series of normal or escaped characters between two quotes, then passes the result into an LString type
    (between pquote pquote (manyChars (parseNormalChar <|> parseEscapedChar))) |>> LString

// Parses any literal value with a correct error message if all of them fail
let p_literal = (p_boolean <|> p_number <|> p_string) <?> "integer, float, boolean or string"

// Forwarded parsers, allows these parsers to call themselves recursively
let p_methodCall, p_methodCallImpl = createParserForwardedToRef<Expression, UserState>()
let p_typedVarDecl, p_typedVarDeclImpl = createParserForwardedToRef<Expression, UserState>()
let p_untypedVarDecl, p_untypedVarDeclImpl = createParserForwardedToRef<Expression, UserState>()
let p_expression, p_expressionImpl = createParserForwardedToRef<Expression, UserState>()

// Helper parsers
let p_identifier' = p_identifier |>> Identifier
let p_literal' = p_literal |>> Value

// Implementation of the function call parser
// identifier , { "." , identifier } , "(" , [ { expression , "," } | expression ] , ")"
do p_methodCallImpl :=
    let p_arg = p_expression |>> Arg
    let p_args = between (pstring "(") (pstring ")") (sepBy p_arg (ws .>> pstring "," .>> ws))
    (p_identifier .>>.? p_args |>> FunctionCall) <?> "method call"

// Implementation of the typed variable declaration parser
// identifier , ":" , identifier , [ "=" , expression ]
do p_typedVarDeclImpl :=
    let p_type = p_identifier <|> (pipe3 (pstring "[") p_identifier (pstring "]") (fun x y z -> x + y + z))
    let p_nameType = tuple2 (p_identifier .>> ws) (str ":" >>. p_type)
    let p_withoutValue = str_ws "var" >>. p_nameType .>> ws
    let p_withValue = tuple2 (p_withoutValue) (str "=" >>. p_expression .>> ws)
    (attempt p_withValue |>> TypedVarDecl') <|> (p_withoutValue |>> TypedVarDecl)

// Implementation of the untyped variable declaration parser
// identifier , "=" , expression
do p_untypedVarDeclImpl :=
    let p = (str_ws "var") >>. p_identifier .>> ws .>>. (str "=" >>. p_expression)
    p |>> UntypedVarDecl

// Parse a typed or untyped variable declaration
let p_varDecl =
    attempt (p_typedVarDecl) <|> p_untypedVarDecl

// Parse an array literal of the form
// "[" , { expression , ";" } , expression , "]"
let p_arrayLiteral =
    str "[" >>. (sepBy1 p_expression (str ",")) .>> str "]" |>> ArrayLiteral

// Parse sugared array get
// ( identifier | array_literal ) , "[" , expression , "]"
let p_arrayGet =
    attempt (tuple2 (p_arrayLiteral <|> p_identifier') (str "[" >>. p_expression .>> str "]")) |>> ArrayGet

// Parse sugared array set
// ( identifier | array_literal ) , "[" , expression , "]" , "=" , expression
let p_arraySet =
    attempt (tuple3 (p_arrayLiteral <|> p_identifier') (str "[" >>. p_expression .>> str "]") (str "=" >>. p_expression)) |>> ArraySet

// Parse an inline if statement (evaluates to an expression)
// "if" , expression , "then" , expression , "else" , expression ;
let p_inlineIf =
    tuple3 (str_ws "if" >>. p_expression) (str_ws "then" >>. p_expression) (str_ws "else" >>. p_expression) |>> InlineIf

// Builds an OperatorPrecedenceParser to handle the parsing of expressions
let operatorParser = OperatorPrecedenceParser<Expression, unit, UserState>()
p_expressionImpl := operatorParser.ExpressionParser
do
    // Parsers that can be used to parse an operand
    let p_value = choice [  attempt p_methodCall;
                            p_varDecl;  
                            p_inlineIf;                          
                            p_arrayGet;
                            p_arraySet;
                            p_arrayLiteral;
                            p_literal';
                            p_identifier';
                          ]
    // The parser can consume either operands, or expressions between brackets
    let p_operand = (p_value <|> (between (str "(") (str ")") p_expression)) .>> ws
    operatorParser.TermParser <- p_operand // Assign the p_operand parser to the operator parser
    // List of infix operator strings, their precedence, and internal type
    let infixOperators = [("and", 1, And);
                          ("or",  1, Or);
                          ("==",  3, Equality);
                          ("<",   4, LsThan);
                          (">",   4, GrThan);
                          ("<=",  4, LsThanEq);
                          (">=",  4, GrThanEq);
                          ("!=",  4, Inequality);
                          ("+",   5, Add); 
                          ("-",   5, Sub);
                          ("*",   6, Mul); 
                          ("/",   6, Div); 
                          ("^",   7, Pow); 
                          ("%",   7, Mod); 
                          (".",   9, Dot);
                          ]
    // List of prefix operator strings, precedence, and internal type
    let prefixOperators = [ ("+",   1, Add); 
                            ("-",   1, Sub);
                            ("not", 1, Not);
                            ]
    // For each infix operator, add an operator to the parser
    for (operator, precedence, node) in infixOperators do
        // First parses the operator, then any possible whitespace
        // If the operator is a '.' (dot notation), then the operator is right-associative, if not, it is left-associative
        // A lambda is passed that produces an InfixOp structure
        operatorParser.AddOperator(InfixOperator(operator, ws, precedence, (if (operator = ".") then Associativity.Right else Associativity.Left), fun lhs rhs -> InfixOp(lhs, node, rhs)))
    for (operator, precedence, node) in prefixOperators do
        // Same as above, but handles prefix operators
        operatorParser.AddOperator(PrefixOperator(operator, ws, precedence, true, fun rhs -> PrefixOp(node, rhs)))

// Gets the current position of the parser in the stream, and returns it as a pair of integers
let pos =
    getPosition |>> fun p -> (p.Line, p.Column)

// Parses an expression, and stores it as a Statement
let p_expression' =
    pos .>>. p_expression |>> Expr

// Parses at least one, or more, expressions, and returns a list
let p_expressions =
    many1 (p_expression)

let p_assignment =
    tuple3 pos (p_expression .>> spaces) (str "=" >>. p_expression) |>> Assignment

// Forwarded declaration of the statement parser
let p_statement, p_statementImpl = createParserForwardedToRef<Statement, UserState>()
// Forwarded declaration of the ifStatement parser (used with else-else if)
let p_ifStatement, p_ifStatementImpl = createParserForwardedToRef<Statement, UserState>()

// Parses a type statement
let p_type =
    // Variable parser, parses zero or more variables separated by semi-colons
    let variables = sepEndBy p_typedVarDecl (str ";")
    // 'Parser' updates the user state to set TopLevel to true
    let setState = updateUserState (fun us -> { us with TopLevel = false })
    // 'Parser' that sets TopLevel to false in the user state
    let removeState = updateUserState (fun us -> { us with TopLevel = true })
    // 'Parser' that checks if TopLevel is true, and fails if it isn't
    let atTopLevel = userStateSatisfies (fun us -> us.TopLevel)
    // Parses "'type' identifier" part of the statement
    let name = str_ws "type" >>. p_identifier .>> ws
    // Parses variables between the two curly braces
    let body = str "{" >>. between (setState) (removeState) (variables) .>> str "}"
    // Creates a final parser that returns the position, and the type statement
    tuple2 (pos) (name .>> (atTopLevel <|> failFatally "types cannot be nested.") .>>. body) |>> Type; // Store the parser output as a Type structure

// Parses an if statement
let p_ifStatement' =
    // Parses the 'if' keyword, the expression, and 'then {'
    let first = str_ws "if" >>. p_expression .>> str_ws "then" .>> str "{"
    // Parses the body of the if statement
    let block = many (p_statement)
    // Parses the closing '}'
    let second = str "}"
    // Combine the above parsers and return an IfStatement
    pos .>>. ((first) .>>. block .>> second) |>> IfStatement

let p_elseStatement =
    str_ws "else" >>. str "{" >>. many (p_statement) .>> str "}"

let p_elseIfStatement =
    str_ws "else" >>. p_ifStatement

do p_ifStatementImpl :=
    let normalIf = p_ifStatement'
    let ifElse = pos .>>. attempt (p_ifStatement' .>>. p_elseStatement)
    let ifElseIf = attempt (p_ifStatement' .>>. p_elseIfStatement)
    ((ifElseIf |>> IfElseIfStatement) <|> (ifElse |>> IfElseStatement) <|> normalIf)

let p_parameter =
    (p_identifier .>> ws) .>>. (str ":" >>. p_identifier .>> ws) |>> BasicParameter

let p_parameterList =
    let param = p_parameter
    sepBy (param) (str ",")

let p_method =
    let setState b = updateUserState (fun us -> {us with InMethod = b; TopLevel = not b;})
    let checkState = userStateSatisfies (fun us -> us.InMethod = false)
    let p1 = str_ws "func" >>. p_identifier .>> (checkState <|> failFatally "methods cannot be nested.") .>> str "("
    let p2 = p_parameterList
    let p3' = between (setState true) (setState false) (many p_statement)
    let p3 = str ")" >>. str "{" >>. p3' .>> str "}"
    let method' = pos .>>. (tuple3 p1 p2 p3) |>> Method
    method'

let p_forLoop =
    let p_counterVariable = p_varDecl
    let p_condition = p_expression
    let p_iterator = p_statement
    let p_body = between (str "{") (str "}") (many p_statement)
    let p_for = tuple4 (str "for" >>. str "(" >>. p_counterVariable .>> str ";") (p_condition .>> str ";") (p_iterator .>> str ")") (p_body)
    pos .>>. p_for |>> ForStatement

let p_emptyStatement =
    pos .>> spaces1 |>> EmptyStatement

let p_assertStatement =
    str_ws "assert" >>. pos .>>. p_expression |>> Assertion

do p_statementImpl :=
    p_type <|> p_method <|> p_ifStatement <|> p_forLoop <|> p_assertStatement <|> attempt (p_assignment) <|> p_expression' <|>  p_emptyStatement

let p_program =
    many (p_statement)

let run p s =
    let result = runParserOnString p UserState.Default ""
    result s