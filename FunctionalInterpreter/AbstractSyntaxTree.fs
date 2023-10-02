module AbstractSyntaxTree

open FParsec

type Literal =
    | LBool of bool
    | LInteger of int32
    | LFloat of float
    | LString of string

type Operator =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Not
    | Dot
    | Pow
    | Equality
    | Inequality
    | GrThan
    | LsThan
    | GrThanEq
    | LsThanEq

type Expression =
    | Value of Literal
    | Identifier of string
    | InfixOp of Expression * Operator * Expression
    | PrefixOp of Operator * Expression
    // ---
    | TypedVarDecl of (string * string)
    | TypedVarDecl' of (string * string) * Expression
    | UntypedVarDecl of string * Expression
    // ---
    | FunctionCall of string * Argument list
    // ---
    | InlineIf of Expression * Expression * Expression
    // ---
    | ArrayLiteral of Expression list
    | ArrayGet of Expression * Expression
    | ArraySet of Expression * Expression * Expression
and Argument =
    | Arg of Expression

type Parameter =
    | BasicParameter of string * string

type Pos = int64 * int64

type Statement =
    | EmptyStatement of Pos
    | Expr of Pos * Expression
    | Method of Pos * (string * Parameter list * Statement list)
    | Type of Pos * (string * Expression list)
    // ---
    | IfStatement of Pos * (Expression * Statement list)
    | IfElseStatement of Pos * (Statement * Statement list)
    | IfElseIfStatement of (Statement * Statement)
    // ---
    | ForStatement of Pos * (Expression * Expression * Statement * Statement list)
    | Assignment of Pos * Expression * Expression
    | Assertion of Pos * Expression