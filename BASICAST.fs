module BASICAST
    
type Expr =
    | Plus of int * Expr * Expr
    | Minus of int * Expr * Expr
    | Times of int * Expr * Expr
    | Div of int * Expr * Expr
    | Pow of int * Expr * Expr
    | Math of string * Expr
    | Number of int * string
    | Fn of string * Expr Option * string Option
    | Variable of int * string
    | List of int * string * Expr
    | Table of int * string * Expr * Expr
    | ListDim of string * string
    | TableDim of string * string * string
    
type BFun =
    | Function of string * Expr * Expr
    
type NodoElemPrint =
    | PEmpty
    | PStr of string
    | PExpr of Expr
    | PComma

type Stmt =
    | Let of int * string * Expr * Expr
    | Read of int * string * Expr list
    | Data of int * string * string list
    | Print of int * string * NodoElemPrint list
    | Goto of int * string * string
    | If of int * string * Expr * string * Expr * string
    | For of int * string * Expr * Expr * Expr * Expr Option
    | Next of int * string * Expr
    | End of int * string
    | Stop of int * string
    | Def of int * string * string * Expr * Expr
    | Gosub of int * string * string
    | Return of int * string
    | Dim of int * string * Expr list
    | Rem of int * string

type Prog = Program of Stmt list
