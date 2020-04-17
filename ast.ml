(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | And | Or

type typ = Int | Bool 

type expr =
    Literal of int
    | BoolLit of bool
    | Id of string
    | Binop of expr * op * expr
    | Assign of string * expr
    | Call of string * expr list

type stmt = 
    Block of stmt list
    | Expr of expr
    | If of expr * stmt * stmt
    | While of expr * stmt
    | For of stmt * stmt
    | Return of expr

type bind = typ * string

type func_def = {
    rtyp: typ;
    fname: string;
    formals: bind list;
    body: stmt list;
}

type program = stmt list * func_def list
