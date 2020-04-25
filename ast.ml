(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | And | Or

type typ = Int | Bool 

type bind = typ * string

type expr =
    Literal of int
    | BoolLit of bool
    | Id of string
    | Binop of expr * op * expr
    | Assign of string * expr
    | BindAssign of typ * string * expr 
    | Call of string * expr list

type stmt = 
    Block of stmt list
    | Expr of expr
    | If of expr * stmt * stmt
    | While of expr * stmt
    | Return of expr

type func_def = {
    rtyp: typ;
    fname: string;
    formals: bind list;
    body: stmt list;
}

type program = stmt list * func_def list

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Greater -> ">" 
  | And -> "&&"
  | Or -> "||"
                                                     
let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
 
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | BindAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^
                           string_of_expr e 
  | Call(f, el) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
                                                            
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"
                                                               
let string_of_fdecl fdecl =
    string_of_typ fdecl.rtyp ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
    ")\n{\n" ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"
                                                                           
let string_of_program (stmts, funcs) =
    "\n\nParsed program: \n\n" ^
    String.concat "" (List.map string_of_stmt stmts) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs)                                                                                    
