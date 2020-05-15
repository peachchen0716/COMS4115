(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | And | Or
        | GreaterEq | LessEq | Incre | Decre | Mod | Not

type typ = Int | Bool | Float | String | None | List of typ

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | FLit of float
  | StrLit of string
  | Id of string
  | ListLit of expr list
  | ListAccess of string * expr
  | ListSlice of expr * expr * expr
  (* TODO: change to Len of expr if time allows *)
  | Len of string 
  | ListPop of string
  | Uniop of expr * op
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  (* Noexpr is used within a for loop to make for (;a < 10;) valid *)
  | Noexpr

type stmt = 
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of stmt * expr * expr * stmt
  | BindAssign of typ * string * expr 
  | Return of expr
  (* append e2 to list e1 *)
  | ListAppend of string * expr
  (* insert e3 to list e1 at index e2 *)
  | ListInsert of expr * expr * expr
  | ListSort of expr
  | ListReverse of expr

type func_def = {
    rtyp: typ;
    fname: string;
    formals: bind list;
    body: stmt list;
}

(*
type func_sig = {
    rtyp: typ;
    formals: typ list;
}
*)

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
  | LessEq -> "<="
  | GreaterEq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Incre -> "++"
  | Decre -> "--"
  | Mod -> "%"
  | Not -> "!"
                                                     
let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | None -> "None"
  | List(t) -> "list " ^ string_of_typ t
 
let rec string_of_list = function
    [] -> ""
  | hd :: tl -> string_of_expr hd ^ ", " ^ string_of_list tl
and string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FLit(f) -> string_of_float f
  | StrLit(s) -> s
  | Id(s) -> s
  | ListLit(lst) -> "[ " ^ string_of_list lst ^ " ]"
  | ListAccess(s, e) -> "list " ^ s ^ "[" ^ string_of_expr e ^ "]"
  | ListSlice(e1, e2, e3) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^
                             " : " ^ string_of_expr e3 ^ "]"
  | Len(s) -> "len(" ^ s ^ ")"
  | ListPop(s) -> "pop" 
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Uniop(e, o) -> string_of_expr e ^ " " ^ string_of_op o
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> "Noexpr"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(s1, e2, e3, s) -> "For ( " ^ string_of_stmt s1 ^ "; " ^
                          string_of_expr e2 ^ "; " ^ 
                          string_of_expr e3 ^ ")\n" ^
                          string_of_stmt s
  | BindAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^
                           string_of_expr e ^ "\n"
  | ListAppend(s, e) -> "append"
  | ListInsert(e1, e2, e3) -> "insert"
  | ListSort(e) -> "sort"
  | ListReverse(e) -> "reverse"
                                                            
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
