(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SFLit of float
  | SStrLit of string
  | SId of string
  | SListLit of typ * sexpr list
  | SListAccess of sexpr * sexpr
  | SListSlice of sexpr * sexpr * sexpr
  | SUniop of sexpr * op
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sstmt * sexpr * sexpr * sstmt
  | SBindAssign of typ * string * sexpr
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  (* slocals: bind list; *)
  sbody: sstmt list;
}

type sprogram = sstmt list * sfunc_def list



(* Pretty-printing functions *)
let rec string_of_slist = function
    [] -> ""
  | hd :: tl -> string_of_sexpr hd ^ ", " ^ string_of_slist tl
and string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SFLit(l) -> string_of_float l
      | SStrLit(s) -> s
      | SId(s) -> s
      | SListLit(_, lst) -> "[ " ^ string_of_slist lst ^ " ]"
      | SListAccess(e1, e2) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]"
      | SListSlice(e1, e2, e3) -> 
        string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ " : " ^ string_of_sexpr e3 ^ "]"

      | SUniop(e, o) -> string_of_sexpr e ^ " " ^ string_of_op o
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SNoexpr -> "Noexpr" 
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(s1, e2, e3, s) -> "For ( " ^ string_of_sstmt s1 ^ "; " ^
                           string_of_sexpr e2 ^ "; " ^
                           string_of_sexpr e3 ^ ")\n" ^
                           string_of_sstmt s
  | SBindAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^
                            string_of_sexpr e ^ "\n"
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (stmts, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_sstmt stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
