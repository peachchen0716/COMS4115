(* Ocamllex scanner for Pyni *)

{
  open Pyniparser 

  let strip_quotes s = 
    match String.length s with
  | 0 | 1 | 2 -> ""
  | l -> String.sub s 1 (l - 2)
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
(* match string that doesn't contain quotation or non escaped character *)
let stringlit = ('"' ([^'\\''"'] | '\\'_)* '"')

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
(* Comments *)
| '#'      { comment lexbuf }
| "\"\"\"" { multcomment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQUA }
| ']'      { RSQUA }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| "++"     { INCRE }
| "--"     { DECRE }
| "%"      { MOD }
| '*' 	   { TIMES }
| '/'      { DIVIDE } 
| '='      { ASSIGN }
| '"'      { QUOTE }
| "def"    { DEF }
| "=="     { EQ }
| "!="     { NEQ }
| '!'      { NOT }
| '<'      { LT }
| '>'      { GT }
| "<="     { LTE }
| ">="     { GTE }
| "and"     { AND }
| "or"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR } 
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "True"   { BLIT(true)  }
| "False"  { BLIT(false) }
| "float"  { FLOAT }
| "str"    { STRING }
| "None"   { NONE }
| "list"   { LIST }
| "len"    { LEN }
| "append" { LIST_APPEND }
| "pop"    { LIST_POP }
| "insert" { LIST_INSERT }
| "sort"   { LIST_SORT }
| "reverse" { LIST_REVERSE }
| "-"? digit+ as lem { LITERAL(int_of_string lem) }
| "-"? digit+ '.' digit* as f { FLIT(float_of_string f) }
| "-"? digit* '.' digit+ as f { FLIT(float_of_string f) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| stringlit as lem { STRLIT(strip_quotes lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "\n" { token lexbuf }
| _    { comment lexbuf }

and multcomment = parse
  "\"\"\""  { token lexbuf }
  | _         { multcomment lexbuf }
