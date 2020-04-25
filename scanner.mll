(* Ocamllex scanner for Pyni *)

{ open Pyniparser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
(* left right square bracket *)
| '['      { LSQUA }
| ']'      { RSQUA }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
(* increment & decrement *)
| "++"     { INCRE }
| "--"     { DECRE }
(* mod *)
| "%"      { MOD }
| '*' 	   { TIMES }
| '/'      { DIVIDE } 
| '='      { ASSIGN }
| '"'      { QUOTE }
| "def"    { DEF }
| "=="     { EQ }
| "!="     { NEQ }
(* not *)
| '!'      { NOT }
| '<'      { LT }
| '>'      { GT }
(* LTE & GTE *)
| "<="     { LTE }
| ">="     { GTE }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
(* for *)
| "for"    { FOR } 
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "float"  { FLOAT }
(* list *)
| "list"   { LIST }
| digit+ as lem  { LITERAL(int_of_string lem) }
| digit+ '.' digit* as f { FLIT(float_of_string f) }
| digit* '.' digit+ as f { FLIT(float_of_string f) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
