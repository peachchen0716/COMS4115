open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Pyniparser.program Scanner.token lexbuf in
  print_endline "success" 
