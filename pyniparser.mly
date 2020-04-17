/* Ocaml parser for Pyni */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS TIMES DIVIDE ASSIGN
/* left right quotation mark, def */
%token LQUOTE RQUOTE DEF
%token EQ NEQ LT GT AND OR
%token IF ELSE WHILE FOR INT BOOL
/* return, COMMA token */
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF


