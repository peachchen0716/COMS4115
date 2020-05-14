/* Ocaml parser for Pyni */

%{
open Ast
%}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE PLUS MINUS TIMES DIVIDE ASSIGN
%token LSQUA RSQUA INCRE DECRE MOD NOT
%token QUOTE DEF
%token EQ NEQ LT GT LTE GTE AND OR NOT
/* Right now, no need to support range */
%token FOR 
%token NOELSE
%token IF ELSE WHILE INT BOOL FLOAT STRING
%token RETURN COMMA
%token LEN
%token LIST_APPEND LIST_POP LIST_INSERT LIST_SORT LIST_REVERSE
%token <int> LITERAL
%token <bool> BLIT
%token <float> FLIT
%token <string> STRLIT
%token <string> ID
%token NONE
%token LIST
%token EOF

%start program
%type <Ast.program> program

%nonassoc ID
%nonassoc NOELSE
%nonassoc ELSE
%nonassoc LSQUA 
%nonassoc RSQUA
%nonassoc LEN
%nonassoc LIST_APPEND LIST_POP LIST_INSERT LIST_SORT LIST_REVERSE

%left INCRE DECRE NOT
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MOD

%%

program:
    decls EOF { $1 }

decls:
    { ([], []) }
    | stmt decls { (($1 :: fst $2), snd $2) }
    | fdecl decls { (fst $2, ($1 :: snd $2)) }

typ:
    INT             { Int }
  | BOOL            { Bool }
  | FLOAT           { Float }
  | STRING          { String }
  | LIST LT typ GT  { List($3) }

fdecl:
    DEF ID LPAREN formals_opt RPAREN COLON typ LBRACE stmt_list RBRACE
    {
        {
            rtyp=$7;
            fname=$2;
            formals=$4;
            body=$9;
        }
    }

formals_opt:
    { [] }
    | formals_list { $1 }

formals_list:
    vdecl { [$1] }
    | vdecl COMMA formals_list { $1::$3 }

vdecl:
    typ ID { ($1, $2) }

stmt_list:
    { [] }
    | stmt stmt_list { $1::$2 }

stmt:
    expr SEMI                             { Expr $1 }
  | LBRACE stmt_list RBRACE               { Block $2 }
  | IF LPAREN expr RPAREN stmt %prec NOELSE     
                                          { If ($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt  { If ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt         { While ($3, $5) }
  | FOR LPAREN stmt expr SEMI expr_opt RPAREN stmt      
                                          { For ($3, $4, $6, $8) }
  | typ ID ASSIGN expr SEMI               { BindAssign($1, $2, $4) }
  | RETURN expr SEMI                      { Return $2 }
  | LIST_APPEND LPAREN ID COMMA expr RPAREN SEMI
                                          { ListAppend($3, $5) }
  | LIST_INSERT LPAREN expr COMMA expr COMMA  expr RPAREN
                                          { ListInsert($3, $5, $7) }
  | LIST_SORT LPAREN expr RPAREN          { ListSort($3) }
  | LIST_REVERSE LPAREN expr RPAREN       { ListReverse($3) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | BLIT             { BoolLit($1) }
  | FLIT             { FLit($1) }
  | STRLIT           { StrLit($1) }
  | NONE             { Noexpr }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add, $3) }
  | expr MINUS  expr { Binop($1, Sub, $3) }
  | expr TIMES  expr { Binop($1, Mult, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq, $3) }
  | expr LT     expr { Binop($1, Less, $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr LTE    expr { Binop($1, LessEq, $3) } 
  | expr GTE    expr { Binop($1, GreaterEq, $3) } 
  | expr AND    expr { Binop($1, And, $3) }
  | expr OR     expr { Binop($1, Or, $3) }
  | expr INCRE       { Uniop($1, Incre) }
  | expr DECRE       { Uniop($1, Decre) }
  | expr MOD    expr { Binop($1, Mod, $3) } 
  | NOT expr         { Uniop($2, Not) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | LSQUA args_opt RSQUA 
                     { ListLit($2) }
  | list_access      { $1 }
  | LEN LPAREN ID RPAREN
                     { Len($3) }
  | LIST_POP LPAREN ID RPAREN 
                     { ListPop($3) }
  | LPAREN expr RPAREN 
                     { $2 }
  | ID LPAREN args_opt RPAREN 
                     { Call ($1, $3) }

list_access:
  | ID LSQUA expr RSQUA { ListAccess($1, $3) }
  | expr LSQUA expr COLON expr RSQUA { ListSlice($1, $3, $5) }

args_opt:
    /* no arg */ { [] }
  | args         { $1 }

args:
    expr { [$1] }
  | expr COMMA args { $1::$3 } 
