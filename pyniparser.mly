/* Ocaml parser for Pyni */

%{
open Ast
%}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE PLUS MINUS TIMES DIVIDE ASSIGN
/* quotation mark, def */
%token QUOTE DEF
%token EQ NEQ LT GT AND OR
/* Right now, no need to support FOR because we don't have definition for list yet */
/* %token FOR */
%token IF ELSE WHILE INT BOOL
/* return, COMMA token */
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS
%left TIMES DIVIDE

%%

program:
    decls EOF { $1 }

decls:
    { ([], []) }
    | stmt decls { (($1 :: fst $2), snd $2) }
    | fdecl decls { (fst $2, ($1 :: snd $2)) }

typ:
    INT { Int }
    | BOOL { Bool }

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
    expr SEMI                               { Expr $1 }
    | LBRACE stmt_list RBRACE               { Block $2 }
    | IF LPAREN expr RPAREN stmt ELSE stmt  {If ($3, $5, $7) }
    | WHILE LPAREN expr RPAREN stmt         { While ($3, $5) }
    | RETURN expr SEMI                      {Return $2}

expr:
      LITERAL          { Literal($1)            }
    | BLIT             { BoolLit($1)            }
    | ID               { Id($1)                 }
    | expr PLUS   expr { Binop($1, Add,   $3)   }
    | expr MINUS  expr { Binop($1, Sub,   $3)   }
    | expr TIMES  expr { Binop($1, Mult,  $3)   }
    | expr DIVIDE expr { Binop($1, Div,   $3)   }
    | expr EQ     expr { Binop($1, Equal, $3)   }
    | expr NEQ    expr { Binop($1, Neq, $3)     }
    | expr LT     expr { Binop($1, Less,  $3)   }
    | expr GT     expr { Binop($1, Greater, $3) }
    | expr AND    expr { Binop($1, And,   $3)   }
    | expr OR     expr { Binop($1, Or,    $3)   }
    | ID ASSIGN expr   { Assign($1, $3)         }
    | typ ID ASSIGN expr { BindAssign($1, $2, $4) }
    | LPAREN expr RPAREN { $2                   }
    | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

args_opt:
    { [] }
    | args { $1 }

args:
    expr { [$1] }
    | expr COMMA args { $1::$3 } 
