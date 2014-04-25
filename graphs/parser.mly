%token EOL
%token HASH
%token RESET
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token TRUE
%token FALSE
%token AND
%token OR
%token NOT
%token CLOS
%token INT
%token UNTIL
%token <string> UNOP
%token <string> BINOP
%token RED
%token GREEN
%token BLUE
%token ARR
%token HAT
%token COMMA
%token QUOTE
%token <int> NUM
%token PAINT
%token <string> IDE
%token LET
%token EQ
%token CHECK
%start main
%type <Csmc.GraphLogic.syntax> main
%%
main:
 | CHECK formula EOL {Csmc.GraphLogic.CHECK $2} 
 | LET IDE EQ formula EOL {Csmc.GraphLogic.LET ($2,[],$4)}
 | LET IDE formalarglist EQ formula EOL {Csmc.GraphLogic.LET ($2,$3,$5)} 
  ;
  formula:
 | LPAREN formula RPAREN {$2}
 | TRUE {Csmc.GraphLogic.TRUE}
 | FALSE {Csmc.GraphLogic.FALSE}
 | IDE {Csmc.GraphLogic.CALL ($1,[])}
 | IDE actualarglist {Csmc.GraphLogic.CALL ($1,$2)}
 | LBRACKET prop RBRACKET {Csmc.GraphLogic.PROP $2}
 | NOT formula {Csmc.GraphLogic.NOT $2}
 | formula AND formula {Csmc.GraphLogic.AND ($1,$3)}
 | formula OR formula {Csmc.GraphLogic.OR ($1,$3)}
 | CLOS formula {Csmc.GraphLogic.CLOS $2}
 | CLOS HAT NUM formula {Csmc.GraphLogic.CLOSN ($3,$4)}
 | INT formula {Csmc.GraphLogic.INT $2}
 | formula UNTIL formula {Csmc.GraphLogic.UNTIL ($1,$3)}
  ;
  formalarglist:
    LPAREN innerformalarglist RPAREN {$2}
  ;
  innerformalarglist:
 | IDE {[$1]}
 | IDE COMMA innerformalarglist {$1::$3}
  ;
  actualarglist:
 | formula {[$1]}
 | formula COMMA actualarglist {$1::$3}
  ;
  prop:
 | IDE {$1}
  ;

