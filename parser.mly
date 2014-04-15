%token EOL
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
%token <string> BINOP
%token RED
%token GREEN
%token BLUE
%token ARR
%token COMMA
%token QUOTE
%token <int> NUM
%token PAINT
%token <string> IDE
%token LET
%token EQ
%start main
%type <Dmc.Syntax.syntax> main
%%
main:
 | PAINT color_const formula eol {Dmc.Syntax.PAINT ($2,$3)} 
 | LET IDE EQ formula eol {Dmc.Syntax.LET ($2,[],$4)}
 | LET IDE formalarglist EQ formula eol {Dmc.Syntax.LET ($2,$3,$5)} 
 | RESET eol {Dmc.Syntax.RESET}
  ;
  eol:
 | EOL {}
  ;
  formula:
 | LPAREN formula RPAREN {$2}
 | TRUE {Dmc.Syntax.TRUE}
 | FALSE {Dmc.Syntax.FALSE}
 | IDE {Dmc.Syntax.CALL ($1,[])}
 | IDE actualarglist {Dmc.Syntax.CALL ($1,$2)}
 | LBRACKET expr RBRACKET {Dmc.Syntax.PROP $2}
 | NOT formula {Dmc.Syntax.NOT $2}
 | formula AND formula {Dmc.Syntax.AND ($1,$3)}
 | formula OR formula {Dmc.Syntax.OR ($1,$3)}
 | CLOS formula {Dmc.Syntax.CLOS $2}
 | INT formula {Dmc.Syntax.INT $2}
 | formula UNTIL formula {Dmc.Syntax.UNTIL ($1,$3)}
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
  expr:
 | color_const {Dmc.Syntax.COLOR $1}
 | value BINOP value {Dmc.Syntax.BINOP ($1,$2,$3)}
 | value EQ value {Dmc.Syntax.BINOP ($1,"=",$3)}
  ;
  color_const:
 | QUOTE IDE QUOTE {$2}
  ;
  value:
 | RED {Dmc.Syntax.RED}
 | GREEN {Dmc.Syntax.GREEN}
 | BLUE {Dmc.Syntax.BLUE}
 | NUM {Dmc.Syntax.NUM $1}

