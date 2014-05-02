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
%start main
%type <Csmc.PictureLogic.syntax> main
%%
main:
 | PAINT color formula eol {Csmc.PictureLogic.PAINT (Csmc.Picture.COL $2,$3)} 
 | LET IDE EQ formula eol {Csmc.PictureLogic.LET ($2,[],$4)}
 | LET IDE formalarglist EQ formula eol {Csmc.PictureLogic.LET ($2,$3,$5)} 
 | RESET eol {Csmc.PictureLogic.RESET}
  ;
  eol:
 | EOL {}
  ;
  formula:
 | LPAREN formula RPAREN {$2}
 | TRUE {Csmc.PictureLogic.TRUE}
 | FALSE {Csmc.PictureLogic.FALSE}
 | IDE {Csmc.PictureLogic.CALL ($1,[])}
 | IDE actualarglist {Csmc.PictureLogic.CALL ($1,$2)}
 | LBRACKET prop RBRACKET {Csmc.PictureLogic.PROP $2}
 | NOT formula {Csmc.PictureLogic.NOT $2}
 | formula AND formula {Csmc.PictureLogic.AND ($1,$3)}
 | formula OR formula {Csmc.PictureLogic.OR ($1,$3)}
 | CLOS formula {Csmc.PictureLogic.CLOS $2}
 | CLOS HAT NUM formula {Csmc.PictureLogic.CLOSN ($3,$4)}
 | INT formula {Csmc.PictureLogic.INT $2}
 | formula UNTIL formula {Csmc.PictureLogic.UNTIL ($1,$3)}
  ;
  formalarglist:
    LPAREN innerformalarglist RPAREN {$2}
  ;
  innerformalarglist:
 | IDE {[$1]}
 | IDE COMMA innerformalarglist {$1::$3}
  ;
  actualarglist:
    LPAREN inneractualarglist RPAREN {$2}
  ;
  inneractualarglist:
 | formula {[$1]}
 | formula COMMA inneractualarglist {$1::$3}
  ;
  prop:
 | color {Csmc.Picture.COL $1}
 | UNOP color {Csmc.Picture.UNOP ($1,$2)}
 | value BINOP value {Csmc.Picture.BINOP ($1,$2,$3)}
 | value EQ value {Csmc.Picture.BINOP ($1,"=",$3)} 
  ;
  color:
 | QUOTE IDE QUOTE {Csmc.Picture.COLOR $2}
 | HASH NUM NUM NUM {Csmc.Picture.RGB ($2,$3,$4)}
  ;
  value:
 | RED {Csmc.Picture.RED}
 | GREEN {Csmc.Picture.GREEN}
 | BLUE {Csmc.Picture.BLUE}
 | NUM {Csmc.Picture.NUM $1}

