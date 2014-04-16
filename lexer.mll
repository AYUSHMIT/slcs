{
  open Parser
  exception Eof
}
rule token = parse
  [' ' '\t' '\n'] { token lexbuf }    
| "Paint" {PAINT}
| "Let" {LET}
| "Reload" {RESET}
| "RED" {RED}
| "GREEN" {GREEN}
| "BLUE" {BLUE}
| "," {COMMA}
| ";" {EOL}
| "(" {LPAREN}
| ")" {RPAREN}
| "[" {LBRACKET}
| "]" {RBRACKET}
| ['"'] {QUOTE}
| "T" {TRUE}
| "F" {FALSE}
| ['0'-'9']+ as lxm {NUM (int_of_string lxm)}
| "&" {AND}
| "|" {OR}
| "!" {NOT}
| "C" {CLOS}
| "I" {INT}
| "U" {UNTIL}
| "->" {ARR}
| "=" {EQ}
| "#" {HASH}
| "~" {UNOP "~"}
| "^" {HAT}
| ['>' '<' '+' '-' '*' '/']+ as lxm {BINOP lxm}
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as lxm {IDE lxm} 
| eof {raise Eof}



