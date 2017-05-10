(* Ocamllex scanner for Grid compiler *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LARRAY }
| ']'      { RARRAY }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '&'      { REF }
| '.'      { DOT }
| '<'      { LT }
| "<--"    { INARROW }
| "-->"    { OUTARROW }
| "=="     { EQ }
| "!="     { NEQ }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "%"      { MODULO }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "string" { STRING }
| "bool"   { BOOL }
| "None"   { NULL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "Grid_Init"   { GRIDINIT }
| "Grid"   { GRID }
| "Player" { PLAYER }
| "Piece"   { PIECE }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '"'([^'"']* as lxm)'"' { STRING_LIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
