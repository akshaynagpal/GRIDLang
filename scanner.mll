(* Ocamllex scanner for GRID *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "and"     { AND }
| "or"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "boolean"   { BOOLEAN }
| "float"   { FLOAT }
| "string"   { STRING }
| "true"   { TRUE }
| "false"  { FALSE }
| "function" { FUNCTION }
| "player"  { PLAYER }
| "item"  { ITEM }
| "grid"  { GRID }
| "coordinate"  { COORDINATE }
| "colocation"  { COLOCATION }
| "gameloop"  { GAMELOOP }
| "checkMove"  { CHECKMOVE }
| "drawGrid"  { DRAWGRID }
| "rand"  { RAND }
| "gameOver"  { GAMEOVER }
| "createGrid"  { CREATEGRID }
| "traverse"  { TRAVERSE }
| "checkGameEnd"  { CHECKGAMEEND }
| "type"  { TYPE }
| "repeat"  { REPEAT }
| "print"  { PRINT }
| "prompt"  { PROMPT }
| "playerOrder"  { PLAYERORDER }
| "P"  { P }
| "LAYOUT"  { LAYOUT }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
