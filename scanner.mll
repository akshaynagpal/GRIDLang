(* Ocamllex scanner for GRID *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "##"     { comment lexbuf }           (* Comments *)
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
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "boolean"   { BOOLEAN }
| "float"   { FLOAT }
| "string"   { STRING }
| "true"   { TRUE }
| "false"  { FALSE }
| "function" { FUNCTION } (* NOT SURE *)
| "player"  { PLAYER }
| "item"  { ITEM }
| "grid"  { GRID }
| "coordinate"  { COORDINATE }
| "rand"  { RAND }
| "type"  { TYPE }
| "repeat"  { REPEAT }
| "playerOrder"  { PLAYERORDER }
| "P"  { P }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "##" { token lexbuf }
| _    { comment lexbuf }

(*
| "colocation"  { COLOCATION }
| "gameloop"  { GAMELOOP }
| "gameOver"  { GAMEOVER }
| "createGrid"  { CREATEGRID }
| "traverse"  { TRAVERSE }
| "checkGameEnd"  { CHECKGAMEEND }
| "print"  { PRINT }
| "prompt"  { PROMPT }
| "checkMove"  { CHECKMOVE }
| "drawGrid"  { DRAWGRID }
| "LAYOUT"  { LAYOUT }
*)
