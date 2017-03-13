type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | LITERAL of (int)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 41 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* NOT *);
  269 (* EQ *);
  270 (* NEQ *);
  271 (* LT *);
  272 (* LEQ *);
  273 (* GT *);
  274 (* GEQ *);
  275 (* TRUE *);
  276 (* FALSE *);
  277 (* AND *);
  278 (* OR *);
  279 (* RETURN *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* FOR *);
  283 (* WHILE *);
  284 (* INT *);
  285 (* BOOL *);
  286 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  287 (* LITERAL *);
  288 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\007\000\007\000\003\000\008\000\
\008\000\010\000\010\000\010\000\010\000\012\000\012\000\011\000\
\011\000\011\000\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\000\000\002\000\003\000\000\000\
\002\000\002\000\002\000\003\000\003\000\000\000\001\000\001\000\
\001\000\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\032\000\000\000\010\000\011\000\012\000\001\000\
\003\000\004\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\013\000\000\000\000\000\009\000\
\014\000\000\000\000\000\000\000\000\000\016\000\005\000\000\000\
\024\000\000\000\017\000\000\000\000\000\000\000\019\000\000\000\
\000\000\018\000\027\000\021\000\020\000\030\000\000\000\000\000\
\026\000\000\000\031\000"

let yydgoto = "\002\000\
\003\000\004\000\009\000\010\000\011\000\016\000\023\000\027\000\
\017\000\035\000\036\000\000\000\047\000\048\000"

let yysindex = "\255\255\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\228\254\046\255\000\000\022\255\230\254\018\255\
\004\255\000\000\010\255\022\255\000\000\247\254\022\255\000\000\
\000\000\248\254\003\255\026\255\011\255\000\000\000\000\001\255\
\000\000\027\255\000\000\030\255\038\255\007\255\000\000\052\255\
\011\255\000\000\000\000\000\000\000\000\000\000\051\255\049\255\
\000\000\011\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\255\000\000\000\000\
\054\255\000\000\000\000\000\000\000\000\000\000\013\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\255\000\000\000\000\000\000\000\000\000\000\000\000\
\055\255\000\000\000\000\000\000\000\000\000\000\000\000\056\255\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\037\000\000\000\026\000\000\000\000\000\031\000\
\000\000\000\000\243\255\000\000\000\000\000\000"

let yytablesize = 287
let yytable = "\001\000\
\008\000\039\000\029\000\012\000\029\000\018\000\030\000\031\000\
\029\000\020\000\030\000\044\000\029\000\021\000\016\000\037\000\
\016\000\016\000\040\000\025\000\019\000\025\000\024\000\028\000\
\025\000\032\000\013\000\046\000\041\000\032\000\042\000\033\000\
\034\000\033\000\034\000\016\000\051\000\033\000\034\000\015\000\
\043\000\033\000\034\000\016\000\016\000\022\000\013\000\014\000\
\026\000\005\000\006\000\007\000\045\000\049\000\050\000\006\000\
\007\000\028\000\029\000\025\000\038\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\006\000\007\000"

let yycheck = "\001\000\
\000\000\001\001\002\001\032\001\002\001\032\001\004\001\005\001\
\002\001\006\001\004\001\005\001\002\001\004\001\002\001\029\000\
\004\001\005\001\032\000\001\001\003\001\003\001\032\001\032\001\
\006\001\023\001\001\001\041\000\002\001\023\001\001\001\031\001\
\032\001\031\001\032\001\023\001\050\000\031\001\032\001\014\000\
\003\001\031\001\032\001\031\001\032\001\020\000\001\001\002\001\
\023\000\028\001\029\001\030\001\001\001\003\001\006\001\003\001\
\003\001\003\001\003\001\023\000\030\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\028\001\029\001\030\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  VOID\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "parser.mly"
            ( _1 )
# 251 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                 ( [], [] )
# 257 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "parser.mly"
               ( (_2 :: fst _1), snd _1 )
# 265 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 273 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 288 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                  ( [] )
# 294 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "parser.mly"
                  ( List.rev _1 )
# 301 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                             ( [(_1,_2)] )
# 309 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                             ( (_3,_4) :: _1 )
# 318 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
        ( Int )
# 324 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
         ( Bool )
# 330 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
         ( Void )
# 336 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                     ( [] )
# 342 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 62 "parser.mly"
                     ( _2 :: _1 )
# 350 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 65 "parser.mly"
               ( (_1, _2) )
# 358 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                   ( [] )
# 364 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
                   ( _2 :: _1 )
# 372 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
              ( Expr _1 )
# 379 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                ( Return Noexpr )
# 385 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                     ( Return _2 )
# 392 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 75 "parser.mly"
                            ( Block(List.rev _2) )
# 399 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                  ( Noexpr )
# 405 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                  ( _1 )
# 412 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 82 "parser.mly"
                     ( Literal(_1) )
# 419 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                     ( Id(_1) )
# 426 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 84 "parser.mly"
                                 ( Call(_1, _3) )
# 434 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                       ( _2 )
# 441 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                  ( [] )
# 447 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 89 "parser.mly"
                  ( List.rev _1 )
# 454 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                            ( [_1] )
# 461 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                            ( _3 :: _1 )
# 469 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
