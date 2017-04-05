(* Abstract Syntax Tree and functions for printing it *)
type typ = Int | Bool | Void | String | ArrayType of typ * int

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
           And | Or

type uop = Neg | Not

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | ArrElementLit of string * int
  | Call of string * expr list
  | Id of string
  | String_Lit of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | ArrAssign of string * int * expr
  | ArrayLiteral of expr list
  | Noexpr
  
type stmt =
    Block of stmt list
  | Expr of expr
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Return of expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list
