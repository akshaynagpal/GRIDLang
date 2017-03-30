(* Abstract Syntax Tree and functions for printing it *)
type typ = Int | Bool | Void | String

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | Call of string * expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | String_Lit of string
  | Assign of string * expr
  | Noexpr
  
type alltyp =
    PrimitiveType of typ * string
  | ArrayType of typ * string * int

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = alltyp list * func_decl list
