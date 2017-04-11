(* Abstract Syntax Tree and functions for printing it *)
type typ = Int | Bool | Void | String | StructType of string (*added new data types*)

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
  | Dotop of expr * string
  | Unop of uop * expr
  | String_Lit of string
  | Assign of expr * expr
  | StructRef of string
  | Noexpr
  | Repeat                (* added repeat keyword in expr*)
  
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

type struct_decl = {   (* for adding player datatype *)
    sname: string;
    sformals: bind list;

}

type program = bind list * func_decl list * struct_decl list
