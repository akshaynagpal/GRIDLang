(*Abstract Syntax Tree for GRID *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not  (*Unary operators*)

type typ = Int | Bool | String     (*Data types*)

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

type program = bind list * func_decl list

(*Block here is for writing a piece of code in {}. We need to decide how we want our language to be. Python style or Java style?*)