type typ = 
        Int 
        | Bool 
        | Void 
        | String 
        | CoordinateType
        | ArrayType of typ * int  (* int[m] *)
        | Array2DType of typ * int * int  (* int[m][n] *)
        | StructType of string 
        | PointerType of typ

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
           And | Or

type uop = Neg | Not | Deref | Ref


type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | ArrIndexLiteral of string * expr
  | ArrIndexRef of string * expr
  | Call of string * expr list
  | Id of string
  | Binop of expr * op * expr
  | Dotop of expr * string
  | Unop of uop * expr
  | String_Lit of string
  | Assign of expr * expr
  | CoordinateAssign of string * expr * expr
  | ArrAssign of string * expr * expr  (* assigning some value to an array *)
  | ArrayLiteral of expr list   (* list inside array *)
  | Noexpr
  | Repeat                (* added repeat keyword in expr*)
  
type stmt =
    Block of stmt list
  | Expr of expr
  | For of expr * expr * expr * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
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
