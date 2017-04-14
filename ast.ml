type typ = 
        Int 
        | Bool 
        | Void 
        | String 
        | Array1DType of typ * int  (* int[m] *)
        | CoordinateType
        | Array2DType of typ * int * int  (* int[m][n] *)
        | StructType of string 
        | PointerType of typ

type op = Add 
          | Sub 
          | Mult 
          | Div 
          | Equal 
          | Neq 
          | Less 
          | Leq 
          | Greater 
          | Geq 
          | And 
          | Or

type uop = Neg | Not | Deref | Ref

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | ArrIndexLiteral of string * expr
  | Arr2DIndexLiteral of string * expr * expr
  | Coordinate_Lit of expr * expr
  | Call of string * expr list
  | Id of string
  | Binop of expr * op * expr
  | Dotop of expr * string
  | Unop of uop * expr
  | Assign of expr * expr
  | Array1DAccess of string * expr * expr (* assigning some value to an array *)
  | Array2DAccess of string * expr * expr * expr (* assigning some value to a 2D array *)
  | String_Lit of string
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
