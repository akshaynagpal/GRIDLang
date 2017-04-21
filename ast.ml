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

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | String_Lit(s) -> s
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "string"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
