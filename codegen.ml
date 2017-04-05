module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "GridLang"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context 
  and array_t   = L.array_type in
  let str_t = L.pointer_type i8_t in
  (*and string_i8 = L.string_of_lltype[L.i8_type] context in*)

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t 
    | A.String -> str_t 
    | A.ArrayType (typ,size) -> array_t i32_t size in 

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let main_func_map = StringMap.add "gameloop" "main" StringMap.empty in
  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = try StringMap.find fdecl.A.fname main_func_map with Not_found -> fdecl.A.fname
      and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = try StringMap.find fdecl.A.fname function_decls with Not_found -> StringMap.find "main" function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "mmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
  ignore (L.build_store p local builder);
  StringMap.add n local m in

      let add_local m (t, n) =
  let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = StringMap.find n local_vars in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
  A.Literal i -> L.const_int i32_t i
  | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
  | A.Id s -> L.build_load (lookup s) s builder
  | A.Assign (s, e) -> let e' = expr builder e in
                     ignore (L.build_store e' (lookup s) builder); e'
    | A.ArrAssign (s, i, e) -> let e' = expr builder e in
                    ignore (L.build_store e' (L.build_in_bounds_gep (lookup s) (Array.of_list [L.const_int i32_t 0; L.const_int i32_t i]) "name" builder) builder); e'
    | A.ArrayLiteral (s) -> L.const_array (ltype_of_typ(A.Int)) (Array.of_list (List.map (expr builder) s))
  | A.String_Lit(s) -> L.build_global_stringptr s "name" builder
  | A.ArrElementLit (s, i) ->  L.build_load (L.build_in_bounds_gep (lookup s) (Array.of_list [L.const_int i32_t 0; L.const_int i32_t i]) "name" builder) "name" builder
  | A.Binop (e1, op, e2) ->
     let e1' = expr builder e1
     and e2' = expr builder e2 in
     (match op with
       A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mult    -> L.build_mul
      | A.Div     -> L.build_sdiv
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Equal   -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Less    -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Greater -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
      let e' = expr builder e in
      (match op with
        A.Neg     -> L.build_neg
            | A.Not     -> L.build_not) e' "tmp" builder     
    (*
    When we encounter a call with the id being print this pattern gets matched.
    Now we take e, evaluate it by calling expr and store it in e'.
    If the type of e' is an int then we call printf_func with int_format_str
    (equivalent to %d) and expr builder e (which simply recomputes e)
    Else we assume it to be a string and pass str_format_str (equivalent to %s)
    to printf_func.
    Rather than using else we need to use else if and match type of e' to a string,
    I'm not sure how to do that 
    *)
    | A.Call ("print", [e]) -> 
      let e' = expr builder e in
      if (L.type_of e' = i32_t ) then 
        L.build_call printf_func [| int_format_str ; (expr builder e) |]
        "printf" builder
      else 
        L.build_call printf_func [| str_format_str ; (expr builder e) |]
        "printf" builder
    
    (*
    match e with
    A.Literal i -> 
    L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder
    | A.String_Lit(s) ->
    L.build_call printf_func [| str_format_str ; (expr builder e) |]
      "printf" builder
    *)
    (*
    let e' = expr builder e in
    if (e' = i32_t ) then 
    *)
    
    (*
    | A.Call ("printStr", [e]) ->
    L.build_call printf_func [| str_format_str ; (expr builder e) |]
      "printStr" builder
    *)
    | A.Call (f, act) ->
      let (fdef, fdecl) = StringMap.find f function_decls in
   let actuals = List.rev (List.map (expr builder) (List.rev act)) in
   let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
  Some _ -> ()
      | None -> ignore (f builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
  A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.While (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);
          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (stmt (L.builder_at_end context body_bb) body) (L.build_br pred_bb);
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
      | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
      | A.Return e -> ignore (match fdecl.A.typ with
    A.Void -> L.build_ret_void builder
  | _ -> L.build_ret (expr builder e) builder); builder
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
