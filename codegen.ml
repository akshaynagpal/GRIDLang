module L = Llvm
module A = Ast

module StringMap = Map.Make(String)
module S = String

let internal_if_flag = ref 0

let translate (globals, functions, structs) =
  let context = L.global_context () in
  let the_module = L.create_module context "GridLang"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context 
  and ptr_t  = L.pointer_type (L.i8_type (context))
  and array_t   = L.array_type in
  let str_t = L.pointer_type i8_t in

  let new_global_builder = ref (L.builder context) in
  (*add all struct names to a hashtable*)
  let struct_types:(string, L.lltype) Hashtbl.t = Hashtbl.create 50 in
  (*Create a reverse hashtable as well (necessary for looking up name from struct type for triggers*)
  let struct_names:(L.lltype,string) Hashtbl.t = Hashtbl.create 50 in
  let add_empty_named_struct_types sdecl =
    let struct_t = L.named_struct_type context sdecl.A.sname in
    let _ = Hashtbl.add struct_types sdecl.A.sname struct_t in
    Hashtbl.add struct_names struct_t sdecl.A.sname 
  in

  let _  =  List.map add_empty_named_struct_types structs in
  
  (*struct can be of any type. For e.g. struct book, struct car. Hashtable will return the type by using sname *)
  let rec ltype_of_typ = function
     A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t 
    | A.StructType s ->  Hashtbl.find struct_types s
    | A.String -> str_t 
    | A.Array1DType (typ,size) -> array_t (ltype_of_typ typ) size 
    | A.PointerType t -> L.pointer_type (ltype_of_typ t) 
    | A.Array2DType (typ,size1,size2) -> array_t (array_t (ltype_of_typ typ) size2) size1 
    | A.CoordinateType -> array_t i32_t 2 (*Declare struct of type coordinate_t here*)
  in
  let populate_struct_type sdecl = 
    let struct_t = Hashtbl.find struct_types sdecl.A.sname in (* get struct by sname*)
    let type_list = List.map (fun(t, _) -> ltype_of_typ t) sdecl.A.sformals in (*construct list of all datatypes of formals in struct*)
    let type_list_with_loc = Array.of_list((ltype_of_typ A.CoordinateType)::type_list) in
    L.struct_set_body struct_t type_list_with_loc true (*finally build struct body in llvm*)
  in 
    ignore(List.map populate_struct_type structs); (*apply populate_struct func on all the structs to build them all*)

  let string_option_to_string = function
    None -> ""
    | Some(s) -> s
  in
  
  (*
    struct_field_index is a map where key is struct name and value is another map
    this second map, the key is the field name and the value is the index number
    basically index every field of struct so that they can be accessed later on
  *)
  
  let struct_field_index_list =
    let handle_list m individual_struct = 
      let struct_field_name_list = List.map snd individual_struct.A.sformals in (*list of all fieldnames of a struct*)
      let struct_field_name_with_loc_list = "location"::struct_field_name_list in
      let increment n = n + 1 in
      (*add each field and index to second map called struct_field_map*)
      let add_field_and_index (m, i) field_name = (StringMap.add field_name (increment i) m, increment i) in 
      let struct_field_map =   List.fold_left add_field_and_index (StringMap.empty, -1) struct_field_name_with_loc_list in
      (*add struct_field_map to the main map*)
      StringMap.add individual_struct.A.sname (fst struct_field_map) m  
    in
    List.fold_left handle_list StringMap.empty structs  
  in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare built-in prompt() function *)
  let prompt_t = L.function_type str_t [||] in
  let prompt_func = L.declare_function "prompt" prompt_t the_module in

  let print_endline_t = L.function_type i32_t [||] in
  let print_endline_func = L.declare_function "print_endline" print_endline_t the_module in

  let print_sameline_t = L.function_type i32_t [| str_t |] in
  let print_sameline_func = L.declare_function "print_sameline" print_sameline_t the_module in

  let diceThrow_num_gen_t = L.function_type i32_t [||] in
  let diceThrow_num_gen_func = L.declare_function "diceThrow" diceThrow_num_gen_t the_module in

  let main_func_map = StringMap.add "initialSetup" "main" StringMap.empty in

  (* Define each struct function (arguments and return type) so we can call it *)
  let struct_function_decls =
    let function_decl m sdecl =
      let fdecl = sdecl.A.sfunc in
        let name = sdecl.A.sname ^ fdecl.A.fname and
        formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) in 
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty structs in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let get_formal_types formal =
        let (t, _) = formal in
        match t with
          A.Array1DType (typ,size) -> L.pointer_type (ltype_of_typ t)
        | A.Array2DType (typ, size1, size2) -> L.pointer_type (ltype_of_typ t)
        | A.StructType s -> L.pointer_type (ltype_of_typ t)
        | _ -> ltype_of_typ t
      in
      let name = try StringMap.find fdecl.A.fname main_func_map with Not_found -> fdecl.A.fname
                 and formal_types = Array.of_list (List.map get_formal_types fdecl.A.formals) in 
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in  

  (* Fill in the body of the given function *)
  let build_function_body the_function fdecl =


    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "mmt" builder in

    let vars_local:(string, L.llvalue) Hashtbl.t = Hashtbl.create 1000 in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal (t, n) p = L.set_value_name n p;
        match t with
          A.Array1DType (typ, size) -> Hashtbl.add vars_local n p
          | A.Array2DType (typ, size1, size2) -> Hashtbl.add vars_local n p
          | A.StructType s -> Hashtbl.add vars_local n p
          | _ -> let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);Hashtbl.add vars_local n local
      in

      let add_local (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
      in 
      Hashtbl.add vars_local n local_var 
    in

    let _ = List.iter2 add_formal fdecl.A.formals (Array.to_list (L.params the_function)) in
        List.map add_local fdecl.A.locals 
    in
    let local = L.build_alloca i32_t "repeat" builder in
    let _ = Hashtbl.add vars_local "repeat" local in 

    (* Return the value for a variable or formal argument *)
    let lookup n = Hashtbl.find vars_local n in

    let lookup_at_index s index builder=
       L.build_in_bounds_gep (lookup s) (Array.of_list [L.const_int i32_t 0; index]) "name" builder in

    let lookup_at_2d_index s index1 index2 builder=
      L.build_in_bounds_gep (lookup s) (Array.of_list [L.const_int i32_t 0; index1; index2]) "name" builder in
    
    let build_1D_array_access array_name i1 index builder isAssign = 
      if isAssign
        then L.build_gep (lookup array_name) [| i1;index|] array_name builder
      else
        L.build_load (L.build_gep (lookup array_name) [|i1;index|] array_name builder) array_name builder
    in 
        (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
      | None -> ignore (f builder) in  

    (* This function has only been created to handle nested structs. It returns the left expr e.g. (book.page).x. If we do not need nested*)
    (*structs in GRIDLang, we would remove this.*)
  let rec llvalue_expr_getter builder = function
     A.Id s -> lookup s
    | A.ArrIndexLiteral (s, e) ->  let index = expr builder e in lookup_at_index s index builder
    | A.Arr2DIndexLiteral(s,e1,e2) -> let index1 = expr builder e1 and index2 = expr builder e2 in lookup_at_2d_index s index1 index2 builder

    | A.Dotop(e1, field) ->  (*e1 is b and field is x in b.x where local decl is struct book b*)
      (match e1 with
        A.Id s -> let etype = fst( 
          try List.find (fun t->snd(t)=s) fdecl.A.locals with 
          |Not_found -> List.find (fun t->snd(t)=s) fdecl.A.formals
          |Not_found -> raise (Failure("Unable to find" ^ s ^ "in dotop"))
        )
        in
        (*above three lines we have found the type of b, which is book*)
        (try match etype with
            A.StructType t->
              let index_number_list = StringMap.find t struct_field_index_list in
              let index_number = StringMap.find field index_number_list in  (*now using field, we find the field's(x) index number for book*)
              let struct_llvalue = lookup s in (*return the value of x*)
              let access_llvalue = L.build_struct_gep struct_llvalue index_number "dotop_terminal" builder in
              access_llvalue (*not sure what this last step is for*)
            | _ -> raise (Failure("No structype.")) with Not_found -> raise (Failure("unable to find" ^ s)) 
        )
        | _ as e1_expr ->  
          let e1'_llvalue = llvalue_expr_getter builder e1_expr in (*This is also for handling nested structs*)
          let loaded_e1' = expr builder e1_expr in
          let e1'_lltype = L.type_of loaded_e1' in
          let e1'_struct_name_string_option = L.struct_name e1'_lltype in
          let e1'_struct_name_string = string_option_to_string e1'_struct_name_string_option in
          let index_number_list = StringMap.find e1'_struct_name_string struct_field_index_list in
          let index_number = StringMap.find field index_number_list in
          let access_llvalue = L.build_struct_gep e1'_llvalue index_number "gep_in_dotop" builder in access_llvalue 
      )

    | A.Unop(op, e)  ->
      (match op with
        A.Deref ->
          let e_llvalue = (llvalue_expr_getter builder e) in
          let e_loaded = L.build_load e_llvalue "loaded_deref" builder in 
          e_loaded
        |_ -> raise (Failure("nooo"))
      )
    |_ -> raise (Failure ("in llvalue_expr_getter but not a dotop!"))

  and expr builder = function
    A.Literal i -> L.const_int i32_t i
    | A.Null t -> L.const_pointer_null (ltype_of_typ(A.PointerType(A.StructType(t))))
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.Coordinate_Lit(x, y) -> let x' = expr builder x and y' = expr builder y in
    (*Create array literal with [x,y] here*) L.const_array (ltype_of_typ(A.Int)) (Array.of_list [x';y'])
    | A.Id s -> L.build_load (lookup s) s builder
    | A.GridCreate (rows, cols) ->  let str_typ = A.StructType("listNode") in
                              let arr_type = A.Array2DType (str_typ, rows, cols) in
                              let grid_val = L.build_alloca (ltype_of_typ arr_type) "Grid" builder in
                              let _ = Hashtbl.add vars_local "Grid" grid_val in
                              (*First do a simple assign for the struct at index 0, 0)*)
                              ignore(
                              for x = 0 to rows do
                              for y = 0 to cols do
                                let cur_struct_ptr = A.Arr2DIndexLiteral("Grid",A.Literal(x),A.Literal(y)) in (*grid[0][0]*)
                                expr builder (A.Assign(A.Dotop(cur_struct_ptr,"next"), A.Null("listNode")))
                              done
                              done);grid_val

    | A.GridAssign (e1, e2, s) ->
                            
                            let struct_llvalue = expr builder (A.Id(s)) in 
                            let struct_type = L.type_of struct_llvalue in
                            let struct_name = Hashtbl.find struct_names struct_type in
                            let rule_func_name = struct_name ^ "rule" in
                            let rule_val = L.build_alloca (ltype_of_typ A.Int) "rule" builder in
                            let _ = Hashtbl.add vars_local "rule" rule_val in 
                            let func_call = A.Call(rule_func_name, [A.Coordinate_Lit(A.Literal(-1),A.Literal(-1));A.Coordinate_Lit(e1,e2)]) in 
                            ignore(expr builder (A.Assign(A.Id("rule"), func_call)));
                            let predicate = A.Binop(A.Id("rule"),A.Equal,A.Literal(1)) in
                            let then_body = A.Expr (A.Call("addToGrid", [A.Id("parray"); e1; e2; A.Id(s);])) in
                            let else_body = A.Block[] in
                            let current_builder = stmt builder (A.If(predicate,then_body,else_body)) in 
                            ignore(internal_if_flag:=1);
                            new_global_builder := current_builder; struct_llvalue

    | A.DeletePlayer (e1, e2, s) -> expr builder (A.Call("deleteFromGrid", [A.Id("parray"); e1; e2; A.Id(s)]));
    
    | A.Dotop(e1, field) -> let e' = expr builder e1 in
      (match e1 with
        A.Id s -> let etype = fst( 
          try List.find (fun t->snd(t)=s) fdecl.A.locals with
          |Not_found -> List.find (fun t->snd(t)=s) fdecl.A.formals
          |Not_found -> raise (Failure("Unable to find" ^ s ^ "in dotop")))
          in
          (try match etype with
            A.StructType t->
              let index_number_list = StringMap.find t struct_field_index_list in
              let index_number = StringMap.find field index_number_list in
              let struct_llvalue = lookup s in
              let access_llvalue = L.build_struct_gep struct_llvalue index_number "dotop_terminal" builder in
              let loaded_access = L.build_load access_llvalue "loaded_dotop_terminal" builder in
              loaded_access  
            | A.PointerType t-> let e_loaded = L.build_load e' "loaded_deref" builder in
              let e1'_lltype = L.type_of e_loaded in
              let e1'_struct_name_string_option = L.struct_name e1'_lltype in
              let e1'_struct_name_string = string_option_to_string e1'_struct_name_string_option in
              let index_number_list = StringMap.find e1'_struct_name_string struct_field_index_list in
              let index_number = StringMap.find field index_number_list in
              let access_llvalue = L.build_struct_gep e' index_number "dotop_terminal" builder in
              let loaded_access = L.build_load access_llvalue "loaded_dotop_terminal" builder in
              loaded_access
            | _ -> raise (Failure("No structype."))
            with Not_found -> raise (Failure("unable to find" ^ s)) 
          )
        | _ as e1_expr ->  let e1'_llvalue = llvalue_expr_getter builder e1_expr in
        let loaded_e1' = expr builder e1_expr in
        let e1'_lltype = L.type_of loaded_e1' in
        let e1'_struct_name_string_option = L.struct_name e1'_lltype in
        let e1'_struct_name_string = string_option_to_string e1'_struct_name_string_option in
        let index_number_list = StringMap.find e1'_struct_name_string struct_field_index_list in
        let index_number = StringMap.find field index_number_list in
        let access_llvalue = L.build_struct_gep e1'_llvalue index_number "gep_in_dotop" builder in
        L.build_load access_llvalue "loaded_dotop" builder 
      )

    | A.Unop(op, e) -> 
      let e' = expr builder e in
      (match op with
        A.Neg     -> L.build_neg e' "tmp" builder
            | A.Not     -> L.build_not e' "temp" builder
        | A.Deref -> let e_loaded = L.build_load e' "loaded_deref" builder in
          e_loaded
        | A.Ref -> let e_llvalue = (llvalue_expr_getter builder e) in
          e_llvalue
      )
    | A.Assign (lhs, e2) -> let e2' = expr builder e2 in  (*we have combined all the assign with match statements. So this method works for x = 1 and book.x = 1 both*)
      (match lhs with
        | A.Array1DAccess (array_name, i, v) -> let addr = (let index = expr builder i in lookup_at_index array_name index builder) 
                                                and value = expr builder v in
                                                ignore(L.build_store value addr builder); value
        (*Check type of v and then use that to call that "type name" ^ "rule"*)
        | A.Array2DAccess(array_name, i,j,v) -> let addr = (let index1 = expr builder i and index2 = expr builder j in lookup_at_2d_index array_name index1 index2 builder) 
                                                and value = expr builder v in
                                                ignore(L.build_store value addr builder); value
        |A.Id s ->ignore (L.build_store e2' (lookup s) builder); e2'
        
        |A.Dotop (e1, field) -> let e' = expr builder e1 in
          (match e1 with
            A.Id s -> let e1typ = fst(
              try List.find (fun t -> snd(t) = s) fdecl.A.locals
              with Not_found -> try List.find (fun t -> snd(t) = s) fdecl.A.formals
              with Not_found ->raise(Failure("unable to find" ^ s ^ "in Sassign"))
            )
            in
            (match e1typ with
              A.StructType t -> (try 
                let index_number_list = StringMap.find t struct_field_index_list in
                let index_number = StringMap.find field index_number_list in
                let struct_llvalue = lookup s in
                let access_llvalue = L.build_struct_gep struct_llvalue index_number field builder in
                (try (ignore(L.build_store e2' access_llvalue builder);e2')
                  with Not_found -> raise (Failure("unable to store " ^ t )) 
                )
                with Not_found -> raise (Failure("unable to find" ^ s)) )

              | A.PointerType t -> let e_loaded = L.build_load e' "loaded_deref" builder in
              let e1'_lltype = L.type_of e_loaded in
              let e1'_struct_name_string_option = L.struct_name e1'_lltype in
              let e1'_struct_name_string = string_option_to_string e1'_struct_name_string_option in
              let index_number_list = StringMap.find e1'_struct_name_string struct_field_index_list in
              let index_number = StringMap.find field index_number_list in
              let access_llvalue = L.build_struct_gep e' index_number field builder in
              (try (ignore(L.build_store e2' access_llvalue builder);e2')
                with Not_found -> raise (Failure("unable to store error" ))
              )
              | _ -> raise (Failure("StructType not found."))
            )
            |_ as e1_expr -> let e1'_llvalue = llvalue_expr_getter builder e1_expr in 
            let loaded_e1' = expr builder e1_expr in
            let e1'_lltype = L.type_of loaded_e1' in
            let e1'_struct_name_string_option = L.struct_name e1'_lltype in
            let e1'_struct_name_string = string_option_to_string e1'_struct_name_string_option in
            let index_number_list = StringMap.find e1'_struct_name_string struct_field_index_list in
            let index_number = StringMap.find field index_number_list in
            let access_llvalue = L.build_struct_gep e1'_llvalue index_number "gep_in_Sassign" builder in
            let _ = L.build_store e2' access_llvalue builder in
            e2'
          )
        |A.Unop(op, e)  ->
          (match op with
            A.Deref ->
              let e_llvalue = (llvalue_expr_getter builder e) in
              let e_loaded = L.build_load e_llvalue "loaded_deref" builder in 
              let _ = L.build_store e2' e_loaded builder in
              e2' 
            |_ -> raise (Failure("nooo"))
          )
          |_ -> raise (Failure("can't match in assign"))
      )
    | A.Array1DAccess (array_name, i, v) -> let addr = (let index = expr builder i in lookup_at_index array_name index builder) 
                                                and value = expr builder v in
                                                ignore(L.build_store value addr builder); value
    | A.Array2DAccess(array_name, i,j,v) -> let addr = (let index1 = expr builder i and index2 = expr builder j in lookup_at_2d_index array_name index1 index2 builder) 
                                                and value = expr builder v in
                                                (*All of the below assumes struct type*)
                                                let loc = expr builder (A.Dotop(v, "location")) in (*Need to do something with this*)
                                                let type_of_value = L.type_of value in
                                                let struct_name = Hashtbl.find struct_names type_of_value in
                                                let rule_func_name = struct_name ^ "rule" in
                                                let _ = expr builder (A.Call(rule_func_name, [])) in
                                                ignore(L.build_store value addr builder); value  
    | A.String_Lit(s) -> L.build_global_stringptr s "name" builder
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
    	  | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder
      
    | A.ArrAssign (s, ie, e2) -> let addr = (let index = expr builder ie in lookup_at_index s index builder) 
                                 and value = expr builder e2 in
                                 ignore(L.build_store value addr builder); value
    | A.ArrIndexLiteral (s, e) ->  let index = expr builder e in L.build_load (lookup_at_index s index builder) "name" builder
    | A.Arr2DIndexLiteral(s,e1,e2) -> let index1 = expr builder e1 and index2 = expr builder e2 in L.build_load(lookup_at_2d_index s index1 index2 builder) "name" builder
    | A.ArrayLiteral (s) -> L.const_array (ltype_of_typ(A.Int)) (Array.of_list (List.map (expr builder) s))
    | A.Binop (e1, op, e2) ->
      (* Construct code for an expression; return its value *)
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
          | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
        let e' = expr builder e in
        (match op with
          A.Neg     -> L.build_neg
          | A.Not     -> L.build_not
        ) e' "tmp" builder     
    | A.Call ("print", [e]) -> 
      (match e with 
        A.Id s-> let etype = fst( 
                  try List.find (fun t->snd(t)=s) fdecl.A.locals with 
                  |Not_found -> List.find (fun t->snd(t)=s) fdecl.A.formals
                  |Not_found -> raise (Failure("Unable to find" ^ s ^ "in expr A.ID"))
                 )
                 in
                (match etype with
                  A.CoordinateType -> let x = L.build_load (lookup_at_index s (expr builder (A.Literal(0))) builder) "name" builder
                                      and y = L.build_load (lookup_at_index s (expr builder (A.Literal(1))) builder) "name" builder
                                      in let _ = L.build_call printf_func [| int_format_str ; x|] "printf" builder in
                                      L.build_call printf_func [| int_format_str ; y|] "printf" builder
                  | _ ->
                  let e' = expr builder e in
                  if (L.type_of e' = i32_t || L.type_of e' = i1_t) then 
                    L.build_call printf_func [| int_format_str ; (expr builder e) |]
                    "printf" builder
                  else
                    L.build_call printf_func [| str_format_str ; (expr builder e) |]
                    "printf" builder
                )
        | _ ->
          let e' = expr builder e in
            if (L.type_of e' = i32_t || L.type_of e' = i1_t) then 
              L.build_call printf_func [| int_format_str ; (expr builder e) |]
              "printf" builder
          else
            L.build_call printf_func [| str_format_str ; (expr builder e) |]
            "printf" builder
      )

    | A.Call ("prompt", []) ->
        L.build_call prompt_func [||] "prompt" builder
    
    | A.Call ("print_endline", []) ->
        L.build_call print_endline_func [||] "print_endline" builder
    
    | A.Call ("print_sameline", [e]) ->
        L.build_call print_sameline_func [| expr builder e |] "print_sameline" builder
    
    | A.Call ("diceThrow", []) ->
        L.build_call diceThrow_num_gen_func [||] "diceThrow" builder
    
    | A.Call (f, act) ->
        let (fdef, fdecl_called) = try StringMap.find f function_decls with Not_found -> StringMap.find f struct_function_decls in
        let map_arguments actual =
          match actual with
          A.Id s -> let etype = fst( 
            try List.find (fun t->snd(t)=s) fdecl.A.locals with
            |Not_found -> List.find (fun t->snd(t)=s) fdecl.A.formals
            |Not_found -> raise (Failure("Unable to find" ^ s ^ "in map_arguments ID")))
            in
            (match etype with
              A.Array1DType (typ,size)-> llvalue_expr_getter builder (actual)
              | A.Array2DType (typ, size1, size2) -> llvalue_expr_getter builder (actual)
              | A.StructType s -> llvalue_expr_getter builder (actual)
              | _ -> expr builder actual)
          | _ -> expr builder actual
          in 
        let actuals = List.rev (List.map map_arguments (List.rev act)) in
        let result = (match fdecl_called.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") 
        in L.build_call fdef (Array.of_list actuals) result builder
    | _ -> raise(Failure("Expr builder failed"))
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    and stmt builder arg_to_match =
    let builder =
    if !internal_if_flag = 1 then
      !new_global_builder
    else builder
    in
    ignore(internal_if_flag := 0);
    match arg_to_match with
      A.Block sl -> List.fold_left stmt builder sl
    | A.Expr e -> ignore (expr builder e); builder
    | A.If (predicate, then_stmt, else_stmt) ->
      let bool_val = expr builder predicate in
      let merge_bb = L.append_block context "merge" the_function in
      let then_bb = L.append_block context "then" the_function in
      add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
      (L.build_br merge_bb);
       let else_bb = L.append_block context "else" the_function in
       add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
       (L.build_br merge_bb);
       ignore (L.build_cond_br bool_val then_bb else_bb builder);       
       L.builder_at_end context merge_bb

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
    | A.Return e -> if (fdecl.A.fname = "gameloop") then (*Assign value of checkGameEnd to "repeat"*)
                    ignore(expr builder (A.Assign(A.Id("repeat"),A.Call ("checkGameEnd", []))))
                    else
                    ignore (match fdecl.A.typ with
                      A.Void -> L.build_ret_void builder
                      | _ -> L.build_ret (expr builder e) builder); builder
    in


    (* Build the code for each statement in the function *)
    let builder = if (fdecl.A.fname = "gameloop") then
                  let _ = ignore(expr builder (A.Assign(A.Id("repeat"),A.Literal(0)))) in 
                  stmt builder (A.While(A.Binop(A.Id("repeat"),A.Equal,A.Literal(0)), A.Block fdecl.A.body))
                  else stmt builder (A.Block fdecl.A.body) 
                in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
                            A.Void -> L.build_ret_void
                            | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in


(*Build the function bodies for struct functions*)
let the_struct_function_list =
  let get_struct_func_decls sdecl =
    let fdecl = sdecl.A.sfunc in
      let (the_function, _) = StringMap.find (sdecl.A.sname ^ fdecl.A.fname) struct_function_decls
    in the_function
  in List.map get_struct_func_decls structs
in
List.iter2 build_function_body the_struct_function_list (List.map (fun (sdecl) -> sdecl.A.sfunc) structs);
(*Build the function bodies*)
let the_function_list = 
  let get_func_decls fdecl =
    let (the_function, _) = 
      try StringMap.find fdecl.A.fname function_decls with Not_found -> StringMap.find "main" function_decls
    in the_function
  in
  List.map get_func_decls functions
in
List.iter2 build_function_body the_function_list functions;

 
the_module