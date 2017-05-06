
type action = Ast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);  (* Print the AST only *)
            ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
            ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  (* print_string (Preprocess.process_files Sys.argv.(2) ) *)
  let lexbuf = Lexing.from_string (Preprocess.process_files Sys.argv.(2) ) in
  let ast = Parser.program Scanner.token lexbuf in
    match action with
  LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
print_string (Llvm.string_of_llmodule m)