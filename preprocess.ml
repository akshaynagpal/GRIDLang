open Str
open String

let tempImportFile = ref ""
let playerStuctFound = ref false
let braceNotFound = ref false
let doBraceCount = ref false
let braceCounter = ref 0
let itemBraceCounter = ref 0
let ruleInPlayerFound = ref false
let insidePlayerStruct = ref false
let ruleInItemFound = ref false
let insideItemStruct = ref false
let exitingItemStruct = ref false
let checkGameEndFound = ref false
let playerStructBraceCounter = ref 0
let listNode = ref "Item listNode\n{\n"
let triggerRule = ref "int triggerRule(int src_x, int src_y, int dst_x, int dst_y, string typetag)\n{\nint x;\n"
let colocPresent = ref false
let colocBlock = "int colocation(int x, int y, Item listNode* i1, Item listNode* i2)\n{\nreturn 0;\n}\n"
let itemStructBraceFound = ref false
let gameloopFound = ref false
(* let playerOrderFound = ref false *)
let playerOrderIntSize = ref 1

let append_to_string structName = 
	triggerRule := !triggerRule ^ "if (typetag == \"" ^ structName ^"\")\n{\nx = "^structName^"rule(src_x, src_y, dst_x, dst_y);\nreturn x;\n}\n"

let process_files filename1 =
	let contains s1 s2 =
	  let re = Str.regexp_string s2
	  in
	    try ignore (Str.search_forward re s1 0); true
	    with Not_found -> false
	in

	let ruleFunc = "int rule(int src_x, int src_y, int dst_x, int dst_y) {\nreturn 1;\n}\n"
	in

	let defaultItemStructFormals = "bool win;\n" ^ "string displayString;\n" ^ "bool exists;\n"
	in

	let playerStruct = "Player {\nint rule(int src_x, int src_y, int dst_x, int dst_y) {\nreturn 1;\n}\n}\n"
	in

	let read_all_lines file_name =
		let in_channel = open_in file_name in
		let rec read_recursive lines =
			try
				let line = input_line in_channel in
					let _ = 
						if (Str.string_match (Str.regexp "Player\[[0-9]+\] playerOrder;") line 0) then
						(* if ((contains line "playerOrder") && (contains line "Player")) then  *)
							begin
								let pos = Str.search_forward (Str.regexp "[0-9]+") line 0 in
								let nextPos =
									try Str.search_forward (Str.regexp "[0-9]+") line (pos+1) 
										with Not_found -> -1
								in
								if(nextPos <> -1) then
									begin
										let size = ( (((int_of_char (line.[pos])) - 48)*10) + (int_of_char (line.[nextPos])-48)) in
										playerOrderIntSize := size;
									end
								else
									begin
										let size = ( (int_of_char (line.[pos])) - 48 ) in
										playerOrderIntSize := size;
									end
							end
					in
					let _ = 
						if (contains line "gameloop") then gameloopFound := true in
					let _ = 
						if (contains line "checkGameEnd") then checkGameEndFound := true in
					let _ = 
						if (contains line "colocation") then colocPresent := true in
					let _ =
						if (contains line "Item") then
			  			begin
			  				if (contains line ";" = false && (contains line ")" = false)) then
			  					begin
			  						insideItemStruct := true;
			  						if (contains line "{") then
			  							begin
			  								let lis = Str.split (Str.regexp "{") line in
					  						let wordlis = Str.split (Str.regexp " ") (List.hd(lis)) in
					  						let wordArr = Array.of_list(wordlis) in
					  						let structType = wordArr.(0) in
					  						let structName = wordArr.(1) in
					  						let _ = append_to_string structName in
					  						listNode := !listNode ^ structType  ^ " " ^ structName ^ "*" ^ " " ^ structName ^ "_node;\n";	
			  							end
			  						else
			  							begin
			  								let wordlis = Str.split (Str.regexp " ") line in
					  						let wordArr = Array.of_list(wordlis) in
					  						let structType = wordArr.(0) in
					  						let structName = wordArr.(1) in
					  						let _ = append_to_string structName in
					  						listNode := !listNode  ^ structType  ^ " " ^ structName ^ "*" ^ " " ^ structName ^ "_node;\n";
			  							end
			  					end
			  			end
			  		in
		  		if (contains line "import") then 
		  			begin
		  				tempImportFile := "gridBasics.grid";
		  				read_recursive ("" :: lines);
		  			end
		  		else if(contains line "Grid_Init<") then
		  			begin
		  				let modifiedLine = Str.replace_first (Str.regexp ";") " grid;" line in
		  					read_recursive ( (modifiedLine ^ "\n") :: lines);
		  			end
		  		(* adding rule to item struct, start *)
		  		else if (!insideItemStruct = true && (contains line "rule") && (contains line "(")) then
		  			begin
		  				ruleInItemFound := true;
		  				read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		else if (!insideItemStruct = true && (contains line "{")) then
		  			begin
		  				itemBraceCounter := !itemBraceCounter + 1;
		  				if (!itemBraceCounter = 1) then
		  					read_recursive ( (line ^ "\n" ^ defaultItemStructFormals) :: lines)
		  				else
		  					read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		else if (!insideItemStruct = true && (contains line "}")) then
		  			begin
		  				itemBraceCounter := !itemBraceCounter - 1;
		  				if(!itemBraceCounter = 0) then
		  					begin
		  						insideItemStruct := false;
			  					if (!ruleInItemFound = false) then 
			  						begin
			  							read_recursive ( (ruleFunc ^ line ^ "\n") :: lines);
			  						end
			  					else
			  						read_recursive ( (line ^ "\n") :: lines);	
		  					end
		  				else
		  					read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		(* adding rule to item struct, end *)
		  		(* adding gameloop to initialSetup, start *)
		  		else if (contains line "setup()") then
		  			begin
		  				doBraceCount := true;
		  				braceCounter := if (contains line "{") then	(!braceCounter) + 1 else !braceCounter;
		  				if(!braceCounter = 1) then
		  					begin
		  						read_recursive ( (line ^ "\n") :: lines);
		  					end
		  				else
		  					begin
		  						read_recursive ( (line ^ "\n") :: lines);
		  					end
		  			end
		  		else if ((!doBraceCount = true) && (contains line "{")) then
		  			begin
		  				braceCounter := !braceCounter + 1;
		  				if(!braceCounter = 1) then
		  					begin
		  						read_recursive ( (line ^ "\n") :: lines);
		  					end
		  				else
		  					begin
		  						read_recursive ( (line ^ "\n") :: lines);
		  					end
		  				(* read_recursive ( (line ^ "\n") :: lines); *)
		  			end
		  		else if (contains line "return" && !doBraceCount && !braceCounter = 1) then
		  			begin
		  				doBraceCount := false;
		  				read_recursive ( ( "playerOrderSize = " ^ string_of_int(!playerOrderIntSize) ^ ";\n" ^ "gameloop();\n" ^ line ^ "\n" ) :: lines);
		  			end
		  		else if (!doBraceCount && contains line "}") then
		  			begin
		  				braceCounter := !braceCounter - 1;
		  				read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		(* adding gameloop to initialSetup, end *)
		  		(* adding rule to player struct, start *)
		  		else if (!playerStuctFound = true && !insidePlayerStruct = true && (contains line "rule") ) then 
		  			begin
		  				ruleInPlayerFound := true;
		  				read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		else if (!playerStuctFound = true && !insidePlayerStruct = true && (contains line "}") ) then 
		  			begin
		  				playerStructBraceCounter := !playerStructBraceCounter - 1;
		  				if(!playerStructBraceCounter = 0) then 
		  					begin
		  						insidePlayerStruct := false;
		  						if (!ruleInPlayerFound = false) then
		  							begin
		  								read_recursive ( (ruleFunc ^ "\n" ^ line ^ "\n") :: lines);
		  							end
		  						else
		  							begin
		  								read_recursive ( (line ^ "\n") :: lines);
		  							end
		  					end
		  				else
		  					begin
		  						read_recursive ( (line ^ "\n") :: lines);
		  					end
		  			end
		  		(* adding rule to player struct, end *)
		  		else if (!playerStuctFound = true && !braceNotFound = true && !insidePlayerStruct = true && (contains line "{") ) then
		  			begin
		  				braceNotFound := false;
		  				insidePlayerStruct := true;
		  				playerStructBraceCounter := !playerStructBraceCounter + 1;
		  				read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		else if (contains line "Player") then
		  			begin
		  				if ( (contains line "{") && (contains line ";" = false) && (contains line ")" = false) ) then
		  					begin
		  						playerStuctFound := true;
		  						insidePlayerStruct := true;
		  						playerStructBraceCounter := !playerStructBraceCounter + 1;
		  						let lis = Str.split (Str.regexp "{") line in
		  						let wordlis = Str.split (Str.regexp " ") (List.hd(lis)) in
		  						let wordArr = Array.of_list(wordlis) in
		  						let structType = wordArr.(0) in
									(* listNode := !listNode ^ "*" ^ structType ^ " " ^ structType ^ "_node;\n";*)
										read_recursive ( (line ^ "\n") :: lines);
		  					end
		  				else if (contains line ";" = false && (contains line ")" = false)) then
		  					begin
		  						playerStuctFound := true;
		  						insidePlayerStruct := true;
		  						let wordlis = Str.split (Str.regexp " ") line in
		  						(* let wordlis = Str.split (Str.regexp " ") (List.hd(lis)) in *)
		  						let wordArr = Array.of_list(wordlis) in
		  						let structType = wordArr.(0) in
									(* listNode := !listNode ^ "*" ^ structType ^ " " ^ structType ^ "_node;\n"; *)
		  						braceNotFound := true;
		  						read_recursive ( (line ^ "\n") :: lines);
		  					end
		  				else
		  					begin
		  						read_recursive ( (line ^ "\n") :: lines);
		  					end
		  			end
		  		else read_recursive ( (line ^ "\n") :: lines)
	  	with
	  		End_of_file ->
	    	lines in
			let lines = read_recursive [] in
			let _ = close_in_noerr in_channel in
			List.rev (lines) in 

	let concat = List.fold_left (fun a x -> a ^ x) "" in 
	let playerOrderSize = "int playerOrderSize;\n" in
	let listNodeContent = "int x;\nint y;\nstring type;\nItem listNode* next;\nstring nametag;
	\nstring typetag;\nPlayer* owner;\nint rule(int src_x, int src_y, int dst_x, int dst_y) {\nreturn 1;\n}\n}\n" in
	let temp =  !listNode ^ listNodeContent ^ "\n" ^ playerOrderSize ^ concat (read_all_lines filename1) in
	let temp1 = if(!colocPresent = false) then colocBlock ^ "\n" ^ temp else temp in
	let temp11 = 
		if(!checkGameEndFound = false) then
			let checkGameEndFunc = "\nint checkGameEnd()\n{\nreturn 1;\n}\n\n" in
				temp1 ^ checkGameEndFunc;
		else
			temp1
	in
	let temp12 = 
		if(!gameloopFound = false) then
			let gameloopFunc = "int gameloop(){\nreturn 0;\n}\n\n" in
				temp11 ^ gameloopFunc;
		else
				temp11
	in
	let tempFinal = temp12 in
	let temp2 = 
		if (!tempImportFile = "") then
				begin
					if(!playerStuctFound = false) then
						let res = playerStruct ^ "" ^ tempFinal in
							res
					else
						tempFinal
				end
			else
				if(!playerStuctFound = false) then
					let res = playerStruct ^ "" ^ tempFinal in
					let importFile = concat (read_all_lines !tempImportFile) in
					let finalRes = res ^ "" ^ importFile in
					finalRes
				else
					let importFile = concat (read_all_lines !tempImportFile) in
					let res = tempFinal ^ "" ^ importFile in
						res
	in
  let temp2 = temp2 ^ !triggerRule ^ "}" in
  temp2