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
let playerStructBraceCounter = ref 0
let listNode = ref "Player listNode\n{\n"

let process_files filename1 =
	let contains s1 s2 =
	  let re = Str.regexp_string s2
	  in
	    try ignore (Str.search_forward re s1 0); true
	    with Not_found -> false
	in
	
	let ruleFunc = "int rule(coordinate c1, coordinate c2) {\nreturn 1;\n}\n"
	in

	let defaultPlayerStructFormals = "coordinate pos;\n" ^ "bool win;\n" ^ "string displayString;\n" ^ "bool exists;\n"
	in

	let playerStruct = "Player good {\n" ^ defaultPlayerStructFormals ^ ruleFunc ^ "}\n"
	in

	let read_all_lines file_name =
		let in_channel = open_in file_name in
		let rec read_recursive lines =
			try
				let line = input_line in_channel in
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
					  						listNode := !listNode ^ "*" ^ structType ^ " " ^ structName ^ " " ^ structName ^ "_node;\n";	
			  							end
			  						else
			  							begin
			  								let wordlis = Str.split (Str.regexp " ") line in
					  						let wordArr = Array.of_list(wordlis) in
					  						let structType = wordArr.(0) in
					  						let structName = wordArr.(1) in
					  						listNode := !listNode ^ "*" ^ structType ^ " " ^ structName ^ " " ^ structName ^ "_node;\n";
			  							end
			  					end
			  			end
			  		in
		  		if (contains line "import") then 
		  			begin
		  				tempImportFile := "gridBasics.grid";
		  				read_recursive ("" :: lines);
		  			end
		  		(* adding rule to item struct, start *)
		  		else if (!insideItemStruct = true && (contains line "{")) then
		  			begin
		  				itemBraceCounter := !itemBraceCounter + 1;
		  				read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		else if (!insideItemStruct = true && (contains line "}")) then
		  			begin
		  				itemBraceCounter := !itemBraceCounter - 1;
		  				if(!itemBraceCounter = 0) then 
		  					begin
		  						insideItemStruct := false;
		  						read_recursive ( (ruleFunc ^ line ^ "\n") :: lines);
		  					end
		  				else
		  					read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		(* adding rule to item struct, end *)
		  		(* adding gameloop to initialSetup, start *)
		  		else if (contains line "initialSetup()") then
		  			begin
		  				doBraceCount := true;
		  				braceCounter := if (contains line "{") then	(!braceCounter) + 1 else !braceCounter;
		  				read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		else if ((!doBraceCount = true) && (contains line "{")) then
		  			begin
		  				braceCounter := !braceCounter + 1;
		  				read_recursive ( (line ^ "\n") :: lines);
		  			end
		  		else if (contains line "return" && !doBraceCount && !braceCounter = 1) then
		  			begin
		  				doBraceCount := false;
		  				read_recursive ( ( "gameloop();\n" ^ line ^ "\n" ) :: lines);
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
		  								read_recursive ( (ruleFunc ^ line ^ "\n") :: lines);
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
		  				read_recursive ( (line ^ "\n" ^ defaultPlayerStructFormals) :: lines);
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
		  						let structName = wordArr.(1) in
		  						listNode := !listNode ^ "*" ^ structType ^ " " ^ structName ^ " " ^ structName ^ "_node;\n";
		  						read_recursive ( (line ^ "\n" ^ defaultPlayerStructFormals) :: lines);
		  					end
		  				else if (contains line ";" = false && (contains line ")" = false)) then
		  					begin
		  						playerStuctFound := true;
		  						insidePlayerStruct := true;
		  						let wordlis = Str.split (Str.regexp " ") line in
		  						(* let wordlis = Str.split (Str.regexp " ") (List.hd(lis)) in *)
		  						let wordArr = Array.of_list(wordlis) in
		  						let structType = wordArr.(0) in
		  						let structName = wordArr.(1) in
		  						listNode := !listNode ^ "*" ^ structType ^ " " ^ structName ^ " " ^ structName ^ "_node;\n";
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
		let temp = !listNode ^ "string type;\n*Player listNode next;\n" ^ ruleFunc ^ "}\n" ^ concat (read_all_lines filename1) in
			if (!tempImportFile = "") then
				begin
					if(!playerStuctFound = false) then
						let res = playerStruct ^ "" ^ temp in
							res
					else
						temp
				end
			else
				if(!playerStuctFound = false) then
					let res = playerStruct ^ "" ^ temp in
					let importFile = concat (read_all_lines !tempImportFile) in
					let finalRes = res ^ "" ^ importFile in
					finalRes
				else
					let importFile = concat (read_all_lines !tempImportFile) in
					let res = temp ^ "" ^ importFile in
						res