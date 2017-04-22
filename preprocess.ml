let tempImportFile = ref ""

let process_files filename1 =
	let read_all_lines file_name =
		let in_channel = open_in file_name in
		let rec read_recursive lines =
			try
			(* issues:
				import file name is forced to be gridBasics.grid 
				can import only one file because global var will get changed
			*)
			let line = input_line in_channel in
	  		if (line = "import gridBasics.grid;") then 
	  			begin
	  				tempImportFile := "gridBasics.grid";
	  				read_recursive ("" :: lines);
	  			end
	  		else read_recursive ( (line ^ "\n") :: lines) 
	  		with
	  		End_of_file ->
	    	lines in
			let lines = read_recursive [] in
			let _ = close_in_noerr in_channel in
			List.rev (lines) in 

	let concat = List.fold_left (fun a x -> a ^ x) "" in 
		let temp = "" ^ concat (read_all_lines filename1) in
			if (!tempImportFile = "") then
				temp
			else
				let importFile = concat (read_all_lines !tempImportFile) in
				let res = temp ^ "" ^ importFile in
				res
				
