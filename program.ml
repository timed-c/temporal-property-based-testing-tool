open Tfg 
open Printf


exception Match_failure_log  of string

type vertex_degree =
{
	vertx : (vertex_id * vertex);
    outdegree : int;
    indegree : int;
}

type edge_weight =
{
     edge : (vertex_id * vertex_id);
	 weight : int option;
} 
 
type tfg_weighted = 
{
	wnodes : vertex_degree list;
	wedges : edge_weight list;
} 

let count = ref 0

let resolution = "ms"

let get_vertex_id v = fst v

let get_outdgree v vdlist = 
	let v1 = (List.find (fun a -> (get_vertex_id a.vertx) = v) vdlist) in
	v1.outdegree 

let get_indgree v vdlist = 
	let v1 = (List.find (fun a -> (get_vertex_id a.vertx) = v) vdlist) in
	v1.indegree

let compute_vertex_degree p elist = 
	let olst = List.filter (fun (a,b) -> if a = p then true else false) elist in 
	let outgoing_edges = List.length olst in 
	let ilst = List.filter (fun (a,b) -> if b = p then true else false) elist in 
	let incoming_edges = List.length ilst in 
	(outgoing_edges, incoming_edges)

let rec compute_vertex_list vlist elist vdlist =
	match vlist with
	| h :: rst -> let vid = get_vertex_id h in
				  let (odegree, idegree) = compute_vertex_degree vid elist in
				  let vdlist = {vertx=h; outdegree=odegree; indegree=idegree} :: vdlist in
				  compute_vertex_list rst elist vdlist 
	| [] -> vdlist 

let rec compute_edge_list elist edlist = 
	match elist with 
	| h::rst -> if ((fst h) = 0) then compute_edge_list rst ({edge = h; weight=Some(0)} :: edlist) else compute_edge_list rst ({edge = h; weight=None} :: edlist)
	| [] -> edlist 


let rec print_vertex_list vlist = 
   match vlist with 
   | v :: rst -> print_int (get_vertex_id v.vertx); print_string " : outdgree = "; print_int v.outdegree; 
				 print_string " : indgree = "; print_int v.indegree; print_string "\n"; print_vertex_list rst
   | [] -> ()


let print_list lst = List.iter (fun a -> print_int a; print_string " ") lst	

(*
let rec visit_adj alist elist vlist path =
	match alist with 
	| (v1, v2):: rst -> let _ = print_string "visit_adj"; print_int v2 ; print_string "\n" in 	
						if ((List.exists (fun a -> a = v2) vlist)) then 
							visit_adj rst elist vlist path 						
						else
						let (vlist, path)  = dfs elist vlist path v2 in visit_adj rst elist vlist path 
	|[] -> (vlist, path)
and dfs elist vlist path v =
	(*let _ = print_string "dfs"; print_int v ; print_string "\n" in*)
	let vlist = v :: vlist in
	let path = v :: path in
	let alist = List.filter (fun (v1, v2) -> if (v1 = v) then true else false) elist in
    let (vlist, path) = (visit_adj alist elist vlist path) in
	(vlist, path)
 

let rec print_log vlist wlist elist [] =
	match vlist with
	|h::rst -> let t = List.exists (fun a -> a = h) wlist in 
			   let path = if (t = true )  else print_while h rst in
			
	| [] -> ()
				
		


let rec path_find elist alist vlist v =
	match alist with 
	| (v1, v2)::rst -> let (vlist, path) = dfs elist vlist [] v2 in
					   let cycle_exists = List.exists (fun a -> if (a = v) then true else false) path in
					   if (cycle_exists) then path else path_find elist rst vlist v 

	| [] -> [] 
					   
let rec find_cycle vlist elist  =
	match vlist with
	|h::rst ->  let vid = (get_vertex_id h.vertx) in
				let _ = print_string "find_cycle"; print_int vid; print_string "\n" in
				let alist = List.filter (fun (v1, v2) -> if (v1 = vid) then true else false) elist in
				let cycle = path_find elist alist [] vid in
				let _ = print_string "cycle ";print_list (List.rev cycle) ; print_string "\n" in
				cycle :: (find_cycle rst elist)
	| [] -> []*) 




(***let rec visit_adj alist elist vlist ve =
	match alist with 
	| (v1, v2):: rst -> if ((List.exists (fun a -> a = v2) vlist)) then 
							visit_adj rst elist vlist ve					
						else
						let vlist = find_path v2 ve elist vlist in visit_adj rst elist vlist ve 
	|[] -> vlist
and find_path vs ve elist vlist = 
	if (vs <> ve && (vs <> 11) && (vs <> 12)) then
	begin 
	    let vlist = vs :: vlist in
		let alist = List.filter (fun (v1, v2) -> if (v1 = vs) then true else false) elist in
		let vlist = visit_adj alist elist vlist ve in 
		vlist 
	end 
	else
		List.rev (vs :: vlist)

let rec concat path upath num =
	if (num <> 0) then
	begin
		let npath = List.append path upath in 
		concat path npath (num-1)
	end
	else
		upath
	

(*
	begin
		let upath = concat path [] 2 in print_list upath 
	end
    else 
		(print_list path; nxt_path (last_element+1) last_element elist 2 )
	
*)

(*let rec call_log elist v ve =
	print_int v; 
	if (v >  ve) then ()
	else 
	begin 
	let alist = List.filter (fun (v1, v2) -> if (v1 = v) then true else false) elist in
	let len = List.length alist in
	if (len = 1) then 
		(let nxt = (snd (List.hd (alist))) in
		call_log elist nxt ve )
	else
		(let nxt = snd (List.find (fun (a,b) -> b <> v+1) alist) in 
		print_path (v+1) v elist 2; call_log elist nxt ve)
	end *)

let rec call_log elist v ve =
	print_string "call_log "; print_int v; print_string "\n";
	if (v >= ve) then []
	else 
	begin 
	let alist = List.filter (fun (v1, v2) -> if (v1 = v) then true else false) elist in
	let len = List.length alist in
	if (len = 1) then 
		(let nxt = (snd (List.hd (alist))) in
		call_log elist nxt ve )
	else
		(let nxt = snd (List.find (fun (a,b) -> b <> v+1) alist) in 
		let _ = nxt_path (v+1) v elist 2 ve in 
		call_log elist nxt ve)
	end 
and nxt_path v1 v2 elist num ve =
	let _ = print_string "print_path"; print_int v1; print_int v2; print_string "\n" in
	let path = find_path v1 v2 elist [] in
	let last_element = List.hd (List.rev path) in
	let upath = 
	if (last_element = v2) then 
	begin
	  concat path [] 2 
	end
	else
	begin 
	  List.append path (call_log elist last_element ve)
	end
	in print_list upath 
 
***)

let init_tfg_weighted vlist elist t =
	let vdlist = compute_vertex_list vlist elist [] in 
	let edlist = compute_edge_list elist [] in
	(*let _ = find_cycle ((List.filter (fun a -> if ((a.outdegree = 2) && (a.indegree =2)) then true else false)) vdlist) elist in
	let while_list = List.map (fun a -> get_vertex_id a) (List.filter (fun a -> if ((a.outdegree = 2) && (a.indegree =2)) then true else false) vdlist) in 
    let (vlist, path) = dfs (List.rev elist) [] [] 0 in 
	print_log vlist while_list; *)
	print_vertex_list (List.rev vdlist);
	{wnodes = vdlist; wedges= edlist}


let print_block v s =
	match s with
	|Exp -> print_int v; print_string ";\n"
	|If -> print_string "If("; print_int v; print_string "){\n"
	|Else -> print_string "}\n else{\n"
	|End -> print_int v; print_string ";}\n"
	|While ->  print_string "While("; print_int v; print_string "){\n"
	|Infinite ->  print_string "While("; print_int v; print_string "){\n" 

let print_statement_stdio v =
	match (snd v) with
	|Timingpoint(p)->  print_block (fst v) p.tstm 
	|Fragment(f) -> print_block (fst v) f.cstm


let print_log_program v s oc tc frag=
	let frmtr = "%d\\n\"" in  
	match s with
	|Exp -> let _ = fprintf oc "printf(\"S:%s, %d);\n" frmtr v; fprintf tc "printf(\"S:%s, %d);\n" frmtr v in
			 (if (frag = true) then fprintf tc "code_fragment%d();\n" v) 
	|If -> let c = Random.int 2 in (fprintf oc "if(%d){\n" c; fprintf oc "printf(\"S:%s, %d);\n" frmtr v); fprintf tc "if(%d){\n" c
	|Else -> fprintf oc "}\n else{\n"; fprintf oc "printf(\"S:%s, %d);\n" frmtr v; fprintf tc "}\n else{\n"
	|End -> fprintf oc "printf(\"S:%s, %d);\n}" frmtr v; fprintf tc "}\n" 
	|While ->  let c = Random.int 10 in 
			   let _ = count := !count + 1  in
			   let var = "var"^(string_of_int !count) in
			   fprintf oc "int %s=0;\n while(%s < %d){%s++;\n" var var c var; fprintf oc "printf(\"S:%s, %d);\n" frmtr v;
			   fprintf tc "int %s=0;\n while(%s < %d){%s++;\n" var var c var; 
	|Infinite -> fprintf oc "while(1){\n"; fprintf oc "printf(\"S:%s, %d);\n" frmtr v; fprintf tc "while(1){\n"


let exit_instrumentation knd abs_arr rel_arr rel_dead oc tc=
	let ffrmtr = "%d" in 
	match knd with 
	|Soft -> fprintf oc "printf(\"E:%s:inf\\n\", %d);\n" ffrmtr abs_arr; 
			 fprintf tc "stp(%d, %d, ms); \n tp_exit(); \n" rel_arr rel_dead 
	|Firm -> fprintf oc "printf(\"E:%s:inf\\n\", %d);\n" ffrmtr abs_arr;
			 fprintf tc "ftp(%d, %d, ms); \n tp_exit(); \n" rel_arr rel_dead 

let entry_instrumentation knd rel_dead abs_arr oc tc =
	let ffrmtr = "%d" in 
	let _ = fprintf tc "tp_entry(); \n" in 
	match knd with 
	|Soft -> fprintf oc "printf(\"A:%s:inf\\n\", %d);\n" ffrmtr abs_arr 
	|Firm -> fprintf oc "printf(\"A:%s:%s\\n\", %d, %d);\n" ffrmtr ffrmtr abs_arr (abs_arr + rel_dead) 

let get_timedc_construct v =
    match v with
	|Timingpoint(p) -> (match (p.kind) with
						|Soft -> Stp
						|Firm -> Ftp)
	|Fragment(p) -> (match (p.critical) with
					 |true -> Critical 
					 |false -> Fragment)

						
let print_statement v oc tc abs_arrival =
	let tv = (snd v) in 
	match tv with
	|Timingpoint(p)->  let rel_arr = tA tv in
					   let rel_dead = tD tv in
					   let _ = entry_instrumentation (k tv) rel_dead abs_arrival oc tc in 
					   let _ = print_log_program (fst v) p.tstm oc tc false in
					   let upd_abs = rel_arr + abs_arrival in
					   exit_instrumentation (k tv) upd_abs rel_arr rel_dead oc tc; upd_abs 
	|Fragment(f) -> (match f.critical with
					|true -> fprintf tc "critical{\n" ; (print_log_program (fst v) f.cstm oc tc true); fprintf tc "}\n"
					|false -> (print_log_program (fst v) f.cstm oc tc true));abs_arrival


let generate_timingpoint_construct v lfile tcfile abs_arrival = 
	let vrtx = snd v in
	let vid = fst v in
	let tp = get_timedc_construct vrtx in
	let newline = "\\n" in
	match tp with
	|Stp -> let entry_time = abs_arrival in
			let exit_time = abs_arrival + tA vrtx in 
			fprintf lfile "printf(\"STP#%d %d-inf %d-inf %s\");\n" vid entry_time exit_time newline;
			fprintf tcfile "long randm_log_entry_time = entry();\n stp(%d, %d, %s);\n exit(%d, randm_log_entry_time);\n" (tA vrtx) (tD vrtx) resolution vid
	|Ftp -> let entry_time_min  = abs_arrival in
			let entry_time_max  = abs_arrival + (tD vrtx) in
			let exit_time_min = abs_arrival + (tA vrtx) in 
			let exit_time_max = abs_arrival + (tA vrtx) + (tD vrtx) in
			fprintf lfile "printf(\"FTP#%d %d-%d %d-%d %s\");\n" vid entry_time_min entry_time_max exit_time_min exit_time_max newline;
			fprintf tcfile "long randm_log_entry_time = entry();\n stp(%d, %d, %s);\n exit(%d, randm_log_entry_time);\n" (tA vrtx) (tD vrtx) resolution vid
	| _ ->  raise (Match_failure_log("generate_timingpoint_construct not a tp"))  



let generate_c_construct vid s lfile tcfile =
	let newline = "\\n" in
	match s with
	|Exp ->  fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline;
			 fprintf tcfile "code_fragment%d(); \n" vid 
	|If ->   let _ = count := !count + 1  in
		     fprintf lfile "int var%d = rand()%%2;\n if(var%d){\n printf(\"Frag#%d %s\");\n" !count !count vid newline;
		     fprintf tcfile "int var%d = rand()%%2;\n if(var%d){\n printf(\"Frag#%d %s\");\n" !count !count vid newline 
	|Else -> fprintf lfile "}\n else {\n printf(\"Frag%d %s\");" vid newline;
			 fprintf tcfile "}\n else{\n printf(\"Frag%d %s\"); \n code_fragment%d();" vid newline vid  
	|End ->  fprintf lfile "}\n printf(\"Frag#%d %s\");\n" vid newline;
			 fprintf tcfile "}\n printf(\"Frag#%d %s\");\n code_fragment%d();\n" vid newline vid 
	|While-> let _ = count := !count + 1  in
			 let wvar = "var"^(string_of_int !count) in 
			 let limit = (Random.int 10) + 1 in
			 let _ = count := !count + 1  in
			 let svar = "var"^(string_of_int !count) in 
		   	 fprintf lfile "int %s = rand() %% %d;\n" wvar limit; 
			 fprintf lfile "int %s = 0; \n while(%s < %s){ %s++; \n printf(\"Frag#%d %s\");\n" svar svar wvar svar vid newline;
			 fprintf tcfile "int %s = rand() %% %d;\n" wvar limit; 
			 fprintf tcfile "int %s = 0; \n while(%s < %s){ %s++; \n printf(\"Frag#%d %s\");\n" svar svar wvar svar vid newline
	|Infinite -> fprintf lfile "while(1){\n printf(\"Frag#%d %s\");\n" vid newline; 
				 fprintf tcfile "while(1){\n printf(\"Frag#%d %s\");\n" vid newline

let generate_fragment v cs lfile tcfile =
	let vid = fst v in
	let cf = get_timedc_construct (snd v) in
	let newline = "\\n" in
	match cf with
	|Critical -> fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline;
				 fprintf tcfile "critical{\n printf(\"Frag#%d %s\"); \n code_fragment%d(); \n }\n" vid  newline vid
	|Fragment -> generate_c_construct vid cs lfile tcfile 
	| _ ->  raise (Match_failure_log("generate_fragment not a fragment"))   
 
let generate_log_execution_program v lfile tcfile abs_arrival =
	let vrtx = snd v in
	match vrtx with
	|Timingpoint(p) -> let upd_arr = abs_arrival + (tA vrtx) in (generate_timingpoint_construct v lfile tcfile abs_arrival); upd_arr
	|Fragment(f) ->	generate_fragment v f.cstm lfile tcfile; abs_arrival 




let rec tfg_log_generator vlist vend nde oc tc abs_arr =
	match nde with 
	|num when num = vend -> ()
	| _ -> let v = List.find (fun (a,b) -> if (a=nde) then true else false) vlist in
		   (*print_statement_stdio v; 
		   let abs_arr = print_statement v oc tc abs_arr in *)
			let abs_arr =  generate_log_execution_program v oc tc abs_arr in
	       tfg_log_generator vlist vend (nde+1) oc tc abs_arr


