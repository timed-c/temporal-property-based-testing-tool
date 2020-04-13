open Tfg
open Program
(*open Checker*)
open Printf


let rec print_vertex_list vlist = 
   match vlist with 
   | (vid, Timingpoint(v)) :: rst -> (match v.kind with
						|Soft ->  print_int vid; print_string "[shape=circle];\n"; print_vertex_list rst 
						|Firm ->  print_int vid; print_string "[shape=Mcircle];\n"; print_vertex_list rst)
   | (vid, Fragment(v)) :: rst -> (match v.critical with
						|false -> print_int vid; print_string "[shape=square];\n"; print_vertex_list rst 
						|true ->  print_int vid; print_string "[shape=Msquare];\n"; print_vertex_list rst)

   | [] -> () 

let rec print_edge_list v = 
   match v with 
   | (v1, v2) :: rst -> print_int v1; print_string " -> "; print_int v2; print_string ";\n"; print_string "\n" ; print_edge_list rst  
   | [] -> ()

let print_tfg t seed = 
    print_string "digraph G {\n label= \"seed ="; print_int seed; print_string "\";\nlabelloc=\"t\";\n"; print_vertex_list t.nodes; print_edge_list (List.rev t.edges); print_string "}"


let rec print_vertex_list_file vlist gc = 
   match vlist with 
   | (vid, Timingpoint(v)) :: rst -> (match v.kind with
						|Soft ->  (fprintf gc "%d" vid); (fprintf gc "[shape=circle];\n"); print_vertex_list_file rst gc 
						|Firm ->  fprintf gc  "%d" vid; fprintf gc "[shape=Mcircle];\n"; print_vertex_list_file rst gc)
   | (vid, Fragment(v)) :: rst -> (match v.critical with
						|false -> fprintf gc "%d" vid; fprintf gc  "[shape=square];\n"; print_vertex_list_file rst gc
						|true ->  fprintf gc "%d" vid; fprintf gc "[shape=Msquare];\n"; print_vertex_list_file  rst gc)

   | [] -> () 

let rec print_edge_list_file v gc = 
   match v with 
   | (v1, v2) :: rst -> fprintf gc "%d" v1; fprintf gc  " -> "; fprintf gc "%d" v2; fprintf gc  ";\n"; fprintf gc  "\n" ; print_edge_list_file rst gc
   | [] -> ()

let print_tfg_file t seed = 
	let filename_tfg= "test/tfg_"^(string_of_int seed)^".dot" in
	let gc = open_out filename_tfg in
    fprintf gc "digraph G {\n label= \"seed =";  fprintf gc "%d" seed; fprintf gc "\";\nlabelloc=\"t\";\n"; print_vertex_list_file t.nodes gc; print_edge_list_file (List.rev t.edges) gc; fprintf gc "}"  

let main =
	let _ = Random.self_init () in
	let seed = Random.int(1000000) in
    let seed = 204540 in
	let t = generate_tfg seed in
    let _ = print_int seed; print_string ":" in
	(*let _ = print_tfg t seed in 
	let _ = print_string "seed :"; print_int seed; print_string "\n" in*)
	let _ = print_tfg_file t seed in
	let filename_gen = "test/gen_"^(string_of_int seed)^".c" in
	let filename_timedc= "test/time_"^(string_of_int seed)^".c" in
	let oc = open_out filename_gen in
    let tc = open_out filename_timedc in
    let _ = fprintf oc "#include<stdio.h>\n#include<stdlib.h> \n \nint main(){\n\tsrand(%d);\n\tlong abs_arr = 0;\n" seed in
    let _ = fprintf tc "#include<stdio.h>\n#include<cilktc.h>\n#include\"pbt.h\"\ntask fun(){\n\tstruct timespec testing_tt;\n\tlong testing_et;\n\ttesting_init_time(&testing_tt);\n " in
	let frmtr = "%d\\n\"" in 
    let _ = fprintf oc "\t"; fprintf oc "printf(\"Frag#0 \\n\");\n" in 
	let _ = tfg_log_generator (t.nodes) ((List.length (t.nodes))) 1 oc tc 0  1 in
	let _ = fprintf oc "}" in
    let _ = fprintf tc "}\nint main(){\n\tsrand(%d);\n\tfun();\n}" seed in
	let _ = close_out oc in 
	let _ = close_out tc in 
    () 
	(*let _ = read_tfg_logs "test/tfg_log" in *)  
(*	let wt = init_tfg_weighted (t.nodes) (t.edges) t in ()*)
