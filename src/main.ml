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
    let seed = if ((Array.length (Sys.argv)) < 2) then ((Random.self_init ()); Random.int(1000000)) else (int_of_string (Sys.argv.(1))) in
	let t = generate_tfg seed in
    let _ = print_int seed; print_string ":" in
	let _ = print_tfg_file t seed in
    generate_program_files seed t 

