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
let fragment_count = ref 0

let resolution = "ms"

let get_vertex_id v = fst v

let get_outdgree v vdlist = 
	let v1 = (List.find (fun a -> (get_vertex_id a.vertx) = v) vdlist) in
	v1.outdegree 

let get_indgree v vdlist = 
	let v1 = (List.find (fun a -> (get_vertex_id a.vertx) = v) vdlist) in
	v1.indegree

let get_timedc_construct v =
    match v with
	|Timingpoint(p) -> (match (p.kind) with
						|Soft -> Stp
						|Firm -> Ftp)
	|Fragment(p) -> (match (p.critical) with
					 |true -> Critical 
					 |false -> Fragment)

						

let get_id_of_while woption =
        match woption with
        |Some(t) -> t 
        |None ->  raise (Match_failure_log("get id of something not while")) 

let rec generate_tab_string lfile tab =
        if (tab > 0) then (fprintf lfile "\t"; (generate_tab_string lfile (tab - 1))) else ()
         

let generate_timingpoint_construct v lfile tcfile abs_arrival tab = 
	let vrtx = snd v in
	let vid = fst v in
	let tp = get_timedc_construct vrtx in
	let newline = "\\n" in
    let frmtr = "%ld" in
	match tp with
    |Stp ->  (generate_tab_string lfile tab); fprintf lfile  "printf(\"STP#%d %s-inf %s-inf %s %s\",abs_arr,(abs_arr + %d), crctl);\n" vid frmtr frmtr frmtr newline (tA vrtx);
             (generate_tab_string lfile tab); fprintf lfile "abs_arr = abs_arr + %d; \n"  (tA vrtx);
             (generate_tab_string tcfile tab); fprintf tcfile "testing_et = testing_entry(&testing_tt);\n";
             (generate_tab_string tcfile tab); fprintf tcfile "crctl=0;\n";
             (generate_tab_string tcfile tab); fprintf tcfile "stp(%d, %d, %s);\n" (tA vrtx) (tD vrtx) resolution;
            (generate_tab_string tcfile tab); fprintf tcfile "testing_exit(&testing_tt, &testing_et, %d, %s, crctl, overshot, &time_until_critical, &testing_ex);\n" vid "\"STP\"";
             (generate_tab_string tcfile tab); fprintf tcfile "crctl=0;\n";
             (tab)
	|Ftp ->  (generate_tab_string lfile tab); fprintf lfile "printf(\"FTP#%d %s-%s %s-%s %s %s\",abs_arr,(abs_arr + %d), (abs_arr+%d), (abs_arr + %d), crctl);\n" vid frmtr frmtr frmtr frmtr frmtr newline (tD vrtx) (tD vrtx) (tD vrtx);
            (generate_tab_string lfile tab); fprintf lfile "abs_arr = abs_arr + %d; \n"  (tA vrtx);
            (generate_tab_string tcfile tab); fprintf tcfile "testing_et = testing_entry(&testing_tt);\n";
            (generate_tab_string tcfile tab); fprintf tcfile "overshot=ftp(%d, %d, %s);\n" (tA vrtx) (tD vrtx) resolution;
            (generate_tab_string tcfile tab); fprintf tcfile "testing_exit(&testing_tt, &testing_et, %d, %s, crctl, overshot, &time_until_critical, &testing_ex);\n"  vid "\"FTP\"";
            (generate_tab_string tcfile tab); fprintf tcfile "crctl=0;\n";
            tab
	| _ ->  raise (Match_failure_log("generate_timingpoint_construct not a tp"))  



let generate_c_construct vid s lfile tcfile tab wfrag =
	let newline = "\\n" in
	match s with
    |Exp ->  (fragment_count := !fragment_count + 1);   
             (generate_tab_string lfile tab); fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline;
             (generate_tab_string tcfile tab); fprintf tcfile "printf(\"Frag#%d %s\");\n" vid newline; 
             (generate_tab_string tcfile tab); fprintf tcfile "code_fragment%d();\n" (!fragment_count);
             (tab, wfrag)
	|If ->   let _ = count := !count + 1  in
             let _ = (fragment_count := !fragment_count + 1) in
             (generate_tab_string lfile tab); fprintf lfile "int var%d = rand()%%2;\n" !count; 
             (generate_tab_string lfile tab); fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline; 
             (generate_tab_string lfile tab); fprintf lfile "if(var%d){\n" !count;
             (generate_tab_string tcfile tab);fprintf tcfile "int var%d = rand()%%2;\n" !count;
             (generate_tab_string tcfile tab); fprintf tcfile "printf(\"Frag#%d %s\");\n" vid newline; 
             (generate_tab_string tcfile tab); fprintf tcfile "if(var%d){\n" !count;
             ((tab+1), wfrag)
    |Else -> (generate_tab_string lfile (tab-1));fprintf lfile "}\n"; 
             (generate_tab_string lfile (tab-1)); fprintf lfile "else{\n";
             (generate_tab_string lfile tab); fprintf lfile "printf(\"Frag%d %s\");\n" vid newline;
             (generate_tab_string tcfile (tab-1)); fprintf tcfile "}\n";
             (generate_tab_string tcfile (tab-1)); fprintf tcfile "else{\n";
             (generate_tab_string tcfile tab); fprintf tcfile "printf(\"Frag%d %s\");\n" vid newline;
             (generate_tab_string tcfile tab); fprintf tcfile "code_fragment%d();\n" (!fragment_count);
             (tab, wfrag)
    |End(et) -> (match et with 
                |Loop ->   let wfrag_val = Stack.pop wfrag in
                           (fragment_count := !fragment_count + 1); 
                           (generate_tab_string lfile (tab-1)); fprintf lfile "}\n"; 
                           (generate_tab_string lfile (tab-1)); fprintf lfile "printf(\"Frag#%d %s\");\n" wfrag_val newline;
                           (generate_tab_string lfile (tab-1)); fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline;
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "}\n"; 
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "printf(\"Frag#%d %s\");\n" wfrag_val newline;
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "printf(\"Frag#%d %s\");\n" vid newline;
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "code_fragment%d();\n" !fragment_count;
                           ((tab-1), wfrag)
                |Infiniteloop ->  let wfrag_val = Stack.pop wfrag in 
                           (fragment_count := !fragment_count + 1); 
                           (generate_tab_string lfile (tab-1)); fprintf lfile "}\n"; 
                           (generate_tab_string lfile (tab-1)); fprintf lfile "printf(\"Frag#%d %s\");\n" wfrag_val newline;
                           (generate_tab_string lfile (tab-1)); fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline;
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "}\n"; 
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "printf(\"Frag#%d %s\");\n" wfrag_val newline;
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "printf(\"Frag#%d %s\");\n" vid newline;
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "code_fragment%d();\n" !fragment_count;
                           ((tab-1), wfrag) 
                | _ -> (fragment_count := !fragment_count + 1); 
                           (generate_tab_string lfile (tab-1)); fprintf lfile "}\n";  
                           (generate_tab_string lfile (tab-1)); fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline;
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "}\n"; 
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "printf(\"Frag#%d %s\");\n" vid newline;
                           (generate_tab_string tcfile (tab-1)); fprintf tcfile "code_fragment%d();\n" !fragment_count;
                           ((tab-1), wfrag))
	|While-> let _ = count := !count + 1  in
			 let wvar = "var"^(string_of_int !count) in 
			 let limit = (Random.int 10) + 1 in
			 let _ = count := !count + 1  in
             let _ = Stack.push vid wfrag in
			 let svar = "var"^(string_of_int !count) in 
             (generate_tab_string lfile tab); fprintf lfile "int %s = rand() %% %d;\n" wvar limit; 
             (generate_tab_string lfile tab); fprintf lfile "int %s = 0;\n" svar; 
             (generate_tab_string lfile tab); fprintf lfile "while(%s < %s){\n" svar wvar; 
             (generate_tab_string lfile (tab+1)); fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline; 
             (generate_tab_string lfile (tab+1)); fprintf lfile "%s++;\n"  svar;
             (generate_tab_string tcfile tab); fprintf tcfile "int %s = rand() %% %d;\n" wvar limit; 
             (generate_tab_string tcfile tab); fprintf tcfile "int %s = 0;\n" svar;
             (generate_tab_string tcfile tab); fprintf tcfile "while(%s < %s){\n" svar wvar; 
             (generate_tab_string tcfile (tab+1)); fprintf tcfile "printf(\"Frag#%d %s\");\n" vid newline;
             (generate_tab_string tcfile (tab+1)); fprintf tcfile "%s++;\n" svar;
             ((tab+1), (wfrag))
    |Infinite -> let _ = Stack.push vid wfrag in
                 (generate_tab_string lfile tab); fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline; 
                 (generate_tab_string lfile tab);fprintf lfile "while(1){\n";
                 (generate_tab_string tcfile tab); fprintf tcfile "printf(\"Frag#%d %s\");\n" vid newline;
                 (generate_tab_string tcfile tab);fprintf tcfile "while(1){\n";
                 ((tab+1), (wfrag))

let generate_fragment v cs lfile tcfile tab wfrag =
	let vid = fst v in
	let cf = get_timedc_construct (snd v) in
	let newline = "\\n" in
	match cf with
    |Critical -> (fragment_count := !fragment_count + 1); (generate_tab_string lfile tab) ;fprintf lfile "printf(\"Frag#%d %s\");\n" vid newline;
                 (generate_tab_string tcfile tab); fprintf tcfile "crctl=1;\n";
                 (generate_tab_string tcfile tab); fprintf tcfile "critical{\n";
                 (generate_tab_string tcfile (tab+1)); fprintf tcfile "printf(\"Frag#%d %s\");\n" vid newline;
                 (generate_tab_string tcfile (tab+1)); fprintf tcfile "code_fragment%d();\n" !fragment_count;
                 (generate_tab_string tcfile (tab+1)); fprintf tcfile "time_until_critical=testing_compute_time_until_critical(&testing_ex);\n";
                 (generate_tab_string tcfile (tab)); fprintf tcfile "}\n";
                 (tab, wfrag)
	|Fragment -> let (tab, wfrag) = generate_c_construct vid cs lfile tcfile tab wfrag in 
                 (tab, wfrag) 
	| _ ->  raise (Match_failure_log("generate_fragment not a fragment"))   
 
let generate_log_execution_program v lfile tcfile abs_arrival tab wfrag =
	let vrtx = snd v in
	match vrtx with
	|Timingpoint(p) -> let upd_arr = abs_arrival + (tA vrtx) in 
                       let tab = generate_timingpoint_construct v lfile tcfile abs_arrival tab in
                       (upd_arr, tab, wfrag)
	|Fragment(f) ->	let (tab, wfrag) = generate_fragment v f.cstm lfile tcfile tab wfrag in
                    (abs_arrival, tab, wfrag) 




let rec tfg_log_generator vlist vend nde oc tc abs_arr tab wfrag =
	match nde with 
    |num when num = vend -> print_int (!fragment_count) ; ()
	| _ -> let v = List.find (fun (a,b) -> if (a = nde) then true else false) vlist in
		   (*print_statement_stdio v; 
		   let abs_arr = print_statement v oc tc abs_arr in *)
			let (abs_arr, tab, wfrag) =  generate_log_execution_program v oc tc abs_arr tab wfrag in
            tfg_log_generator vlist vend (nde+1) oc tc abs_arr tab wfrag

let print_statistics oc seed t =
        let tp = List.filter (fun a -> is_timingpoint (snd a)) t.nodes in
        let num_tp = List.length tp in 
        let stp = List.filter (fun a -> is_stp (snd a)) tp in
        let ftp = List.filter (fun a -> is_ftp (snd a)) tp in 
        let num_ftp = List.length ftp in 
        let num_stp = List.length stp in 
        let frag = List.filter (fun a -> is_fragment (snd a)) t.nodes in 
        let num_frag = List.length frag in 
        let ctrcl = List.filter (fun a -> is_critical (snd a)) frag in 
        let num_ctrcl = List.length ctrcl in
        let whle = List.filter (fun a -> is_while (snd a)) frag in 
        let num_while = List.length whle in 
        let ifblck = List.filter (fun a -> is_if (snd a)) frag in 
        let num_if = List.length ifblck in
        let _ = fprintf oc "\n \n \n /************************ statistics *************************\n" in
        let _ = fprintf oc "XXX  max timing points %d\n" num_tp in
        let _ = fprintf oc "breakdown:\n\t initial timing point 1\n\t soft timing points %d \n\t firm timing point %d\n" (num_stp-1) num_ftp in
        let _ = fprintf oc "XXX max fragments %d\n" (num_frag - num_while - num_if) in 
        let _ = fprintf oc "breakdown: \n\t critical fragments %d \n\t non-critical fragments %d\n" num_ctrcl (num_frag - num_while - num_if - num_ctrcl) in
        let _ = fprintf oc "XXX max while loops %d\n" (num_while) in 
        let _ = fprintf oc "XXX max if-else blocks %d\n" num_if in 
        let _ = fprintf oc "********************* end of statistics **********************/" in
        ()


let generate_program_files seed t =
	let filename_gen = "test/gen_"^(string_of_int seed)^".c" in
	let filename_timedc= "test/time_"^(string_of_int seed)^".c" in
	let oc = open_out filename_gen in
    let tc = open_out filename_timedc in
    let _ = fprintf oc "#include<stdio.h>\n#include<stdlib.h> \n \nint main(){\n\tsrand(%d);\n\tlong abs_arr = 0;\n\tlong crctl=0;\n" seed in
    let _ = fprintf tc "#include<stdio.h>\n#include<cilktc.h>\n#include\"pbt.h\"\ntask fun(){\n\tstruct timespec testing_tt, testing_ex;\n\tlong testing_et, time_until_critical=0;\n\tlong crctl;\n\tlong overshot=0;\n\ttesting_init_time(&testing_tt, &testing_ex);\n " in
	let frmtr = "%d\\n\"" in 
    let _ = fprintf oc "\t"; fprintf oc "printf(\"Frag#0 \\n\");\n" in 
	let _ = tfg_log_generator (t.nodes) ((List.length (t.nodes))) 1 oc tc 0  1 (Stack.create ()) in
	let _ = fprintf oc "}" in
    let _ = fprintf tc "}\nint main(){\n\tsrand(%d);\n\tfun();\n}" seed in
    let _ = print_statistics tc seed t in
	let _ = close_out oc in 
	let _ = close_out tc in 
    () 
