open Printf
open String 

exception Log_match_failure of string

type elem_type =
	|Id of string * int 
	|Tp of string * int * int 
	|Ep of string * int 

let read_file filename = 
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines

let to_elem_type_tlog lst =
	(*let _ = List.iter (fun a-> print_string (a^"\n")) lst in*)
	let first_elem  = (List.hd lst) in 
	match first_elem with 
	| "S" -> let eid = int_of_string (List.hd (List.rev lst)) in
			 Id("S", eid)
	| "A" -> let lb = int_of_string (List.nth lst 1) in
			 let ub = if ((List.nth lst 2) = "inf") then max_int else int_of_string (List.nth lst 2) in
			 Tp("A",lb, ub)
	| "E" -> let lb = int_of_string  (List.nth lst 1) in
			 let ub = if ((List.nth lst 2) = "inf") then max_int else int_of_string (List.nth lst 2) in
			 Tp("E",lb, ub)
	| _ ->  raise (Log_match_failure("to_elem mismatch")) 

let to_elem_type_elog lst = 
    (*let _ = List.iter (fun a-> print_string (a^"\n")) lst in*)
	let first_elem  = (List.hd lst) in 
	match first_elem with 
	| "S" -> let eid = int_of_string (List.hd (List.rev lst)) in
			 Id("S", eid)
	| "A" -> let lb = int_of_string (List.nth lst 1) in
			 Ep("A",lb)
	| "E" -> let lb = int_of_string (List.nth lst 1) in
			 Ep("E",lb)
    | _ ->  raise (Log_match_failure("to_elem mismatch")) 

let check_element thd ehd =
	let telist = String.split_on_char ':' thd in
	let eelist = String.split_on_char ':' ehd in
	let telem  =  to_elem_type_tlog telist in
	let eelem =   to_elem_type_elog eelist in
    match (telem, eelem) with 
	|(Id(s1, i1), (Id(s2,i2))) -> if ((s1 = s2) && (i1 = i2)) then true else false 
	|(Tp(s1, l, u), Ep(s2,i)) ->  if ((s1 = s2) && (l <= i) && (i <= u )) then true else false 
	| _ -> raise (Log_match_failure("check_element mismatch")) 
	 
let print_error llist =
	print_string "FAIL\n"

let rec check_logs llist =
	match llist with
	|((thd :: ttail), (ehd :: etail)) -> let pass = check_element thd ehd in
										 if (pass) then check_logs (ttail, etail) else false
	|([], []) -> true 
    | _ -> raise (Log_match_failure("check_logs failure"))


let read_tfg_logs fname = 
	let tfg_list = read_file fname in
	let exe_list = read_file "test/exe_log" in
	let _ = if ((List.length tfg_list) <> (List.length exe_list)) then (raise (Log_match_failure("log not of equal length"))) in
	let res = check_logs (tfg_list, exe_list) in 
	if (res = true) then print_string "PASS\n" else print_string "FAIL\n" 
	(*(List.iter (fun a-> print_string (a^"\n"))) llist*)
