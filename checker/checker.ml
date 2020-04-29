open Printf
open String
open Str


exception Log_match_failure of string

(*
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
	let telist = String.split_on_char '#' thd in
	let eelist = String.split_on_char '#' ehd in
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
	(*(List.iter (fun a-> print_string (a^"\n"))) llist*) *)

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

let get_int_val t = 
    let tlst = Str.split (Str.regexp "-") t in
    let u = int_of_string (List.nth tlst 0) in
    let l = if ((List.nth tlst 1) = "inf") then (max_int) else int_of_string (List.nth tlst 1) in
    (u,l)

let check_entry_stp_and_ftp_without_dmiss tlst elst =
   let (tlow1, tupp1) = get_int_val (List.nth tlst 1) in
   let etme_a = int_of_string (List.nth elst 1) in
   if ((tlow1 <= etme_a) && (etme_a <= tupp1)) then true else false 

let check_entry_ftp_dmiss tlst elst =
   let etme_critical = int_of_string (List.nth elst 4) in 
   let etme_trigger_precision = int_of_string (List.nth elst 5) in 
   let etme_overshot = int_of_string (List.nth elst 3) in 
   if (etme_critical <> 0) then 
            begin 
              if (etme_trigger_precision = etme_overshot) then true else false 
            end
    else 
            begin
            if (etme_overshot = 0) then true else false 
            end 

let check_exit_stp_and_ftp_without_dmiss tlst elst = 
   let (tlow2, tupp2) = get_int_val (List.nth tlst 2) in
   let etme_d = int_of_string (List.nth elst 2) in
   if ((tlow2 <= etme_d) && (etme_d <= tupp2)) then true else false

let check_exit_ftp_dmiss tlst elst =
    let (tlow2, tupp2) = get_int_val (List.nth tlst 2) in
    let etme_critical = int_of_string (List.nth elst 4) in 
    let etme_trigger_precision = int_of_string (List.nth elst 5) in 
    let etme_d = int_of_string (List.nth elst 2) in
    if (etme_critical <> 0) then 
            begin 
             if ((tlow2 <= etme_d) && (etme_d <= tupp2)) then true else false
            end
    else 
            begin
              if ((tlow2 <= etme_d) && (etme_d <= (tupp2 + etme_trigger_precision))) then true else false
            end 

let compare_tp tlst elst = 
        let id = if ((List.nth tlst 0) = (List.nth elst 0)) then true else false in
        let etme_a = int_of_string (List.nth elst 1) in
        let stporftp = List.nth (Str.split (Str.regexp "#+") (List.nth elst 0)) 0 in
        let upper = if ((etme_a <> 0) || (stporftp = "STP")) then check_entry_stp_and_ftp_without_dmiss tlst elst else check_entry_ftp_dmiss tlst elst in
        let lower = if (etme_a <> 0 || (stporftp = "STP")) then check_exit_stp_and_ftp_without_dmiss tlst elst else check_exit_ftp_dmiss tlst elst in
        (id && upper && lower)


let string_equal s t =  if (s = t) then true else false

let compare_element telem eelem =
   let tlst = Str.split (Str.regexp " +") telem in
   let elst = Str.split (Str.regexp " +") eelem in
   if (((List.length tlst) = 1) && (((List.length elst) = 1))) then (string_equal telem eelem) else compare_tp tlst elst
 
  

let rec compare_log pr =
   match pr with 
   |(th::trst, eh::erst) -> let res = compare_element th eh in
                            if res then (compare_log (trst, erst)) else false
   |([],[]) -> true

let read_logs tlog elog =
    let tlist = read_file tlog in
    let elist = read_file elog in
    let _ = if ((List.length tlist) <> (List.length elist)) then (raise (Log_match_failure("log not of equal length"))) in
    let res = compare_log (tlist, elist) in 
    if res then print_string "PASS\n" else print_string "FAIL\n"
    

let main =
        read_logs "tfg_log" "exe_log"
