open List 
open Random 

exception Match_failure of string

type kind_type = 
 | Soft  
 | Firm  

type c_construct = 
 |Expression
 |Ifelse
 |Loop
 |Infiniteloop 

type timedc_construct = 
 |Stp
 |Ftp
 |Fragment 
 |Critical

type vertex_id = int 

type block_construct =
	|Exp
	|If
	|Else
	|While
	|Infinite
	|End

type timingpoint_param =
{
  kind : kind_type;
  relative_arrival : int option;
  relative_deadline : int option;
  release_jitter  : int option;
  trigger_precision : int option;
  tstm : block_construct;

} 

type codefragment_param =
{
  bcet : int option;
  wcet : int option;
  critical: bool;
  cstm : block_construct;
}

type vertex =
  |Timingpoint of timingpoint_param
  |Fragment of codefragment_param

type tfg = 
{
  nodes : (vertex_id * vertex) list; 
  edges : (vertex_id * vertex_id) list; 
} 

let node_count = ref 0 

let rec pick_sample table weight sample =
  match table with 
  |h::rst -> let weight = weight + (snd h) in 
			  if (sample < weight) then (fst h) else (pick_sample rst weight sample)
  |[] -> raise (Match_failure("sample error"))
  
  
let sample_distribution dtable =
  let prob = Random.int 100 in
  pick_sample dtable 0 prob 


let trig_precision knd = 
  match knd with
  |Soft -> Some(0)
  |Firm -> None 


let k v =
  match v with 
  |Timingpoint(p) -> p.kind
  | _ -> raise (Match_failure("trying to find kind of vertex that is not a timing point"))

let tA v =
  match v with 
  |Timingpoint(p) ->  (match p.relative_arrival with
					   |Some(tm) -> tm
					   |None -> raise (Match_failure("trying to find tA of vertex that is not a timing point")))  					 
  | _ -> raise (Match_failure("trying to find tA of vertex that is not a timing point"))

let tD v =
  match v with 
  |Timingpoint(p) ->  (match p.relative_deadline with 
					   |Some(tm) -> tm
					   |None -> raise (Match_failure("trying to find tD of vertex that is not a timing point")))  	
  | _ -> raise (Match_failure("trying to find tD of vertex that is not a timing point"))

let tj v =
  match v with 
  |Timingpoint(p) ->  p.release_jitter 
  | _ -> raise (Match_failure("trying to find tj of vertex that is not a timing point"))

let tr v =
  match v with 
  |Timingpoint(p) ->  p.trigger_precision 
  | _ -> raise (Match_failure("trying to find tj of vertex that is not a timing point"))

let tB v =
  match v with 
  |Fragment(f) ->  f.bcet
  | _ -> raise (Match_failure("trying to find BCET of a vertex that is not a code fragment"))

let tW v =
  match v with 
  |Fragment(f) ->  f.wcet
  | _ -> raise (Match_failure("trying to find WCET of vertex that is not a code fragment"))

let critical v =
  match v with 
  |Fragment(f) ->  f.critical
  | _ -> raise (Match_failure("trying to find critical of vertex that is not a code fragment"))


let generate_timingpoint (plist:(vertex_id*vertex) list) (elist:(vertex_id*vertex_id) list) (vid:vertex_id) (knd:kind_type) (st:block_construct) = 
  let rtable = [(3,1); (2,2); (5,5); (25,10); (25,20); (3,50); (20,100);(1,200); (4,250);(5,500);(3,750);(4,1000);] in 
  let tA = sample_distribution rtable in 
  let tD = tA in
  (*let tD = sample_distribution rtable in*)
  let p = Timingpoint({kind = knd; relative_arrival= Some(tA); relative_deadline=Some(tD); release_jitter=None; trigger_precision=(trig_precision knd); tstm=st}) in  
  let _ = node_count := !node_count + 1  in
  let pid = !node_count in
  let plist_new = (pid, p) :: plist in
  let elist_new = (vid,pid) :: elist in  
  (plist_new, elist_new)


let generate_fragment (flist:(vertex_id*vertex) list) (elist:(vertex_id*vertex_id) list) (vid:vertex_id) (crt:bool)  st = 
  let f = Fragment({bcet=None; wcet= None; critical=crt; cstm=st}) in 
   let _ = node_count := !node_count + 1  in
  let fid = !node_count in
  let flist_new = (fid, f) :: flist in
  let elist_new = (vid,fid) :: elist in  
  (flist_new, elist_new)


let generate_vertex (plist:(vertex_id*vertex) list) (flist:(vertex_id*vertex) list) (elist:(vertex_id*vertex_id) list) (vid: vertex_id) (st:block_construct) =
   let ctable =[(Stp, 50);(Ftp, 0);(Critical, 0);(Fragment, 50)] in
   let prob = sample_distribution ctable in
   match prob with
   |Stp -> let (plist_new, elist_new) = generate_timingpoint plist elist vid Soft st in
			 (plist_new, flist, elist_new, !node_count)
   |Ftp -> let (plist_new, elist_new) = generate_timingpoint plist elist vid Firm st in
			 (plist_new, flist, elist_new, !node_count)
   |Critical -> let (flist_new, elist_new) = generate_fragment flist elist vid true st in
			 (plist, flist_new, elist_new, !node_count)
   |Fragment -> let (flist_new, elist_new) = generate_fragment flist elist vid false st in
			 (plist, flist_new, elist_new, !node_count)

    
let rec generate_subgraph plist flist elist vid iter =
  match iter with
   | 0 -> (plist, flist, elist, vid)
   | _ -> generate_block plist flist elist vid iter 

and generate_block plist flist elist vid iter = 
   let ptable =[(Expression, 80) ;(Ifelse, 10);(Loop,10);(Infiniteloop, 0)] in
  (* let dtable = [(2, 10); (2, 5); (2, 85)] in *)
   let dtable = [(1, 35); (2, 35); (3, 20);(4, 10)] in
   let prob = sample_distribution ptable in 
   match prob with 
   |Expression -> (*print_string "expression";*)  let (plist, flist, elist, vid) = generate_vertex plist flist elist vid Exp in 
				   (*print_int (!node_count); print_string "\n";*)generate_subgraph plist flist elist vid (iter-1)
   |Ifelse -> let (flist, elist) = generate_fragment flist elist vid false If in
   			  let v1 = !node_count in
			  let num = sample_distribution dtable in 
 			  let  (plist, flist, elist, v2) = generate_subgraph plist flist elist v1 num in
			  let num = sample_distribution dtable in 
			  let (flist, elist) = generate_fragment flist elist v1 false Else in
              let v3 = !node_count in
 			  let  (plist, flist, elist, v4) = generate_subgraph plist flist elist v3 num in
			  let (flist, elist) = generate_fragment flist elist v2 false End in
			  let vid = !node_count in
              let elist = (v4, vid) :: elist in 
				 generate_subgraph plist flist elist vid (iter-1)
   |Loop -> (*print_string "loop"; *) let (flist, elist ) = generate_fragment flist elist vid false While in
			(*print_int (!node_count); print_string "\n";*)let v1 = !node_count in
			let num = sample_distribution dtable in 
            let  (plist, flist, elist, v2) = generate_subgraph plist flist elist v1 num in
			let (flist, elist) = generate_fragment flist elist v1 false End in
			let elist = (v2, v1) :: elist in 
            let v1 = !node_count in
            generate_subgraph plist flist elist v1 (iter-1)
   |Infiniteloop -> (*print_string "infiniteloop\n";*)  let (flist, elist) = generate_fragment flist elist vid false Infinite in
			   let v1 = !node_count in
			   let num = sample_distribution dtable in 
               let  (plist, flist, elist, v2) = generate_subgraph plist flist elist v1 num in
			   let elist = (v2, v1) :: elist in 
               generate_subgraph plist flist elist vid 0

 
(*let rec print_state_st t =
	match t with
	| h::rst -> print_int h; print_string ","	
    | [] -> ()

let print_state t =
	print_string "st = ["; print_state_st t.st; print_string "]"; print_string " idx ="; print_int t.idx; print_string "\n" *)
let generate_tfg seed = 
    let dtable = [(1, 0); (2, 35); (3, 35);(4, 30)] (* (5, 10); (6, 10);(7, 10); (8, 10); (9, 10);(10, 10)] *)in 
    let _ = Random.init(seed) in
    let p = Timingpoint({kind = Soft; relative_arrival= Some(0); relative_deadline=Some(0); release_jitter=None; trigger_precision=Some(0); tstm=Exp}) in 
    let vid = 0 in 
	let plist = [(vid, p)]  in
    let flist = [] in
    let elist = [] in
    let iter = sample_distribution dtable in 
    let (plist, flist, elist, vid) = generate_subgraph plist flist elist vid iter in 
    let t = {nodes = (List.append (List.rev plist) (List.rev flist)); edges = elist } in
    t





