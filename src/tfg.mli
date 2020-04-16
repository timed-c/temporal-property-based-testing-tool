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
	|End of c_construct

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


val k: vertex -> kind_type

val tA: vertex -> int 

val tD:  vertex -> int 

val tj: vertex -> int option

val tr:  vertex -> int option

val tB: vertex -> int option 

val tW:  vertex -> int option 

val generate_tfg: int -> tfg
