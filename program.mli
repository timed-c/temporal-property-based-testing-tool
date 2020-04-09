open Tfg 

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

val tfg_log_generator  : (vertex_id*vertex) list -> int -> int -> out_channel -> out_channel -> int ->  unit


val init_tfg_weighted : (vertex_id*vertex) list -> (vertex_id*vertex_id) list -> tfg -> tfg_weighted

