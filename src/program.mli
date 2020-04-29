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



val generate_program_files : int -> tfg ->  unit



