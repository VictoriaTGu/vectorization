(* General matrix module *)

(* positions start in 0 *)
module type MATRIX = 
sig
  type elt
  type matrix

  val default : elt
  
  (* returns the dimensions (rows/cols) of the matrix *)
  val dimensions : matrix -> (int * int)

  (* set an element of the matrix in a position (row/col)
   * returns unchanged matrix if position not found *)
  val set : matrix -> int -> int -> elt -> matrix

  (* gets the element in the given position (row/col) 
   * returns default element if position not found *)
  val get : matrix -> int -> int -> elt

  (* initialize a matrix with the given dimensions (rows/cols).
   * all elements will be the default element *)
  val initialize : int -> int -> matrix

end


(* Module for Directed Graphs *)
module type DIRECTEDGRAPH = 
sig
  
  type node
  
  type graph

  (* a graph with no vertices *)
  val empty : graph

  (* returns true if the given nodes are equal *)
  val isequal : node -> node -> bool
  
  (* returns the out-neighborhood of a vertex.
  returns empty list if vertex is not in graph *)
  val outneighbors : graph -> node -> node list

  (* returns the in-neighborhood of a vertex *)
  val inneighbors : graph -> node -> node list

  (* returns a list of all vertices *)
  val vertices : graph -> node list
    
  (* add a vertex into the graph - without edges *)
  val add : graph -> node -> graph

  (* removes a vertex (and its edges) from the graph
   * if vertex not found returns graph unchanged *)
  val remove : graph -> node -> graph

  (* inserts/changes an edge in the graph, from the first to the
   * last vertex. if vertices are not found returns the graph 
   * unchanged *)
  val set_edge : graph -> node -> node -> graph

  (* removes the edge from the first to the second vertex 
   * if vertex is not found, return graph unchanged *)
  val remove_edge : graph -> node -> node -> graph

  (* get a vertex from the graph *)
  val get_vertex : graph -> node option

end


(* Type of comparison *)
type relation = Less | Greater | Equal ;;

(* type for weight *)
type inf_weight = Infinity | Weight of (int * float) ;;

(* Module for Weighted Directed Graphs  
   the nodes must be integers from 0 to n-1, 
   where n is the total number of nodes *)
module type WEIGHTEDGRAPH = 
sig
  type node
  type graph
  type weight

  (* a graph with no vertices *)
  val empty : graph

  (* returns true if the given nodes are equal *)
  val isequal : node -> node -> bool

  (* initializes a graph with n nodes *)
  val init_graph : int -> graph
  
  (* returns the out-neighborhood of a vertex.
  returns empty list if vertex is not in graph *)
  val outneighbors : graph -> node -> node list

  (* returns the in-neighborhood of a vertex 
  val inneighbors : graph -> node -> node list *)

  (* returns a list of all vertices 
  val vertices : graph -> node list *)
    
  (* add a vertex into the graph - without edges 
     adds the node in the last position *)
  val add : graph -> graph 

  (* removes a vertex (and its edges) from the graph
   * if vertex not found returns graph unchanged 
  val remove : graph -> node -> graph *)

  (* inserts/changes an edge in the graph (with a weight), 
   * from the first to the last vertex. 
   * if vertices are not found returns the graph 
   * unchanged *)
  val set_edge : graph -> node -> node -> weight -> graph
  
  (* returns Some w, if the edge exists and has weight w, or None if the edge from v1 to v2 does not exist *)
  val get_edge : graph -> node -> node -> weight option
  

  (* compares two weights *)
  val compare : weight -> weight -> relation

  (* compares two nodes *)
  val compare_nodes : node -> node -> relation
  
  (* sums two weights *)
  val sum : weight -> weight -> weight
  
  (* clones a vertex n, puts the clone in the last position
     and copies the edges *)
  val clone_vertex : graph -> node -> graph
  
  (* prints the graph *)
  val print_graph : graph -> unit
  
  (* removes the edge from the first to the second vertex 
   * if vertex is not found, return graph unchanged 
  val remove_edge : graph -> node -> node -> graph

  (* get a vertex from the graph *)
  val get_vertex : graph -> node option *)

end


(* type for directions in step 2 *)
type direction = South | North | East | West ;;


(* BiColor type for binary pixels *)
type biColor = Black | White ;;

(* Paths declaration *)
  type 'a vertex = ('a * 'a) ;;
  type 'a path = ('a vertex list * biColor) ;;
  

(* types for the vector display *) 
type point = float vertex ;;
type curve = Line | Bezier of (point*point) ;;
type vector = point * ((curve*point) list);; 
(*type vector = (point * curve) list ;;*)
type line = (float vertex * (float*float)) ;;
