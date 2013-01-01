open Types;;
open Graphmatrix;;


(* PART 1 - READ A BITMAP FILE AND CONVERT IT TO A BICOLOR MATRIX *)

(* BITMAPFILEHEADER is 14 bytes long, BITMAPINFOHEADER is 40 bytes long
   width = 18-22 array elements
   height = 22-24 array elements
   The colors come after the first 54 bytes.*)


(* read bytes from file and stores in a int list *)
let get_bytes_from_file filename : int list =
  let ch = open_in filename in
  let lst = ref [] in
  try
    while true;
    do
      lst := (input_char ch) :: (!lst);
    done;
    []
  with End_of_file ->
    close_in ch;
    List.map (int_of_char) (List.rev (!lst))
;;


exception ContractViolation;;
exception BlackWhite;;

let rec print_int_list (l: int list) : unit =
  match l with
    | [] -> Printf.printf "" 
    | hd :: tl -> Printf.printf "%d," hd; print_int_list tl
;;

(* converts an int list corresponding to the bytes of the 
   bitmap to a biColor matrix *)
let bytes_to_matrix (l: int list) : BiColorMatrix.matrix =
  
  (* security check for bitmaps *)
  let length = List.length l in
  if (length <= 54) then raise ContractViolation
  else if (not (((length - 54) mod 3) = 0)) then raise ContractViolation
    
  else
    ( 
      (* gets the width and height from the header of the bitmap file *)
      let width = (List.nth l 18) + ((List.nth l 19) * 16) + 
	((List.nth l 20) * 16 * 16) + ((List.nth l 21) * 16 * 16 * 16) in
      let height = (List.nth l 22) + ((List.nth l 23) * 16) + 
	((List.nth l 24) * 16 * 16) + ((List.nth l 25) * 16 * 16 * 16) in
      (* calculates padding *)
      let padding = (4 - ((width * 3) mod 4)) mod 4 in

      (* initializes matrix with given height and width *)
      let myMatrix =  BiColorMatrix.initialize height width in

      (* extracts the first 54 numbers in an int list,
	 corresponding to the header of the bitmap *)
      let rec extract (l: int list) (n: int) : int list =
	if (n = 0) then l
	else 
	  match l with
            | [] -> []
            | h::t -> extract t (n-1)
      in
    
      (* removes the padding from the int list *)
      let rec fix_padding (l: int list) (cur_width: int) (width: int) (pad: int) : int list =
	if (cur_width = width) 
	then fix_padding (extract l pad) 0 width pad
	else (
	match l with
	  | [] -> []
	  | h::[] -> raise ContractViolation
	  | h1::h2::[] -> raise ContractViolation
	  | r::g::b::t -> r::g::b::(fix_padding t (cur_width + 1) width pad))
      in
      
    (* updates the cordinates of a pixel to the next pixel  *)
      let update_coords (x: int)  (y: int)  (width: int) : (int * int) =
	if (x = width - 1) then (0, y-1)
	else (x + 1, y)
      in
    
    (* converts the adjusted int list to a matrix *)
      let rec store (l: int list) (m: BiColorMatrix.matrix) ((x,y): (int * int)) (width: int) : BiColorMatrix.matrix =
	match l with
          | [] -> m
          | h::[] -> raise ContractViolation
          | h::t::[] -> raise ContractViolation
          | r::g::b::t -> if ((r,g,b) = (0,0,0)) then store t (BiColorMatrix.set m y x Black) (update_coords x y width) width
            else if ((r,g,b) = (255,255,255))
            then store t (BiColorMatrix.set m y x White) (update_coords x y width) width
            else store t (BiColorMatrix.set m y x Black) (update_coords x y width) width
    (* raise BlackWhite *)
      in

      (*
      print_int_list (extract l 54);
      Printf.printf "\n\n";
      print_int_list (fix_padding (extract l 54) 0 width padding); *)
      store (fix_padding (extract l 54) 0 width padding) 
	myMatrix (0,height-1) width)
;;



let bitmap_to_matrix (file: string) : (BiColorMatrix.matrix) = 
  bytes_to_matrix (get_bytes_from_file file)
;;
  


(* PART 2 - READ A BICOLOR MATRIX AND CONVERT IT TO A GRAPH *)

(* HELPER FUNCTIONS: *)

(* add a list of nodes to a graph *)
let rec add_nodes (g: IntVertexGraph.graph) 
    (lst: IntVertexGraph.node list) : IntVertexGraph.graph =
  match lst with
    | [] -> g
    | hd :: tl -> add_nodes (IntVertexGraph.add g hd) tl
;;


(* remove all the nodes with no edges from a graph *)
let remove_single_nodes (g: IntVertexGraph.graph) : IntVertexGraph.graph =
  let nodes = IntVertexGraph.vertices g in
  let rec remove_list (g0: IntVertexGraph.graph) (lst: IntVertexGraph.node list) =
    match lst with
      | [] -> g0
      | hd :: tl -> 
	match (IntVertexGraph.outneighbors g0 hd, IntVertexGraph.inneighbors g0 hd) with
	  | ([],[]) -> remove_list (IntVertexGraph.remove g0 hd) tl
	  | _ -> remove_list g0 tl
  in
  remove_list g nodes
;;

(* initializes list of all pairs (i,j) with 0<=i<=xmax and 0<=j<=ymax *)
let rec init_list (xmax: int) (ymax: int) : (int*int) list =
  let rec init_line (x: int) (maxy: int) =
    if (maxy <= 0) then [(x,0)] 
    else (x, maxy) :: (init_line x (maxy-1))
  in
  if (xmax <= 0) then init_line 0 ymax
  else (init_line xmax ymax) @ (init_list (xmax-1) ymax)
;;


(* gets a pixel (entry from the matrix), compares this pixel with its neighbors
   and the edges to the given graph. It only consider the case when the pixel
   is black, so we don't count the same case twice. *)
let set_edges_single (b: BiColorMatrix.matrix) ((x,y): (int*int))
    (g: IntVertexGraph.graph) =

  let edge (g0: IntVertexGraph.graph) ((x1,y1): (int*int)) ((x2,y2): (int*int))
      (n1: IntVertexGraph.node) (n2: IntVertexGraph.node) = 
    match ((BiColorMatrix.get b y1 x1),(BiColorMatrix.get b y2 x2)) with
      | (Black, White) -> IntVertexGraph.set_edge g0 n1 n2
      | _ -> g0
  in

  let g1 = edge g (x,y) (x-1,y) (x,y) (x,y+1) in
  let g1 = edge g1 (x,y) (x+1,y) (x+1,y+1) (x+1,y) in
  let g1 = edge g1 (x,y) (x,y-1) (x+1,y) (x,y) in
  let g1 = edge g1 (x,y) (x,y+1) (x,y+1) (x+1,y+1) in

  g1
;;


(* FUNCTION THAT CONVERTS THE BITMAP MATRIX TO A GRAPH *)

(* converts a bitmap to its respective graph, using helper functions above *)
let matrix_to_graph (b: BiColorMatrix.matrix) : IntVertexGraph.graph =
  let (height, width) = BiColorMatrix.dimensions b in
  let init_graph = add_nodes IntVertexGraph.empty (init_list width height) in
  let g0 = List.fold_right (set_edges_single b) (init_list width height) init_graph in
  remove_single_nodes g0
;;






(* PART 3 - EXTRACT PATHS FROM A GRAPH *)

exception NoNeighbors ;;

(* HELPER FUNCTIONS *)

(* gets an outneighbor of a given node from a given graph *)
let get_next (g: IntVertexGraph.graph) (v: IntVertexGraph.node) : IntVertexGraph.node =
  let out = IntVertexGraph.outneighbors g v in
  match out with
    | [] -> raise NoNeighbors
    | hd :: tl -> hd
;;


(* gets a path that starts in a given node and ends in a given node 
   to use as a helper function for get_cycle *)
let rec cycle_ending_in (g: IntVertexGraph.graph) (first: IntVertexGraph.node)
    (current: IntVertexGraph.node list) : (IntVertexGraph.node list) =
  match current with
    | [] -> 
      let outnb = get_next g first in
      cycle_ending_in (IntVertexGraph.remove_edge g first outnb) first [outnb] 
    | hd :: tl -> 
      if (IntVertexGraph.isequal first hd) then current @ [first]
      else 
	let outnb = get_next g hd in
	cycle_ending_in (IntVertexGraph.remove_edge g hd outnb) first (outnb :: current) 	  
;;


(* gets a graph, and returns a path which is a cycle that starts and ends
   in the left most top most vertex *)
let get_cycle (g: IntVertexGraph.graph) : (IntVertexGraph.node list) =
  match (IntVertexGraph.get_vertex g) with
    | None -> []
    | Some v ->
      List.rev (cycle_ending_in g v [])
;;


(* removes the all internal cycles *)
let squeeze (l: int vertex list) : int vertex list =
  let rec rec_remove (l: int vertex list) (v: int vertex) : int vertex list =
    match l with
      | [] -> []
      | h::t -> if (v = h) then l
        else rec_remove t v
  in 
  let rec rec_squeeze (l: int vertex list) (frontier: int vertex list) : int vertex list =
    match l with
      | [] -> List.rev frontier
      | h::t -> if (List.mem h frontier) 
	then (rec_squeeze t (rec_remove frontier h))
	else (rec_squeeze t (h::frontier))
  in
  match l with
    | [] -> []
    | h::t -> h::(rec_squeeze t [])
;;


(* removes all edges that correspond to a sequence of connected nodes *)
let rec remove_edges (g: IntVertexGraph.graph) (lst: IntVertexGraph.node list) 
    : IntVertexGraph.graph =
  match lst with
    | [] -> g
    | hd :: [] -> g
    | hd1 :: hd2 :: tl -> remove_edges (IntVertexGraph.remove_edge g hd1 hd2) (hd2 :: tl)
;;


(* removes all the nodes with no edges from a graph *)
let remove_single_nodes (g: IntVertexGraph.graph) : IntVertexGraph.graph =
  let nodes = IntVertexGraph.vertices g in
  let rec remove_list (g0: IntVertexGraph.graph) (lst: IntVertexGraph.node list) =
    match lst with
      | [] -> g0
      | hd :: tl -> 
	match (IntVertexGraph.outneighbors g0 hd, IntVertexGraph.inneighbors g0 hd) with
	  | ([],[]) -> remove_list (IntVertexGraph.remove g0 hd) tl
	  | _ -> remove_list g0 tl
  in
  remove_list g nodes
;;


exception NotPath ;;
exception WrongEdge ;;


let path_color (lst: IntVertexGraph.node list) : biColor =
  match lst with 
    | [] -> raise NotPath 
    | hd :: [] -> raise NotPath
    | hd1 :: hd2 :: tl ->
      let (x1,y1) = hd1 in
      let (x2,y2) = hd2 in
      match ((x2-x1),(y2-y1)) with
	| (0,1) -> Black
	| (1,0) -> White
	| _ -> raise WrongEdge
;;
      


let rec rec_extract_paths (g: IntVertexGraph.graph) : (int path list) =
  match IntVertexGraph.get_vertex g with
    | None -> []
    | Some v -> 
      let cycle = get_cycle g in
      let path = squeeze cycle in
      let newgraph = (remove_single_nodes (remove_edges g path)) in
      ((path, path_color path) :: (rec_extract_paths newgraph))


(* EXTRACT PATHS FROM GRAPH *)

let extract_paths (g: IntVertexGraph.graph) : (int path list) =
  List.rev (rec_extract_paths g)
