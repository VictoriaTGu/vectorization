open Types;;
open Print;;


(* implementation of module BiColorMatrix for bitmaps using arrays *)
module BiColorMatrix : (MATRIX with type elt = biColor) =
struct
  type elt = biColor
  type matrix = (elt array) array
  let default = White
    

  let dimensions (mat: matrix) = 
    let first = Array.get mat 0 in
    (Array.length first, Array.length mat)
  

  let set (mat: matrix) (row: int) (col: int) (entry: elt) =
    let (rows, cols) = dimensions mat in
    (* if (row,col) not found, return unchanged matrix *)
    if ((row < 0) || (row >= rows) || (col < 0) || (col >= cols))
    then mat
    else
      let thecol = Array.get mat col in
      let _ = Array.set thecol row entry in
      let _ = Array.set mat col thecol in
      mat

  
  let get (mat: matrix) (row: int) (col: int) =
    let (rows, cols) = dimensions mat in
    (* if (row,col) not found, return default value *)
    if ((row < 0) || (row >= rows) || (col < 0) || (col >= cols))
    then default
    else
      let thecol = Array.get mat col in
      Array.get thecol row 


  let initialize (rows: int) (cols: int) = 
    Array.make_matrix cols rows default

end 



(* implementation of graph with int vertices *)
module IntVertexGraph : (DIRECTEDGRAPH with type node = (int vertex)) =
struct
  type node = (int vertex)
  type graph = (node * node list) list

  let empty = []


  let isequal (v1: node) (v2: node) = 
    let (x1,y1) = v1 in
    let (x2,y2) = v2 in
    ((x1 = x2) && (y1 = y2))

  let rec inlist (lst: node list) (v1: node) = 
    match lst with
      | [] -> false
      | hd :: tl ->
	(isequal v1 hd) || (inlist tl v1)


  let outneighbors (g: graph) (v: node) = 
    let rec outhelper (g0: graph) (v0: node) =
      match g0 with
	| [] -> []
	| hd :: tl -> 
	  let (v1, nb) = hd in
	  if (isequal v1 v0)
	  then nb
	  else outhelper tl v0
    in 
    outhelper g v

  let inneighbors (g: graph) (v: node) = 
    let rec inhelper (g0: graph) (v0: node) =
      match g0 with
	| [] -> []
	| hd :: tl -> 
	  let (v1, nb) = hd in
	  if (inlist nb v0)
	  then v1 :: (inhelper tl v0)
	  else inhelper tl v0

    in 
    inhelper g v

  let vertices (g: graph) = 
    let rec verthelper (g0: graph) =
      match g0 with
	| [] -> []
	| hd :: tl ->
	  let (v, nb) = hd in
	  v :: (verthelper tl)
    in
    verthelper g
	

  let add (g: graph) (v: node) =
    if (inlist (vertices g) v) then g
    else (v, []) :: g

  let rec remove_from_list (lst: node list) (v: node) =
    match lst with
      | [] -> []
      | hd :: tl -> 
	if (isequal v hd) then remove_from_list tl v
	else hd :: (remove_from_list tl v)
  
  let remove (g: graph) (v: node) = 
    let rec remove_helper (g0: graph) (v0: node) = 
      match g0 with
	| [] -> []
	| hd :: tl -> 
	  let (v1, nb) = hd in
	  if (isequal v1 v0) then remove_helper tl v0
	  else (v1, remove_from_list nb v0) :: remove_helper tl v0
    in
    remove_helper g v


  let rec add_to_list (lst: node list) (v: node) = 
    match lst with
      | [] -> [v]
      | hd :: tl -> if (isequal v hd) then lst
	else hd :: (add_to_list tl v)


  let set_edge (g: graph) (v1: node) (v2: node) =
    let rec sethelper (g0: graph) (vt1: node) (vt2: node) =
      match g0 with
	  | [] -> []
	  | hd :: tl -> 
	  let (v, nb) = hd in
	  if (isequal vt1 v)
	  then (vt1, add_to_list nb vt2) :: tl
	  else hd :: (sethelper tl vt1 vt2)
    in
    if (inlist (vertices g) v2)
    then sethelper g v1 v2
    else g


  let remove_edge (g: graph) (v1: node) (v2: node) =
    let rec removehelper (g0: graph) (vt1: node) (vt2: node) =
      match g0 with
	| [] -> []
	| hd :: tl -> 
	  let (v, nb) = hd in
	  if (isequal vt1 v)
	  then (vt1, remove_from_list nb vt2) :: tl
	  else hd :: (removehelper tl vt1 vt2)
    in
    removehelper g v1 v2



  let rec list_min (lst: int list) : int =
    match lst with
      | [] -> max_int
      | hd :: tl -> min hd (list_min tl)


  (* gets the left most top most vertex *)
  let get_vertex (g: graph) =
    let vlist = vertices g in
    match vlist with
      | [] -> None
      | hd :: tl -> 
	let xlist = List.map (fun (a,b) -> a) vlist in
	let minx = list_min xlist in
	let leftmost = List.filter (fun (a,b) -> (a = minx)) vlist in
	let ylist = List.map (fun (a,b) -> b) leftmost in
	let miny = list_min ylist in
	Some (minx, miny)

end  






module WIntGraph : (WEIGHTEDGRAPH with type node = int
			       and type weight = inf_weight) =
struct
  type node = int 
  type weight = inf_weight
  type graph = ((node*weight) list) array

  let empty = Array.make 0 []

  let isequal (v1: node) (v2: node) : bool = 
    v1 = v2

  let rec inlist (lst: node list) (v1: node) = 
    match lst with
      | [] -> false
      | hd :: tl -> (isequal v1 hd) || (inlist tl v1)

  (* Initialize graph with values of 0 *)
  let init_graph (n: int): graph = 
    Array.make n []

  let outneighbors (g: graph) (v: node) = 
    let lst = Array.get g v in
    List.map (fun (nb,w) -> nb) lst

  (* let inneighbors (g: graph) (v: node) = 
    let rec inhelper (g0: graph) (v0: node) =
      match g0 with
	| [] -> []
	| hd :: tl -> 
	  let (v1, nb) = hd in
	  if (inlist nb v0)
	  then v1 :: (inhelper tl v0)
	  else inhelper tl v0

    in 
    inhelper g v 

  let vertices (g: graph) = 
    let rec verthelper (g0: graph) =
      match g0 with
	| [] -> []
	| hd :: tl ->
	  let (v, nb) = hd in
	  v :: (verthelper tl)
    in
    verthelper g 
	
*)
  let add (g: graph) : graph =
    Array.append g (Array.make 1 [])
    
(*
  let rec remove_from_list (lst: node list) (v: node) =
    match lst with
      | [] -> []
      | hd :: tl -> 
	if (isequal v hd) then remove_from_list tl v
	else hd :: (remove_from_list tl v)
  
  let remove (g: graph) (v: node) = 
    let rec remove_helper (g0: graph) (v0: node) = 
      match g0 with
	| [] -> []
	| hd :: tl -> 
	  let (v1, nb) = hd in
	  if (isequal v1 v0) then remove_helper tl v0
	  else (v1, remove_from_list nb v0) :: remove_helper tl v0
    in
    remove_helper g v  *)


  let rec add_to_list (lst: (node*weight) list) (v: node) (w: weight) = 
    match lst with
      | [] -> [(v,w)]
      | hd :: tl -> let (v0,w0) = hd in
	if (isequal v v0) then lst
	else hd :: (add_to_list tl v w) 


  let set_edge (g: graph) (v1: node) (v2: node) (w: weight) =
    let nbs = Array.get g v1 in
    let newnbs = add_to_list nbs v2 w in
    Array.set g v1 newnbs; g
    
  let get_edge (g: graph) (v1: node) (v2: node) : weight option =
  let nbs = Array.get g v1 in
  let rec find_node (n: node) (l: (node*weight) list) =
  match l with
  | [] -> None
  | hd :: tl ->  let (v,w) = hd in
    if (isequal v n) then Some w
    else find_node n tl
  in
  find_node v2 nbs 
    
  (*

    let rec sethelper (g0: graph) (vt1: node) (vt2: node) =
      match g0 with
	| [] -> []
	| hd :: tl -> 
	  let (v, nb) = hd in
	  if (isequal vt1 v)
	  then (vt1, add_to_list nb vt2) :: tl
	  else hd :: (sethelper tl vt1 vt2)
    in
    if (inlist (vertices g) v2)
    then sethelper g v1 v2
    else g *)

  let compare (w1: weight) (w2: weight) : relation =
    match (w1,w2) with
    | (Infinity,Infinity) -> Equal
    | (Infinity,Weight(n2,p2)) -> Greater
    | (Weight(n1,p1),Infinity) -> Less
    | (Weight(n1,p1),Weight(n2,p2)) ->
    if ((n1-n2) < 0)
    then Less
    else if ((n1-n2) > 0)
    then Greater
    else if ((p1-.p2) < 0.)
    then Less
    else if ((p1-.p2) > 0.)
    then Greater
    else Equal
    
  let compare_nodes (n1: node) (n2: node) : relation =
    if (n1 > n2) then Greater
    else if (n1 < n2) then Less
    else Equal  

  let sum (w1: weight) (w2: weight) : weight =
    match (w1,w2) with
    | (Weight(n1,p1),Weight(n2,p2)) -> Weight(n1+n2,p1+.p2)
    | _ -> Infinity
    
    
    let clone_vertex (g: graph) (n: node) : graph =
    let total = Array.length g in
    let newgraph = add g in
    let nbs = Array.get g n in
    let _ = Array.set newgraph total nbs in
    
    
    let rec nat_list (a: int) (b: int) : int list  =
    if (b < a) then []
     else a :: (nat_list (a+1) b)
     in
     
     let update_nbs (k: int) =
        match (get_edge newgraph k n) with
        | None -> newgraph
        | Some w -> set_edge newgraph k total w
     in
   List.fold_right (fun x newgraph -> update_nbs x) (nat_list 0 total) newgraph 
;;  


let print_graph (g: graph) : unit = 
    let print_node_weight (n,w) = 
        Printf.printf "("; 
        print_int n;
        Printf.printf ",";
        print_weight w;
        Printf.printf ")"; 
    in 
    print_array (print_list (print_node_weight)) g
;;
        
(*
  let remove_edge (g: graph) (v1: node) (v2: node) =
    let rec removehelper (g0: graph) (vt1: node) (vt2: node) =
      match g0 with
	| [] -> []
	| hd :: tl -> 
	  let (v, nb) = hd in
	  if (isequal vt1 v)
	  then (vt1, remove_from_list nb vt2) :: tl
	  else hd :: (removehelper tl vt1 vt2)
    in
    removehelper g v1 v2



  let rec list_min (lst: int list) : int =
    match lst with
      | [] -> max_int
      | hd :: tl -> min hd (list_min tl)


  (* gets the left most top most vertex *)
  let get_vertex (g: graph) =
    let vlist = vertices g in
    match vlist with
      | [] -> None
      | hd :: tl -> 
	let xlist = List.map (fun (a,b) -> a) vlist in
	let minx = list_min xlist in
	let leftmost = List.filter (fun (a,b) -> (a = minx)) vlist in
	let ylist = List.map (fun (a,b) -> b) leftmost in
	let miny = list_min ylist in
	Some (minx, miny) *)

end  
