open Types ;;
open Print ;;
open Graphmatrix ;;


(* helper function extremely useful everywhere *)

(* gives a list with the integers from a to b, inclusive *)
let rec nat_list (a: int) (b: int) : int list  =
  if (b < a) then []
  else a :: (nat_list (a+1) b)
;;


(********************************************
 * PART 1 - CONVERT PATH TO WEIGHTED GRAPH  *
 ********************************************)


(*** 1.1 - CHEKING THE IF THE FOUR DIRECTIONS OCCUR ***)

exception Impossible ;;


(* finds the direction from v1 to v2 *)
let find_direction (v1 : int vertex) (v2: int vertex) : direction =
  let ((a, b), (c, d)) = (v1, v2) in
  match (c-a), (d-b) with
    | (0, 1) -> North
    | (1, 0) -> East
    | (0, -1) -> South
    | (-1, 0) -> West
    | _ -> raise Impossible


(* gives all the directions contained in a subpath - (first != last) *)
let rec all_directions (arr: (int vertex) array)
    ((s,e) : int*int) : direction list =
  let n = Array.length arr in
  let (ns,ne) = ((s mod n),(e mod n)) in
  if (((ne-ns) mod n) = 0)
  then []
  else 
    let (v1,v2) = ((Array.get arr ns),(Array.get arr ((ns+1) mod n))) in
    (find_direction v1 v2) :: (all_directions arr (s+1,e))

	    
(* returns true if all four directions occur in the list *)
let four_directions (d: direction list) : bool =
  (* checks if one direction occurs *)
  let rec check_helper (d: direction list) (dir: direction) =
    match d with
      | [] -> false
      | hd :: tl -> (hd = dir) || (check_helper tl dir)
  in
  (check_helper d East) && (check_helper d West)
  && (check_helper d North) && (check_helper d South)


(* CHECKS IF A SUBPATH FAILS IN THE DIRECTION CONDITION - (fisrt != last) *)
let check_direction (arr: (int vertex) array) ((s,e) : int*int) : bool =
  not (four_directions (all_directions arr (s,e)))
;;



(*** 1.2 - CHECKING THE APPROXIMATION CONDITION ***)


(* converts an int vertex in a float vertex *)	  
let vertex_float (v: int vertex) : float vertex =
  let (x,y) = v in
  (float_of_int x, float_of_int y)
;;


(* Find the slope of a straight line between two endpoints. 
   It returns None if the line is vertical ("infinite" slope) *)
let slope (v1: int vertex) (v2: int vertex) : float option =
  let ((x1,y1),(x2,y2)) =  (vertex_float v1, vertex_float v2) in
  if (x1 = x2) then None
  else Some ((y2 -. y1)/.(x2 -. x1))
;;

      
(* Calculates the distance between a point p and the line
   defined by v1 and v2 *)
let dist_seg_pt (v1: int vertex) (v2: int vertex) (pt: int vertex) : float =
  let (px,py) = vertex_float pt in
  let ((x1,y1),(x2,y2)) = (vertex_float v1, vertex_float v2) in
  match (slope v1 v2) with
    | None -> abs_float(px-.x1)
    | Some s -> (abs_float (y1-.(s*.x1)-.(py-.s*.px)))/.(sqrt (s**2. +. 1.))
;;



(* CHECKS IF A SUBPATH CAN BE APPROXIMATED BY A LINE - (first != last) *)


(* initializes list of all pairs (i,j) with s<=i<j<=e with i!=n *)
let rec init_square_list (s: int) (e: int) : (int*int) list =
  (* returns [(st,en);(st+1,en);...;(en-1,en)] *)
  let rec init_line (st: int) (en: int) =
    if (st >= en) then []
    else (st,en) :: init_line (st+1) en
  in
  List.fold_right (fun x l -> (init_line s x) @ l) (nat_list s e) []
;; 

 
(* checks the triplewise condition for fixed i and j (see paper) *)
let rec check_triple (arr: (int vertex) array) (n: int)
    (c: int) ((s,e): (int*int)) : bool =
  let vc = Array.get arr (c mod n) in
  let vs = Array.get arr (s mod n) in
  let ve = Array.get arr (e mod n) in
  if (((c-e) mod n) = 0) then true
  else
    ((dist_seg_pt vs ve vc) <= 1.0) && 
      (check_triple arr n (c+1) (s,e))
;;


(* gets the length of a subpath starting at i (inlusive) 
   and ending at j (exclusive) *)
let cyclic_difference (i: int) (j: int) (length: int) : int =
  if (i<=j) then j-i else (j-i+length) 
;;


(* checks triplewise condition *)
let check_approximation (arr: (int vertex) array) ((s,e) : int*int) : bool =
  let n = Array.length arr in
  let lst = init_square_list (s mod n) ((s mod n)+(cyclic_difference s e n)) in
  List.fold_right (fun (x,y) b -> b && (check_triple arr n x (x,y))) lst true
;;



(*** 1.3 - TAKES A PATH AND RETURNS THE STRAIGHT SUBPATHS 
   (denoting the starting and ending indexes) ***)

(* checks if a subpath of a path (array) is straight *)
let check_straight (arr: (int vertex) array) ((s,e) : int*int) : bool =
  (check_direction arr (s,e)) && (check_approximation arr (s,e))
;;

(* inits the list of (i,j) with 0<=i,j<=n and i != j *)
let rec init_square_list2 (n: int) : (int*int) list =
  let rec init_line (fixed: int) (var: int) =
    if (var > n) then []
    else if (fixed = var) then init_line fixed (var+1)
    else (fixed,var) :: init_line fixed (var+1)
  in
  List.fold_right (fun x l -> l @ (init_line x 0)) (nat_list 0 n) []
;; 


(* RETURNS THE STRAIGHT SUBPATHS OF A PATH (list) - (first != last) *)
let straight_subpaths (arr: (int vertex) array) : (int*int) list =
  let n = Array.length arr in
  let lst = init_square_list2 (n-1) in
  List.filter (check_straight arr) lst
;;



(*** 1.4 - ADDS THE PENALTIES WITH THE STRAIGHT SUBPATHS ***)

(* sums the squared distances contained in the penalties formula *)
let summation (arr: (int vertex) array) ((s,e): int*int) =
  let rec summation_helper (arr: (int vertex) array) 
      (n: int) (c: int) ((s,e): int*int) =
    if (((c-e) mod n) = 0) then 0.
    else
      let (v1,v2) = ((Array.get arr (s mod n)),
		     (Array.get arr (e mod n))) in
      let pt = Array.get arr (c mod n) in
      ((dist_seg_pt v1 v2 pt)**2.) +. (summation_helper arr n (c+1) (s,e))
  in
  summation_helper arr (Array.length arr) s (s,e)
;;


(* calculates the distance between two vertices *)
let distance_pts (v1: int vertex) (v2: int vertex) : float = 
  let ((x1, y1),(x2, y2)) = (vertex_float v1, vertex_float v2) in
  sqrt((x2-.x1) *. (x2-.x1) +. (y2-.y1) *. (y2-.y1))

      
(* gets the length of a subpath starting at i (inlusive) 
   and ending at j (exclusive) *)
let cyclic_difference (i: int) (j: int) (length: int) : int =
  if (i<=j) then j-i else (j-i+length) 
;;


(* calculates the penalty for a straight line between two vertices *)
let penalty (arr: (int vertex) array) ((s,e) : int*int) : float =
  let n = Array.length arr in
  let (v1,v2) = ((Array.get arr (s mod n)),(Array.get arr (e mod n))) in
  let distance = distance_pts v1 v2 in
  let sum = summation arr (s,e) in
  let cyc = (cyclic_difference s e n) + 1 in
  distance *. (sqrt (sum /. (float_of_int cyc))) 
;;


(* RETURNS THE STRAIGHT SUBPATHS ALONG WITH THEIR PENALTIES - (first != last) *)
let straight_penalties (arr: (int vertex) array) : ((int*int) * float) list =
  let subpaths = straight_subpaths arr in
  let convert_subpath (arr: (int vertex) array) ((s,e) : int*int) =
    ((s,e), penalty arr (s,e))
  in
  List.map (convert_subpath arr) subpaths
;;
        


(*** 1.5 - GET THE GRAPH FROM THE PATH ***)


(************** CONVERTS A PATH TO A GRAPH ***************)
(* first = last in the path *)
let path_to_graph (p: (int vertex) list) : WIntGraph.graph = 
  let sides = (List.length p) - 1 in
  let arr = Array.sub (Array.of_list p) 1 sides in
  let graph = WIntGraph.init_graph sides in
  let subpaths = straight_penalties arr in
  let add_edge (((s,e),p): (int*int)*float) 
      (g: WIntGraph.graph) : WIntGraph.graph =
    WIntGraph.set_edge g s e (Weight(1,p))
  in
  List.fold_right add_edge subpaths graph

;;



(******************************************
 * PART 2 - GET OPTIMAL CYCLE FROM GRAPH  *
 ******************************************)
       

(* SORTING HELPER FUNCTIONS *)
let sort_by_value (n1,v1,p1,u1) (n2,v2,p2,u2) =
  match (u1,u2) with
    | (false, false) -> 0
    | (false, true) -> 1
    | (true, false) -> -1
    | (true, true) ->
      if ((WIntGraph.compare v1 v2) = Less) then -1
	else if ((WIntGraph.compare v1 v2) = Equal) then 0 
	else 1
;;
  
let sort_by_node (n1,v1,p1,u1) (n2,v2,p2,u2) = 
  if ((WIntGraph.compare_nodes n1 n2) = Less) then -1 
  else if ((WIntGraph.compare_nodes n1 n2) = Equal) then 0 
  else 1
;;




(**** 2.1 - Implementation of the Dijkstra Algorithm ****)


(* Our implementation of Djikstra algorithm *)
let rec algorithm (initial: WIntGraph.node) (destination: WIntGraph.node) (g: WIntGraph.graph)
    (current: (WIntGraph.node*WIntGraph.weight*(WIntGraph.node option)*bool))
    (unvisited: (WIntGraph.node*WIntGraph.weight*(WIntGraph.node option)*bool) array) =


  let (cn, cv, cp, cb) = current in

  let rec backtrack (p: WIntGraph.node) (init: int)
      (lst: WIntGraph.node list) : (WIntGraph.node list) = 
    let (n,_,prev,_) = Array.get unvisited p in
    match prev with
      | None -> lst
      | Some p -> backtrack p init (p::lst) in

  if (cn = destination)
  then 
    let (_,w,_,_) = Array.get unvisited destination in
    ((backtrack cn initial []),w)
  else
    let update n (v,p,u) : unit = 
      match (WIntGraph.get_edge g cn n) with
        | None -> ()
        | Some wt -> (if ((WIntGraph.compare (WIntGraph.sum wt cv) v) = Less) 
          then Array.set unvisited n (n,(WIntGraph.sum wt cv),Some cn,u)
          else ())
    in
     
    
    let rec update_neighbors lst =
      match lst with
	| [] -> () 
	| (hd::tl) -> 
	  let (n0,v0,p0,u0) = Array.get unvisited hd in
	  if (u0 = true) 
	  then (update hd (v0,p0,u0); update_neighbors tl; ())
      else update_neighbors tl 
    in
    
    
    let sort_by_value (n1,v1,p1,u1) (n2,v2,p2,u2) =
      match (u1,u2) with
        | (false, false) -> 0
        | (false, true) -> 1
        | (true, false) -> -1
        | (true, true) ->
	  if ((WIntGraph.compare v1 v2) = Less) then -1
	  else if ((WIntGraph.compare v1 v2) = Equal) then 0 
	  else 1
    in

    let sort_by_node (n1,v1,p1,u1) (n2,v2,p2,u2) = 
      if ((WIntGraph.compare_nodes n1 n2) = Less) then -1 
      else if ((WIntGraph.compare_nodes n1 n2) = Equal) then 0 
      else 1
    in

    (* update neighbors of current node with optimized "distances" *)
    let _ = update_neighbors (WIntGraph.outneighbors g cn) in
    (* sorts the unvisited array by "distances" *)
    let _ = Array.sort sort_by_value unvisited in

    (* gets the node with least "distance" *)
    let newcurrent = Array.get unvisited 0 in
    let (n0,v0,p0,u0) = newcurrent in

    (* gets the new unvisited array without the newcurrent *)
    let _ = Array.set unvisited 0 (n0,v0,p0,false) in
    let _ = Array.sort sort_by_node unvisited in 
    algorithm initial destination g newcurrent unvisited

;;




(**** 2.2 - Set up the variables to apply Djikstra ****)

(* gives a list with the integers from a to b, inclusive *)
let rec nat_list (a: int) (b: int) : int list  =
  if (b < a) then []
  else a :: (nat_list (a+1) b)
;;

(* sets up graph to apply the algorithm - returns the unvisited array *)
let set_unvisited (total: int) (start: int) : 
    ((WIntGraph.node*WIntGraph.weight*(WIntGraph.node option)*bool) array) =
  let nodes = nat_list 0 total in
  let lst = List.map (fun n -> (n,Infinity,None,true)) nodes in
  let arr = Array.of_list lst in
  let _ = Array.set arr total (total,Weight(0,0.),None,false) in
  arr
;;

let set_curr (n: int) = 
  (n,Weight(0,0.),None,false) 
;;



let optimal_cycle_with_start (g: WIntGraph.graph) (total: int) (start: int) =
  let newgraph = WIntGraph.clone_vertex g start in
  let (p,w) = algorithm total start newgraph (set_curr total) (set_unvisited total start) in
  (start :: (List.tl p) @ [start], w)
;;



(**** 2.3 - Applies Dijkstra in every point and gets optimal polygon ****)

(* Stores only the previous polygon with lowest weight, compares two at a time *)
let optimal_from_graph (g: WIntGraph.graph) (total: int) : int list =
    let compare (p1,w1) (p2,w2) =
        (if ((WIntGraph.compare w1 w2)=Less) then (p1,w1)
        else if (WIntGraph.compare w1 w2=Greater) then (p2,w2)
        else (p1,w1)) in
    let rec helper (g: WIntGraph.graph) (counter: int) (total: int) prev=
        (if (counter=(total-1)) then (let (p,_)= prev in p) else
        let next = (optimal_cycle_with_start g total (counter+1)) in
        if (counter=(total-2)) then (let (p,_)= (compare prev next) in p)
        else (let newprev = (compare prev next) in helper g (counter+1) total newprev))
    in
    let first = optimal_cycle_with_start g total 0 in
    helper g 1 total first
;;




(***** GETS THE OPTIMAL POLYGON FROM A PATH! *****)

let optimal_subpath_from_path (p: (int vertex) list) : int list =
  let total = (List.length p) - 1 in
  let g = path_to_graph p in
  let return = List.map (fun n -> (n+1) mod total) (optimal_from_graph g total) in
  return
;;


(* path has the first and the last vertices equal *) 
let optimal_polygon (p: (int vertex) list) : (int vertex) list =
  let total = (List.length p) - 1 in
  let arr = Array.of_list (List.tl p) in
  let g = path_to_graph p in
  let path = optimal_from_graph g total in
  List.map (fun n -> Array.get arr n) path
;; 




