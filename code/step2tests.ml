open Types ;;
open Graphmatrix ;;
open Step2 ;;
open Print;;


let init_graph_with_edges (total: int) (edges: (WIntGraph.node*WIntGraph.node*float) list) = 
  let graph = WIntGraph.init_graph total in
  List.fold_right (fun (x,y,w) g -> WIntGraph.set_edge g x y (Weight(1,w))) edges graph
;;

let set_curr (n: int) = 
  (n,Weight(0,0.),0,false) 
;;

let print_dijkstra out =
  let (lst,w) = out in
  let str = 
    match w with
      | Infinity -> "infinity?"
      | Weight(x,y) -> Printf.sprintf "%f" y
  in
  print_list print_int lst; Printf.printf " W: %s" str
;;


let rec print_unvisited (unv: (WIntGraph.node*WIntGraph.weight*WIntGraph.node*bool) array) : unit = 
  print_array (fun (n,w,m,b) -> (Printf.printf "("; print_int n;  Printf.printf ","; print_weight w;
     Printf.printf ","; print_int m;  Printf.printf ","; print_bool b;  Printf.printf ")")) unv; ()
;;


(*

let run_tests () = 
(*
  let init = 0 in
  let dest = 4 in
  let curr = (0,Weight(0,0.0),0,false) in
  let unv =  Array.make 5 (0,Weight(0,0.0),0,false) in
  let _ = Array.set unv 1 (1,Infinity,0,true) in
  let _ = Array.set unv 2 (2,Infinity,0,true) in
  let _ = Array.set unv 3 (3,Infinity,0,true) in
  let _ = Array.set unv 4 (4,Infinity,0,true) in
  let graph = WIntGraph.init_graph 5 in
  let graph = WIntGraph.set_edge graph 0 1 (Weight(1,1.5)) in
  let graph = WIntGraph.set_edge graph 1 2 (Weight(1,0.6)) in
  let graph = WIntGraph.set_edge graph 2 3 (Weight(1,1.0)) in
  let graph = WIntGraph.set_edge graph 3 4 (Weight(1,1.0)) in
  let output = algorithm init dest graph curr unv in
  Printf.printf "Test 1: "; 
  print_dijkstra output;
  let init = 0 in
  let dest = 2 in
  let curr = (0,Weight(0,0.0),0,false) in
  let unv = Array.make 5 (0,Weight(0,0.0),0,false) in
  let _ = Array.set unv 1 (1,Infinity,0,true) in
  let _ = Array.set unv 2 (2,Infinity,0,true) in
  let _ = Array.set unv 3 (3,Infinity,0,true) in
  let _ = Array.set unv 4 (4,Infinity,0,true) in
  let graph = WIntGraph.init_graph 5 in
  let graph = WIntGraph.set_edge graph 0 1 (Weight(1,1.)) in
  let graph = WIntGraph.set_edge graph 0 4 (Weight(1,5.)) in
  let graph = WIntGraph.set_edge graph 4 2 (Weight(1,1.)) in
  let graph = WIntGraph.set_edge graph 1 2 (Weight(1,2.)) in
  let output = algorithm init dest graph curr unv in
  Printf.printf "\nTest 2: "; print_dijkstra output;;*)

(* 3 doesn't work *)
  (*let graph = init_graph_with_edges 6 [(0,1,1.5);(1,2,2.);(2,0,3.);(0,4,2.);(4,5,3.);(5,0,2.)] in
  (* let unv = set_unvisited 6 0 in *)
  let output = optimal_cycle_with_start graph 6 0 in
  Printf.printf "\nTest 2: "; print_dijkstra output; Printf.printf "\n";*)

(*
  newline (); 
  print_float (dist_seg_pt (2,5) (2,5) (4,5));
  newline ();
  let path = [(1,0);(1,1);(1,2);(1,3);(1,4);(2,4);(2,5);(3,5);(4,5);(4,4);(4,3);(4,2);(4,1);(4,0);(3,0);(2,0);(1,0)] in
  let arr = Array.sub (Array.of_list path) 1 ((List.length path)-1) in 
  Printf.printf "\nGRAPH: \n"; WIntGraph.print_graph (path_to_graph path);
newline (); newline ();
  Printf.printf "\nTEST POLYGON: "; print_list print_vertex (optimal_polygon path)
*)

(*let newpath = [(0,0);(0,1);(0,2);(1,2);(1,1);(1,0);(0,0)] in
let graph = path_to_graph newpath in
Printf.printf "\nGRAPH: \n"; WIntGraph.print_graph (graph);
<<<<<<< HEAD
let opt,_ = optimal_cycle_with_start graph 4 3 in
Printf.printf "\nOpt w/ start 2: \n"; print_list print_int opt; Printf.printf "\n";;
*)
(*let start = 0 in
let opt,_ = optimal_cycle_with_start graph 6 start in
Printf.printf "\nOpt w/ start %d: \n" start; print_list print_int opt; Printf.printf "\n";;*)
(*
let npath = [(0,0);(0,1);(0,2);(0,3);(1,3);(1,4);(1,5);(0,5);(0,6);(1,6);(1,7);(2,7);(2,6);(3,6);(3,7);(4,7);(4,6);(4,5);(4,4);(4,3);(4,2);(3,2);(3,1);(3,0);(2,0);(2,1);(2,2);(1,2);(1,1);(1,0);(0,0)] in
*)

*)


(*let npath = [(0,3);(1,3);(1,4);(1,5);(2,5);(3,5);(4,5);(4,4);(4,3);(4,2);(3,2);(3,1);(3,0);(2,0);(2,1);(2,2);(1,2);(1,1);(1,0);(0,0);(0,1);(0,2);(0,3)] in 


  (*let npath = [(0,0);(0,1);(0,2);(1,2);(2,2);(2,1);(2,0);(1,0);(0,0)] in *)

let g = optimal_polygon npath in 
Printf.printf "\nOptimized: \n"; print_list (fun (x,y)-> Printf.printf "%d, %d" x y) g; Printf.printf "\n";*)

  (*let g = path_to_graph [(0,0);(0,1);(0,2);(1,2);(2,2);(2,1);(2,0);(1,0);(0,0)] in

  print_list print_int (optimal_from_graph g 8);*)

(*Printf.printf "\n\n\n***** GRAPH *****\n\n";
WIntGraph.print_graph (g); 
Printf.printf "\n\n\n***** CLONED GRAPH *****\n\n";
WIntGraph.print_graph (WIntGraph.clone_vertex g 0); *)
(*
Printf.printf "\n\nOUTNEIGHBORS OF 22: "; print_list print_int (WIntGraph.outneighbors (WIntGraph.clone_vertex g 0) 22); newline();

Printf.printf "\n\n\n***** UNVISITED *****\n\n";
print_unvisited (set_unvisited ((List.length npath)-1) 0);

*)

(*Printf.printf "\n End of Graph \n";
(*let opt,_ = optimal_cycle_with_start g ((List.length npath) -1) 3 in*)
let opt = optimal_from_graph g ((List.length npath) -1) in  
Printf.printf "\nOptimized: \n"; print_list print_int opt; Printf.printf "\n";
Printf.printf "\nPolygon: \n"; print_list print_vertex (optimal_polygon npath); Printf.printf "\n";
*)

(*let total = (List.length npath) - 1 in
let arr = Array.of_list (List.tl npath) in
Printf.printf "\nArray: \n"; print_array print_vertex arr*)

  (*let npath = [(7,14);(7,15);(7,16);(7,17);(7,18);(7,19);(7,20);(7,21);(7,22);(7,23);(7,24);(8,24);(8,25);(8,26);(9,26);(9,27);(9,28);(10,28);(10,29);(11,29);(11,30);(12,30);(13,30);(13,31);(14,31);(15,31);(15,32);(16,32);(17,32);(18,32);(19,32);(20,32);(21,32);(22,32);(23,32);(24,32);(25,32);(26,32);(26,31);(27,31);(28,31);(28,30);(29,30);(30,30);(30,29);(31,29);(31,28);(32,28);(32,27);(32,26);(33,26);(33,25);(33,24);(34,24);(34,23);(34,22);(34,21);(34,20);(34,19);(34,18);(34,17);(34,16);(34,15);(34,14);(33,14);(33,13);(33,12);(32,12);(32,11);(32,10);(31,10);(31,9);(30,9);(30,8);(29,8);(28,8);(28,7);(27,7);(26,7);(26,6);(25,6);(24,6);(23,6);(22,6);(21,6);(20,6);(19,6);(18,6);(17,6);(16,6);(15,6);(15,7);(14,7);(13,7);(13,8);(12,8);(11,8);(11,9);(10,9);(10,10);(9,10);(9,11);(9,12);(8,12);(8,13);(8,14);(7,14);] in
*)
(*
let npath = [(3,20);(3,21);(3,22);(3,23);(4,23);(5,23);(6,23);(6,22);(7,22);(7,21);(7,20);(7,19);(7,18);(7,17);(7,16);(7,15);(7,16);(7,17);(6,17);(6,18);(5,18);(5,19);(4,19);(4,20);(3,20)] in
*)
(* (23,27);(24,27);(25,27);(26,27);(27,27);(28,27);(29,27);(30,27);(31,27);(32,27);(32,26);(31,26);(31,25);(31,24);(30,24);(30,23);(30,22);(30,21);(30,20);(30,19);(30,18);(29,18);(29,17);(29,16);(29,15);(29,14);(29,13);(29,12);(28,12);(28,11);(28,10);(28,9);(27,9);(27,8);(27,7);(27,6);(27,5);(26,5);(26,6);(26,7);(25,7);(25,8);(25,9);(24,9);(24,10);(23,10);(23,11); *)
(*
let g = path_to_graph npath in 
Printf.printf "\n\n\n***** CLONED GRAPH *****\n\n";
WIntGraph.print_graph (WIntGraph.clone_vertex g 0);

Printf.printf "\n\n\n***** CLONED GRAPH *****\n\n";
print_list print_int (optimal_from_graph g);



;; *)
(*Printf.printf "\nTEST POLYGON: "; print_list print_vertex (optimal_polygon npath)*)

(*let (opt,w) = optimal_cycle_with_start g ((List.length npath)- 1) 4 in

(*let opt = optimal_from_graph g ((List.length npath) -1) in *)
Printf.printf "\nOptimized: \n"; print_list print_int opt; Printf.printf "\n";
Printf.printf "\nFINAL WEIGHT: \n"; print_weight w*)

(*
let g = path_to_graph npath in

(*Printf.printf "\nGRAPH: \n"; WIntGraph.print_graph (g);
Printf.printf "\n End of Graph \n";*)
Printf.printf "\nOpt w/ start 1: \n";
let opt,_ = optimal_cycle_with_start g 30 13 in
Printf.printf "\nOpt w/ start 2: \n"; print_list print_int opt; Printf.printf "\n";;
*)

(* let path2 = [(2,2);(3,2);(3,3);(3,4);(3,5);(2,5);(2,4);(2,3);(2,2)] in
Printf.printf "\nGRAPH: \n"; WIntGraph.print_graph (path_to_graph path2); 
Printf.printf "\nTEST POLYGON: "; print_list print_vertex (optimal_polygon npath); *)


(*

  newline ();
  print_bool (check_approximation arr (5,7));
  print_float (dist_seg_pt (2,5) (4,5) (4,5)); *)
(*
let _ = run_tests ();; 
*)
(*

let graphh = ref WIntGraph.empty ;;
let total = ref 0 ;;

let find_graph () = 

(*
  let npath = [(10,0);(10,1);(11,1);(12,1);(12,2);(13,2);(14,2);(15,2);(15,3);(16,3);(17,3);(17,4);(16,4);(16,5);(15,5);(15,6);(16,6);(17,6);(18,6);(19,6);(20,6);(20,5);(20,4);(20,3);(20,2);(20,1);(20,0);(19,0);(18,0);(17,0);(16,0);(15,0);(14,0);(13,0);(12,0);(11,0);(10,0)] in

*)
  let npath = [(7,14);(7,15);(7,16);(7,17);(7,18);(7,19);(7,20);(7,21);(7,22);(7,23);(7,24);(8,24);(8,25);(8,26);(9,26);(9,27);(9,28);(10,28);(10,29);(11,29);(11,30);(12,30);(13,30);(13,31);(14,31);(15,31);(15,32);(16,32);(17,32);(18,32);(19,32);(20,32);(21,32);(22,32);(23,32);(24,32);(25,32);(26,32);(26,31);(27,31);(28,31);(28,30);(29,30);(30,30);(30,29);(31,29);(31,28);(32,28);(32,27);(32,26);(33,26);(33,25);(33,24);(34,24);(34,23);(34,22);(34,21);(34,20);(34,19);(34,18);(34,17);(34,16);(34,15);(34,14);(33,14);(33,13);(33,12);(32,12);(32,11);(32,10);(31,10);(31,9);(30,9);(30,8);(29,8);(28,8);(28,7);(27,7);(26,7);(26,6);(25,6);(24,6);(23,6);(22,6);(21,6);(20,6);(19,6);(18,6);(17,6);(16,6);(15,6);(15,7);(14,7);(13,7);(13,8);(12,8);(11,8);(11,9);(10,9);(10,10);(9,10);(9,11);(9,12);(8,12);(8,13);(8,14);(7,14)] in 

  let _ = total := List.length npath in

  let _ = graphh := path_to_graph npath in 
  Printf.printf "\n\n\n***** CLONED GRAPH *****\n\n";
  WIntGraph.print_graph (WIntGraph.clone_vertex (!graphh) 0)

;;


let path_graph () =

Printf.printf "\n\n\n***** OPTIMAL PATH  *****\n\n";
print_list print_int (optimal_from_graph (!graphh) (!total-1));

;;

let _ = find_graph () ;;
let _ = path_graph () ;;
*)

