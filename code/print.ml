open Types;;
(* PRINT HELPER FUNCTIONS *)

let print_list (print: 'a -> unit) (lst: 'a list) : unit =
  let rec print_list_h print lst =
    match lst with
      | [] -> Printf.printf ""
      | hd::tl -> print hd; Printf.printf ";"; print_list_h print tl
  in
  Printf.printf "["; print_list_h print lst; Printf.printf "]"
;;

let print_vertex (x,y) = Printf.printf "(%d,%d)" x y ;;
let print_float_vertex (x,y) = Printf.printf "(%f,%f)" x y ;;
let print_int n = Printf.printf "%d" n ;;
let print_bool b = if (b) then Printf.printf "true" else Printf.printf "false" ;;
let print_float f = Printf.printf "%f" f ;;

let print_dijkstra out =
  let (lst,w) = out in
  let str = 
    match w with
      | Infinity -> "infinity?"
      | Weight(x,y) -> Printf.sprintf "%f" y
  in
  print_list print_int lst; Printf.printf " W: %s" str
;;

let print_array (print: 'a -> unit) (arr: 'a array) : unit =
    Printf.printf "[|";
    Array.fold_left (fun () x -> (print x); Printf.printf ",\n") () arr;
    Printf.printf "|]"
;;

let print_weight (w: inf_weight) =
        match w with
        | Infinity -> Printf.printf "Infinity"
        | Weight (i,f) -> Printf.printf "Weight(%d,%f)" i f
;;

let newline () = Printf.printf "\n"; () ;;

let print_vector_element ((c,(x,y)): curve * point) : unit =
  match c with
  | Line -> Printf.printf "\nPoint:(%f,%f) Line" x y
  | Bezier ((x0,y0),(x1,y1)) -> Printf.printf "\nPoint:(%f,%f) Bezier of Points:(%f,%f) (%f,%f))" x y x0 y0 x1 y1
;;


  
   (*
let rec print_list m = 
    match m with
    | [] -> ()
    | hd::tl -> let x = match hd with
            | (a, b)-> (string_of_float a) ^","^(string_of_float b) ^ "\n" in
        let _ = Printf.printf "%s" x in
    print_list tl 
;;*)
(*
let print_curve (l: (curve * point) list) : unit =
  print_list print_vector_element l
;;*)

