open Types;;
open Print;;
open Graphmatrix;;


exception Wrong of string;;

(*******************************
 * PART 1 - VERTEX ADJUSTMENTS *
 *******************************)

(****** 1.1 helper functions *****)

exception Arith0
(* finds the arithmetic mean of a list of ints *)
let arith_mean (l: int list) : float =
  let sum = List.fold_right (+) l 0 in
  let n = List.length l in
  if (n <= 0) then raise Arith0
  else
    (float_of_int sum) /. (float_of_int n)
;;


(* splits a list of pairs into two lists of (x coords and y coords) *)
let rec split (l: (int * int) list) : (int list) * (int list) = 
  match l with
    | [] -> ([],[])
    | (x,y) :: tl -> 
      let (xs, ys) = split tl in
      (x::xs, y::ys)
;;

(* finds constants for the 2x2 matrix that helps to approximate lines *)
let find_constants (l: (int * int) list) : (float*float*float) =
  let (xs, ys) = split l in
  let squared_xs = List.map (fun x -> x*x) xs in
  let squared_ys = List.map (fun x -> x*x) ys in
  let prod = List.map (fun (x,y) -> x*y) l in
  let a = (arith_mean squared_xs) -. (arith_mean xs) *. (arith_mean xs)  in
  let b = (arith_mean prod) -. ((arith_mean xs)*.(arith_mean ys)) in
  let c = (arith_mean squared_ys) -. (arith_mean ys) *. (arith_mean ys) in
  (a,b,c)
;;


(* find eigenvalue of the matrix ((a,b)^T,(b,c)^T) *)
let find_eigenvalue (a: float) (b: float) (c: float) : float =
  ((a+.c)+.(sqrt ((a-.c)**2. +. 4.*.(b**2.))))/.2.
;;

(* find the eigenvector of the matrix ((a,b)^T,(b,c)^T) *)
let get_eigenvector (a: float) (b: float) (c: float) : (float*float) =
  let lambda = find_eigenvalue a b c in
  if (not (c = 0.)) then (lambda -. c, b)
  else if (not (b = 0.)) then (b, lambda -. a) 
  else (0.,0.)
;;
  
exception EmptyList

(* gets the gravity center of a list of points *)  
let gravity_ctr (l: (int*int) list) : (float*float) = 
    match l with 
    | [] -> raise EmptyList
    | _ -> let (xs,ys) = split l in
        (arith_mean xs, arith_mean ys)
;;

(* returns the first n elements of a vertex list and
   the nth element cons to the rest of the original list *)
exception ContractViolation

let extract_n (v: 'a list) (n: int) : (('a list) * ('a list)) =
  let rec sub_extract (v: 'a list) (n: int) (accum: 'a list) : ('a list * 'a list) =
    if (n <= 0) then (if ((List.length accum)= 0) then ([], v)
                         else (List.rev accum, (List.hd accum)::v))
    else
      match v with
        | [] ->  raise ContractViolation
        | hd :: tl -> sub_extract tl (n - 1) (hd::accum) in  
  sub_extract v n []
;;

(* distance between vertex and float point*)  
let dist_floats (v1 : int vertex) (v2 : float vertex) : float =
     let ((x1a, y1a), (x2, y2)) = (v1, v2) in
     let ((x1, y1)) = ((float_of_int x1a, float_of_int y1a)) in
         sqrt((x2-.x1) *. (x2-.x1) +. (y2-.y1) *. (y2-.y1))
;;
         
         
(* gets the line between returns a list of (center_gravity, slope, vertex from the polygon that comes after the gravity center) 
   indices at which we find the optimized polygon vertices *)         
let get_lines (indices: int list) (old: int vertex list) : (line * int vertex) list =  
  let rec rec_get_lines (indices: int list) (old: int vertex list) (accum: (line * int vertex) list): (line * int vertex) list = 
    match indices with
      | [] | _::[] -> List.rev accum
      | hd1 :: hd2 :: tl ->  
        let (extracted, rest) =  extract_n old (hd2 - hd1 + 1) in
        match extracted with 
        | [] -> Printf.printf "%i %i" hd1 hd2; []
        | hd :: tl1 -> 
        
        let (a, b, c) =  find_constants extracted in
        let _ = if (extracted = []) then Printf.printf "%i %i" hd1 hd2 in
        let center_gravity = gravity_ctr extracted in
        let eigenvector = get_eigenvector a b c in
        match rest with
            | [] -> raise (Wrong "GET_LINES")
            | hdd :: tll -> 
        rec_get_lines (hd2::tl) rest (((center_gravity, eigenvector), List.hd rest) :: accum) 
  in      
  rec_get_lines indices old []
;;
         
 
(* generate the new approximate point *)
let rec gen_ak (original: int vertex) (test: float vertex) : float vertex =
  if ((dist_floats original test) < 0.5) then test 
  else (   
    let ((x1a, y1a), (x2, y2)) = (original, test) in
    let ((x1, y1)) = ((float_of_int x1a, float_of_int y1a)) in
    if (x2 = x1) then 
      (if (y2 > y1) then (x1, y1 +. 0.5) else (x1, y1 -. 0.5))
    else 
      let m = (y2 -. y1)/. (x2-. x1) in
      if (x2 > x1) then 
        let x = x1 +. (0.5 /. sqrt(1. +. m *.m)) in 
        let y = y1 +. m *. (x -. x1) in
        (x, y)
      else 
        let x = x1 -. (0.5 /. sqrt(1. +. m *.m)) in 
        let y = y1 +. m *. (x -. x1) in
        (x, y))
;;

exception DivideZero of int ;;

(* finds the intersection*)
let rec find_intersection (line1: line) (line2: line) : float vertex = 
    let ((x1, y1), m1) = line1 in
    let ((x2, y2), m2) = line2 in
    match m2 with
    | (0. , _) -> 
        let (a, b) = m1 in
        if(b=0. ) then (x1, y2)  
        else (
            let s2 = a /. b in
            (((y2-. y1)/. s2) +. x1, y2)) 
    | (_, 0.) -> 
        let (a, b) = m1 in
        if (a = 0.) then (x2, y1)
        else (
         if(b=0. ) then raise (DivideZero 1) else 
        (let s1 = a /. b in
        (x2, y1 +. s1 *. (x2 -. x1))))

    | _ ->
 
      let (a1, b1) = m1 in
      let (a2, b2) = m2 in
      if (b1 =0.) then (
	let k2 = a2 /. b2 in
	(x1, y2-.(x2-.x1) *. k2))
      else 
      let s1 = a1 /. b1 in
      let s2 = a2 /. b2 in
      let _ = if (s1=s2) then raise (DivideZero 3) in
      let x = (y1 -. y2 +. (s2 *. x2) -. (s1 *. x1)) /. (s2 -. s1) in
      let y = y1 +. s1 *. (x -. x1) in
      (x, y)
;;

(* shifts a1 to front of a2*)
let gen_final (oldlst: (line * int vertex) list) : float vertex list = 
  let _ = 
    match oldlst with
    | [] -> raise (Wrong "gen_final")
    | _ -> ()
    in
  let lst = oldlst @ [List.hd oldlst] in
  let rec sub_gen (lst2: (line * int vertex) list) (accum: float vertex list): float vertex list = 
    match lst2 with
      | [] | _:: [] -> accum
      | hd1::hd2::tl -> let (a, v) = hd1 in
         let (p1, m1) = a in 
         let (b, _) = hd2 in
         let (p2, m2) = b in
         let test = find_intersection (p1, m1) (p2, m2) in
         let d = gen_ak v test in
         sub_gen (hd2::tl) (d::accum) 
  in
  let final =sub_gen lst [] in
  let final2 = Array.of_list final in
  (List.nth final ((List.length final)-1) ) :: (Array.to_list (Array.sub final2 0 ((List.length final) - 1)))
;;
 
exception NonCyclicPolygon



(* shifts the polygon to have 0 as first vertex *)
let shift_subpath (path: (int vertex) list) (sub: int list) =
  let newpath = List.rev (List.tl (List.rev path)) in 
  let total = (List.length newpath) in
  let _ = 
    match sub with
    | [] -> raise (Wrong "gen_final")
    | _ -> ()
    in
  let first = List.hd sub in
  let (l1,l2) = extract_n newpath (first+1) in
  let indices = List.map (fun n -> (n-first + total) mod total) sub in

  (*Printf.printf "PATH: "; print_list print_vertex newpath; newline();
  Printf.printf "SUB: "; print_list print_int sub; newline();
  Printf.printf "NEWPATH: "; print_list print_vertex (l2@l1); newline();
  Printf.printf "INDICES: "; print_list print_int indices; newline();*)
  let return = 
    match (List.rev indices) with
      | [] -> (l2@l1, [])
      | h::t -> (l2@l1, List.rev (total::t))
  in
  (*let ret1,ret2 = return in*)
  (*Printf.printf "NEWPATH: "; print_list print_vertex ret1; newline();
  Printf.printf "INDICES: "; print_list print_int ret2; newline();*)
  return
;;
  
  

(*********************** VERTEX ADJUSTMENT FUNCTION ********************************)

(* gets an int polygon and spits out a float polygon; disregards polygons that have 2 or less vertices
   remember that the polygons are cyclic, so the first vertex repeats at the end 
   polygon:[a0,a1,a2,a3] indices:[0,1,3] where a0 = a3 || indicies always start at 0*)
let convert_polygon (sub: int list) (path: int vertex list) : float vertex list =
  let (p,indices) = shift_subpath path sub in
  if (List.length indices <= 2) then raise NonCyclicPolygon
  else 
    let line_list = get_lines indices p in
    gen_final line_list

;;
        

(* PART 2 - SMOOTHING AND CORNER ANALYSIS *)

(* slope of the line between two float vertices*)
let slope_tuple (v1: float vertex) (v2: float vertex) : (float * float)  =
  let ((x1,y1),(x2,y2)) = (v1,v2) in
    ((y2-.y1), (x2-.x1))

(* finds the distance of a point from the line defined by (a1 and a2) *)

exception DistLinePt0
let dist_line_pt (i: float vertex) (a1: float vertex) (a2: float vertex):float=
  let (i1,i2)= i in
  let (x1,y1)= a1 in
  let (x2,y2)= a2 in
  let (m1, m2) = slope_tuple a1 a2 in
    if (x1=x2) then (i1-. x1)
    else (
       if (m2 = 0.) then raise DistLinePt0
       else
         (abs_float (i2-.((m1/. m2) *.i1)-.(y1-.(m1/. m2)*.x1)))/.(sqrt ((m1/.m2)**2. +. 1.))
    )
;;                         
                
(* distance between two float points *)  
let dist_floatvertices (v1 : float vertex) (v2 : float vertex) : float =
     let ((x1, y1), (x2, y2)) = (v1, v2) in
     sqrt((x2-.x1) *. (x2-.x1) +. (y2-.y1) *. (y2-.y1))
;;

exception Impossible
(*takes the polygon from step 2.3.1 and inserts the midpoints of the segments in between the points
  in the form [a1;b1;a2;b2;a3;b3;...;a1], where [a1;a2...] is the original polygon*)
let find_midpoints (old: float vertex list) : float vertex list =
    let _ = match old with
        | [] -> raise (Wrong "find_mid")
        | _ -> ()
        in
        
  let rec rec_find_midpoints (v: float vertex list) (accum: float vertex list) : float vertex list =
    match v with
      | (x1, y1) :: [] -> let (x2, y2) = (List.hd old) in
        (List.rev accum)@((x1, y1)::((x1 +. x2)/. 2., (y1 +. y2)/. 2.) :: (x2, y2)::[]) 
      | (x1,y1)::(x2,y2)::t -> let m = ((x1+.x2)/.2., (y1+.y2)/.2.) in
                                 rec_find_midpoints ((x2,y2)::t) (m::(x1,y1)::accum) 
      | [] -> []
  in
  rec_find_midpoints old []
;;

let rec print_coord m = 
    let x = match m with
    | (a, b) -> (string_of_float a) ^","^(string_of_float b) ^ "\n" in
        let _ = Printf.printf "%s" x in
        ()
;;
    
(* finds alpha *)
let find_a (b1: float vertex) (a: float vertex) (b2: float vertex): float = 
    let (ax, ay) = a in
    let gamma = 
      let possible = [dist_line_pt (ax +. 0.5, ay -. 0.5) b1 b2;
		      dist_line_pt (ax -. 0.5, ay +. 0.5) b1 b2;
		      dist_line_pt (ax +. 0.5, ay +. 0.5) b1 b2;
		      dist_line_pt (ax -. 0.5, ay -. 0.5) b1 b2]
      in
      let less = List.hd (List.sort compare possible) in
      less /. (dist_line_pt a b1 b2)
    in
    let alpha =(4./.3.)*.gamma in
    if (alpha <= 0.55) then 0.55 else alpha
;;

(* finds alpha
let find_a (b1: float vertex) (a: float vertex) (b2: float vertex): float = 
    let (ax, ay) = a in
    let c = 
      match (slope_tuple b1 b2) with 
	| (0., _) -> if ((dist_line_pt (ax, ay +. 0.5) b1 b2) < (dist_line_pt (ax, ay -. 0.5) b1 b2))
          then (ax, ay +. 0.5) else (ax, ay -. 0.5)
	| (_, 0.) -> if ((dist_line_pt (ax -. 0.5, ay) b1 b2) < (dist_line_pt (ax +. 0.5, ay) b1 b2))
          then (ax -. 0.5, ay) else (ax +. 0.5, ay)
	| (a, b) -> 
          if ((a /. b)>0.) then 
            if ((dist_line_pt (ax +. 0.5, ay -. 0.5) b1 b2) < (dist_line_pt (ax -. 0.5, ay +. 0.5) b1 b2))
            then (ax +. 0.5, ay -. 0.5) else (ax -. 0.5, ay +. 0.5)
          else 
            if ((dist_line_pt (ax +. 0.5, ay +. 0.5) b1 b2) < (dist_line_pt (ax -. 0.5, ay -. 0.5) b1 b2))
            then (ax +. 0.5, ay +. 0.5) else (ax -. 0.5, ay -. 0.5)
    in
    (4. /. 3.) *. ((dist_floatvertices b1 c) /. (dist_floatvertices b1 a))
;;
*)


exception Colinear
exception Impossible

(* takes 3 points and deduces the afine map (midpoint1, midpoint2, vertex) *)
let transform ((x1,y1) : point) ((x2,y2) : point) ((x3,y3) : point) : float*float*float*float*float*float =  
  let find_ace  ((x1,y1) : point) ((x2,y2) : point) ((x3,y3) : point) =
    if ((2. *. y3 = y1 +. y2) && (2. *. x3 = x1 +. x2) && (not (x2 = x3))) then raise Colinear
    else if  (not (x2 *. y1 -. (x3 *. y1) -. (x1 *. y2) +. (x3 *. y2) +. (x1 *. y3) -. (x2 *. y3) = 0.) && not (x1 = x3)) then
       let e = (x1 +. x2 -. 2. *. x3)/.(-.x2 *. y1 +. x3 *. y1 +. x1 *. y2 -. x3 *. y2 -. x1 *. y3 +.  x2 *. y3)  in
       let c = (-. 1. -. (e *. y1) +. (e *. y3))/.(x1 -. x3) in
       let a = -. (c *. x3) -. (e *. y3) in
       (a,c,e)
    else if (x1 = x3 && (not (y1 = y3)) && (not (x2 = x3))) then 
       let e = 1./.(-.y1 +. y3) in
       let c = (1. -. (e *.y2) +. (e *. y3))/.(x2 -. x3) in
       let a = -.(c *. x3) -. (e *. y3) in
       (a,c,e)
     else if ((y1 = -. y2 +. (2. *. y3)) && x2 = x3 && x1 = x3 && (not (y2 = y3))) then raise Colinear
     else let _ = newline(); print_float_vertex (x1,y1); newline(); print_float_vertex (x2,y2); newline(); print_float_vertex (x3,y3) in
     raise (Wrong "FIND ACE")
   in
    
   let find_bdf ((x1,y1) : point) ((x2,y2) : point) ((x3,y3) : point) =
     if (y1 = y2 && x1 = x2) then raise Colinear
     else if (not (x2 *. y1 -. x3*. y1 -. x1 *. y2 +. x3 *. y2 +. x1 *. y3 -. x2 *. y3 = 0. && (not (y1 = y3))) ) then 
        let d = (-.y1 +. y2)/.(x2 *. y1 -. x3 *. y1 -. x1 *. y2 +. x3 *. y2 +. x1 *. y3 -. x2  *. y3) in
        let f  = (-1. -. (d *. x1) +. (d *. x3))/.(y1 -. y3) in
        let b = 1. -. (d *. x3) -. (f *. y3) in    
        (b,d,f)
        
     else if (y1 = y3 && (not (x1 = x3)) && (not (y2 = y3)) ) then 
        let d =1./.(-.x1 +. x3) in
        let f  = (-1. -. (d *. x2) +. (d *. x3))/.(y2 -. y3)  in
        let b = 1. -. (d *. x3) -. (f  *. y3) in
        (b,d,f)
     else if (y2 = y3 && y1 = y3 && x1 = x2) then raise Colinear
     else raise (Wrong "FIND BDF")
   in 
   let (a,c,e) = find_ace (x1,y1) (x2,y2) (x3,y3) in
   let (b,d,f) = find_bdf (x1,y1) (x2,y2) (x3,y3) in
   (a,b,c,d,e,f)
;;

exception GetControl0
let get_control (a,b,c,d,e,f) alpha : point*point =
  let det = c*.f -. d*.e in
  if (det = 0.) then raise GetControl0
  else
    let x0 = ((1. -. alpha -. a)*.f +. (alpha -. b)*.(-.e))/.det in
    let y0 = ((1. -. alpha -. a)*.(-.d) +. (alpha -. b)*.c)/.det in
    let x1 = ((-1. +. alpha -. a)*.f +. (alpha -. b)*.(-.e))/.det in
    let y1 = ((-1. +. alpha -. a)*.(-.d) +. (alpha -. b)*.c)/.det in
    ((x1,y1),(x0,y0))
;;

  

exception SmallAdjusted
let convert_bezier (adjusted: float vertex list) : vector =
  let rec rec_convert_bezier (points_and_mids: float vertex list)  (lst: (curve*point) list) : (curve*point) list = 
    match points_and_mids with 
      | m1::a::m2::tl -> 
	let alpha = find_a m1 a m2 in
	let floats = transform m1 m2 a in
	let ctr1,ctr2 = get_control floats alpha in
	if (alpha <= 0.9)
	then rec_convert_bezier (m2::tl) ((Bezier (ctr1,ctr2), m2) :: lst)
	else  rec_convert_bezier (m2::tl) ((Line,m2) :: ((Line,a) :: lst))
      | _ -> lst 
  in
  let lst = find_midpoints adjusted in
  match (lst) with
    | hd :: tl -> (hd, List.rev (rec_convert_bezier tl []))
    | _ -> raise SmallAdjusted
;; 

        
          
let polygon_to_vector (p: int path) (polygon: int list) : vector*biColor =
  let path,color = p in
  (convert_bezier (convert_polygon polygon path),color)
;;
