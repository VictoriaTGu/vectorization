open Step3;;
open Print;;
open Types;;

let test_arith_mean () = 
    let a = [1;3;3;1] in
    assert(arith_mean a = 2.0);
    ()
;;
    
let test_split() =
  let l1 = [] in
  let l2 = [(1,2);(3,4)] in
  assert (split l1 = ([],[]));
  assert (split l2 = ([1;3],[2;4]));
  ()
;;
         
let test_find_constants () =
  let lst1 = [(3, 2); (2, 4); (1, 5); (4, 7)] in
  assert(find_constants lst1 = (1.25, 0.5, 3.25));
  ()
;;
 
let test_find_eigenvalue () =
  let e = find_eigenvalue 1. 3. 1. in
  assert (e = 4.);
  ()
;;
  
let test_get_eigenvector () = 
  let e = get_eigenvector 1. 3. 1. in
  assert (e = (3.,3.));
  ()
;;
 
(* Cannot put in an empty list!*)
let test_gravity_ctr () =
  let lst = [(1,3);(3,1);(3,1);(1,3)] in
  assert ((gravity_ctr lst)=(2., 2.));
  ()
;;

let test_extract_n () = 
  let l = [(3,4);(5,6);(7,8)] in
  let el1 = extract_n l 2 in
  assert (el1 = ([(3,4);(5,6)],[(5,6);(7,8)]));
  let el2 = extract_n [(3,4)] 0 in
  assert (el2 = ([],[(3,4)]));
  ()
;;
  
let test_dist_floats () =
  let v1 = (3, 2) in
  let v2 = (3.0, 5.0) in
  let v3 = (3.0, 2.0) in
  assert(dist_floats v1 v2 = 3.0);
  assert(dist_floats v1 v3 = 0.0);
  ()
;;    

let test_get_lines () =
  let indices = [0;1;4] in
  let old = [(1,1);(2,2);(3,3);(4,4);(1,1)] in
  let lines = get_lines indices old in
  assert (List.length lines = 2);
  ()
;;         

let test_gen_ak () =
    let original = (3,2) in
    let test2 = (3.25, 2.25) in
    assert(gen_ak original test2 = test2);
    ()

;;

let test_find_intersection () =
  let g1 = (5., 2.) in
  let m1 = (2.0, 5.0) in
  let g2 = (3., 4.) in
  let dne = (3.0, 0.0) in  
  assert(find_intersection (g1, m1) (g2, dne) = (3., 1.2));
  ()

;;

let test_shift_subpath () =
  let shifted = shift_subpath [(0,0);(0,1);(0,2);(0,3);(1,3);(1,4);(1,5);(0,5);(0,6);(1,6);(1,7);
                 (2,7);(2,6);(3,6);(3,7);(4,7);(4,6);(4,5);(4,4);(4,3);(4,2);(3,2);
                 (3,1);(3,0);(2,0);(2,1);(2,2);(1,2);(1,1);(1,0);(0,0)]  
                 [23;26;0;10;16;23] in
  assert (shifted = ([(3,0);(2,0);(2,1);(2,2);(1,2);(1,1);(1,0);(0,0);(0,0);(0,1);(0,2);(0,3);(1,3);(1,4);(1,5);(0,5);(0,6);(1,6);(1,7);
                 (2,7);(2,6);(3,6);(3,7);(4,7);(4,6);(4,5);(4,4);(4,3);(4,2);(3,2);(3,1);(3,0)],  [0;3;8;18;24;30])); 
  ()
;;  

let test_slope_tuple() = 
  let v1 = (3.0, 2.0) in
  let v2 = (2.0, 3.0) in
  let vdne = (3.0, 4.0) in
    let v0 = (2.0, 2.0) in
    assert(slope_tuple v1 v2 = (1., -1.));
    assert(slope_tuple v1 vdne = (2., 0.));
    assert(slope_tuple v1 v0 = (0., -1.));
    ()
;;

let test_dist_line_pt() =
    let test = (3.0, 4.0) in
    let a1 = (2.0, 3.0) in
    let a2 = (2.0, 5.0) in 
    assert(dist_line_pt test a1 a2 = 1.0);
    ()
;;
    
let test_dist_floatvertices() = 
    let v1 = (3.0, 2.0) in
    let v2 = (2.0, 3.0) in
    let vdne = (3.0, 4.0) in
    let v0 = (2.0, 2.0) in
    assert(dist_floatvertices v1 v2 = sqrt(2.0));
    assert(dist_floatvertices v1 vdne = 2.0);
    assert(dist_floatvertices v1 v0 = 1.);
    ()

;;
 
let rec print_list m = 
    match m with
    | [] -> ()
    | hd::tl -> let x = match hd with
            | (a, b)-> (string_of_float a) ^","^(string_of_float b) ^ "\n" in
        let _ = Printf.printf "%s" x in

    print_list tl 

;;

                   
let test_find_midpoints ()=
  let lst = [(1.0,2.0); (3.0,4.0); (5.0,6.0)] in
  let lst2 = [] in
  assert(find_midpoints lst = [(1.0,2.0); (2.0,3.0);(3.0,4.0); (4.0,5.);(5.0,6.0);(3.0, 4.0); (1.0,2.0)]);
  assert(find_midpoints lst2 = []);
  ()
;;

 
let test_find_a () = 
    let b1 = (3., 2.) in
    let b2 = (5., 4.) in
    let a1 = (4., 2.) in
    let c = (3.5, 2.5) in 
    let sol = 4. /. 3. *. (dist_floatvertices b1 c) /. (dist_floatvertices b1 a1) in
    let ans = find_a b1 a1 b2 in
    assert (ans=sol);
    ()

;;
    

let test_transform () = 
  let p1 = (1., 1.) in
  let p2 = (3., 1.) in
  let p3 = (2., 2.) in
  let constants = transform p1 p2 p3 in
  assert (constants = (-2., -1., 1., 0., 0., 1.));

  ()

;;


let run_tests () =
  test_arith_mean();
  test_split();
  test_find_constants ();
  test_find_eigenvalue ();
  test_get_eigenvector ();
  test_gravity_ctr ();
  test_extract_n (); 
  test_dist_floats ();
  test_get_lines ();
  test_gen_ak ();
  test_find_intersection ();
  test_shift_subpath ();
  test_slope_tuple ();
  test_dist_line_pt ();
  test_dist_floatvertices ();
  test_find_midpoints ();
  test_find_a ();
  test_transform ();
  ()

  
let _ = run_tests();;
