open Types;;
open Print;;
open Graphmatrix;;
open Step1;;
open Step2;;
open Step3;;

(******************************************************************
   INSTRUCTION FOR ARGUMENTS:
   ./output INPUT_FILE OUTPUT_FILE SCALE 
   INPUT_FILE: direction to input file, from the current folder
   OUTPUT_FILE: name of output file, without extension. It will
   be saved in testsoutputs/OUTPUT_FILE.svg
   SCALE: optional argument to choose the scale to see the image.
   Default is 50 (50 times bigger than original bitmap)
*******************************************************************)

(* FUNCTIONS FOR OUTPUTS *)

(* converts a matrix to an svg string to be displayed, shifted
   to start in (posx,posy) with each pixel displayed with size scale *)
let svg_matrix (mat: BiColorMatrix.matrix) ((posx,posy): (int*int)) (scale: int) : string =

  (* template of svg rectangle *) 
  let svg_template (x: int) (y: int) (fill: string) (scale: int) : string =
    let str = Printf.sprintf "<rect x=\"%dpx\" y=\"%dpx\" width=\"%dpx\"
       height=\"%dpx\" stroke=\"blue\" fill=\"%s\"/>\n" 
      (posx + (x * scale)) (posy + (y * scale)) scale scale fill in
    str
  in

  (* converts biColor to string *)
  let bicolor_to_fill (c: biColor) : string =
    match c with
      | Black -> "black"
      | White -> "white"
  in

  (* gets dimensions of the matrix *)
  let (rows,cols) = BiColorMatrix.dimensions mat in

  (* initializes list of coordinates of the pixels of the matrix *)
  let pixels = init_list (cols-1) (rows-1) in

  (* concatenates the svg templates for each pair (x,y) in a given list *)
  let rec get_svg (m: BiColorMatrix.matrix) (lst: (int*int) list) : string =
    match lst with
      | [] -> ""
      | hd :: tl -> 
	let (x,y) = hd in
	let fill = bicolor_to_fill (BiColorMatrix.get m y x) in
	(svg_template x y fill scale) ^ (get_svg m tl)
  in

  get_svg mat pixels
;;

(* generates an arrow to display the graph - for testing 
   the arrow is not actually an arrow, it is a segment with 
   a circle in the edge *)

let generate_arrow ((x1,y1) : (int*int)) ((x2,y2) : (int*int)) 
    ((posx,posy) : (int*int)) (scale: int) : string = 
  let triangle_pts ((x1,y1) : (int*int)) ((x2,y2) : (int*int)) 
      ((posx,posy) : (int*int)) (scale: int) : string =
    let tri_const = scale/10 in
    match ((x1-x2),(y1-y2)) with
      | (0,1) -> Printf.sprintf "%d,%d %d,%d" 
	(posx+x2*scale-tri_const) (posy+y2*scale+2*tri_const)
	(posx+x2*scale+tri_const) (posy+y2*scale+2*tri_const)
      | (0,-1) -> Printf.sprintf "%d,%d %d,%d" 
	(posx+x2*scale-tri_const) (posy+y2*scale-2*tri_const)
	(posx+x2*scale+tri_const) (posy+y2*scale-2*tri_const)
      | (1,0) -> Printf.sprintf "%d,%d %d,%d" 
	(posx+x2*scale+2*tri_const) (posy+y2*scale+tri_const)
	(posx+x2*scale+2*tri_const) (posy+y2*scale-tri_const)
      | (-1,0) -> Printf.sprintf "%d,%d %d,%d"
	(posx+x2*scale-2*tri_const) (posy+y2*scale+tri_const)
	(posx+x2*scale-2*tri_const) (posy+y2*scale-tri_const) 
      | _ -> raise WrongEdge
  in
  Printf.sprintf "<polyline stroke=\"red\" points=\"%d,%d %d,%d\"/>
    <polyline stroke=\"red\" points=\"%d,%d %s %d,%d\"/>"
    (posx+x1*scale) (posy+y1*scale) (posx+x2*scale) (posy+y2*scale)
    (posx+x2*scale) (posy+y2*scale) (triangle_pts (x1,y1) (x2,y2) (posx,posy) scale) 
    (posx+x2*scale) (posy+y2*scale)

;;


let svg_graph (g: IntVertexGraph.graph) (pos : (int*int)) (scale: int) : string =
  let vertices = IntVertexGraph.vertices g in
  let connect_outnb (gr: IntVertexGraph.graph) (n: IntVertexGraph.node) =
    List.fold_right (fun nb str -> str ^ (generate_arrow n nb pos scale)) 
      (IntVertexGraph.outneighbors gr n) ""
  in
  List.fold_right (fun nd str -> str ^ (connect_outnb g nd)) vertices ""
;;



(* CONVERTS INT PATH TO SVG *)
let svg_single_path ((lst, color) : int path) ((posx, posy) : (int*int)) (scale: int) : string =
  let rec list_to_points (l: int vertex list) ((px,py) : (int*int)) : string =
    match l with
      | [] -> ""
      | (x0,y0) :: tl -> (Printf.sprintf "%d,%d " (x0*scale+px) (y0*scale+py))^(list_to_points tl (px,py))
  in
  let fill =
    match color with
      | Black -> "black"
      | White -> "white"
  in 
  "<polyline stroke=\"red\" fill=\""^fill^ "\" points=\""
  ^(list_to_points lst (posx, posy))^"\" />\n" 
;;

let svg_paths (lst : (int path) list) ((posx,posy) : (int*int)) (scale: int) : string =
  let rec rec_paths (l : int path list) : string = 
    match l with
      | [] -> ""
      | hd :: tl ->
	(svg_single_path hd (posx,posy) scale) ^ (rec_paths tl)
  in
  rec_paths (List.rev lst)
;;


(* CONVERTS VECTOR TO SVG *)
let svg_single_vector ((vec, color) : (vector*biColor)) ((posx, posy) : (int*int)) (scale: int) : string =

  let (firstpt, lst) = vec in
  let xf,yf = firstpt in

  let b = Printf.sprintf "M%f,%f " 
     (xf*.(float_of_int scale)+.(float_of_int posx))
    (yf*.(float_of_int scale)+.(float_of_int posy))
  in

  let rec list_to_points (l: (curve*point) list) ((px,py) : (int*int)) : string =
    match l with
      | [] -> ""
      | (cur,pt) :: tl -> 
	let x0,y0 = pt in
	(match cur with
	  | Line -> 
	    (Printf.sprintf "L%d,%d " 
	       (int_of_float ((x0*.(float_of_int scale)+.(float_of_int px))+.0.5))
	       (int_of_float ((y0*.(float_of_int scale)+.(float_of_int py))+.0.5)))
	    ^(list_to_points tl (px,py))
	  | Bezier (ctr1,ctr2) -> 
	    let ((x1,y1),(x2,y2)) = (ctr1,ctr2) in
	    (Printf.sprintf "C%d,%d %d,%d %d,%d"
	       (int_of_float ((x1*.(float_of_int scale)+.(float_of_int px)+.0.5)))
	       (int_of_float ((y1*.(float_of_int scale)+.(float_of_int py)+.0.5)))
	       (int_of_float ((x2*.(float_of_int scale)+.(float_of_int px)+.0.5)))
	       (int_of_float ((y2*.(float_of_int scale)+.(float_of_int py)+.0.5)))
	       (int_of_float ((x0*.(float_of_int scale)+.(float_of_int px)+.0.5)))
	       (int_of_float ((y0*.(float_of_int scale)+.(float_of_int py)+.0.5))))
	    ^(list_to_points tl (px,py)))
  in
  
  let fill =
    match color with
      | Black -> "black"
      | White -> "white"
  in 
  
  "<path stroke=\"red\" fill=\""^fill^ "\" d=\""
  ^b^(list_to_points lst (posx, posy))^"\" />\n" 
;;

let svg_vectors (lst : (vector*biColor) list) ((posx,posy) : (int*int)) (scale: int) : string =
  let rec rec_paths (l : (vector*biColor) list) : string = 
    match l with
      | [] -> ""
      | hd :: tl ->
	(svg_single_vector hd (posx,posy) scale) ^ (rec_paths tl)
  in
  rec_paths (List.rev lst)
;;
 


let save file string =
  let channel = open_out file in
  output_string channel string;
  close_out channel
;;


let svg_header h w = Printf.sprintf "<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" 
  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg x=\"10px\" y=\"10px\" height=\"%dpx\" width=\"%dpx\" version=\"1.1\"
     xmlns=\"http://www.w3.org/2000/svg\">\n" h w ;;
let svg_footer = "</svg>";;




let optimize (p: int path) =
  let (lst, color) = p in
  (optimal_polygon lst, color)
;;

let rec get_vectors (lst: int path list) : (vector*biColor) list = 
  match lst with
    | [] -> []
    | hd :: tl ->
      let (p,color) = hd in
      let subpath = optimal_subpath_from_path p in
      let sides = List.length subpath in
      if (sides <= 3) then ((*Printf.printf "\nLESS THAN 2: "; print_list print_int subpath;*) get_vectors tl) 
      else ((*Printf.printf "\nMORE THAN 2: "; print_list print_int subpath;*)
	    (polygon_to_vector hd subpath) :: (get_vectors tl))
;;

  

let generate_output (input: string) (output: string) (scale: int) : unit =
  let matrix = bitmap_to_matrix input in
  let graph = matrix_to_graph matrix in
  let pathlist = extract_paths graph in
  let optpathlist = List.map optimize pathlist in
  let vectors = get_vectors pathlist in
  let (input_height, input_width) = BiColorMatrix.dimensions matrix in
  let svg_output = (svg_header (2*(input_height+1)*scale) (3*(input_width+1)*scale)) ^ (svg_matrix matrix (0,0) scale) ^ (svg_graph graph (0,scale + input_height*scale) scale) ^ (svg_paths pathlist (scale+scale*input_width,0) scale) ^ (svg_paths optpathlist (scale+scale*input_width,scale + input_height*scale) scale) ^ (svg_vectors vectors (2*(scale+scale*input_width),0) scale) ^ svg_footer
  in
  save ("../report/testoutputs/"^output^".svg") svg_output
;;  


let input = ref "" ;;
let output = ref "" ;;
let scale = ref 50 ;;

let check_args () : unit =
  let argc = Array.length Sys.argv in
  if ((argc <= 2) || (argc >= 5)) then (
    Printf.printf "Right input: ./output INPUT_FILE OUTPUT_FILE (SCALE)";
    exit 0)
  else
    input := Array.get Sys.argv 1;
    output := Array.get Sys.argv 2;
    if (argc = 3) then ()
    else scale := int_of_string (Array.get Sys.argv 3);
    generate_output (!input) (!output) (!scale) 
;;

let _ = check_args () ;;



