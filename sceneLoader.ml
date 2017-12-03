(** In charge of loading and parsing a file describing the scene.
    We decided against using ocamllex/ocamlyacc or dedicated lexers/parsers
    to keep it simple and understandable for students,
    who have not necessarily had any course about parsing and compilation.
    We also only use the standard library (though using Batteries would have been nice).

    To compile, you have to link with the Str module.
    See https://caml.inria.fr/pub/docs/manual-ocaml/libstr.html
*)

(* Vector but also camera, light, objects. Replace by you own modules *)
(*open Vector*)

type point = {x:float;
              y:float;
              z:float};;

type vecteur = point;;

type color = {red:float;
              green:float;
              blue:float};;

type materiau = {mat_color:color;
                 ka:float;
                 kd:float;
                 ks:float;
                 beta:float};;

type distant_light = {light_dir:vecteur;
                      light_color:color;
                      intensity:float};;

type ray = {ray_origin:point;
            ray_dir:vecteur};;

  
type camera = {cam_origin:point;
               height:float;
               width:float;
               angle:float;
               pvise:vecteur};;

type sphere = {center:point;
               radius:float;
               mat:materiau};;

let norme v1 = sqrt (v1.x**2. +. v1.y**2. +. v1.z**2.);;
  
let normalisation v1 = let n = norme v1 in
                       {x=v1.x /. n;
                        y=v1.y /. n;
                        z=v1.z /. n};;
  
  
(* Text and line of error *)
exception SyntaxError of string * int

let fs = float_of_string
let is = int_of_string

(*
When we parse several tokens at the same time, we give to the parsing functions the number
of the token before the ones we want to parse.
*)

(* To parse a point just after the tag of the line starting after token i*)
let parse_point i tokens =
  {x = fs tokens.(i + 1);
   y = fs tokens.(i + 2);
   z = fs tokens.(i + 3)
  }

let parse_colour i tokens =
  { red = float_of_int (is tokens.(i + 1));
    green = float_of_int (is tokens.(i + 2));
    blue = float_of_int (is tokens.(i + 3))
  }

let parse_camera materials (camera, objects, lights) tokens  =
  let cam_origin = parse_point 0 tokens in
  let pvise = parse_point 3 tokens in
  let angle = fs tokens.(7) in
  let width = float_of_int (is tokens.(8)) in
  let height = float_of_int (is tokens.(9)) in
  (Some {cam_origin ; pvise; angle; width ; height} , objects, lights)

let parse_light materials (camera, objects, lights) tokens =
  let light_dir = normalisation (parse_point 0 tokens) in
  let light_color = parse_colour 3 tokens in
  let intensity = fs tokens.(7) in
  (camera, objects, {light_dir; light_color; intensity}::lights)

let parse_sphere materials (camera, objects, lights) tokens =
  (* Center *)
  let center = parse_point 0 tokens in
  let radius = fs tokens.(4) in
  let mat = Hashtbl.find materials tokens.(5) in
  (camera, {center ; radius; mat}::objects, lights)

let parse_material materials scene tokens =
  let name = tokens.(1) in
  let mat_color  = parse_colour 1 tokens in
  let k = parse_point 4 tokens in
  let beta = fs tokens.(8) in
  Hashtbl.add materials name {mat_color ; ka = k.x ; kd = k.y ; ks = k.z ; beta};
  scene

(** Load the scene, as a triplet (camera option, object list, light list) *)
let load_scene fileName =
  let file = open_in fileName in
  let blank_splitter = Str.regexp "[ \t]+" in
  let numLine = ref 0 in
  let rec parse_line materials scene =
    incr numLine;
    try
      let line = input_line file in
      let tokens = Array.of_list (Str.split blank_splitter line) in
      let new_scene  = (match tokens.(0) with
       | "camera" -> parse_camera
       | "light"  -> parse_light
       | "sphere" -> parse_sphere
       | "material" -> parse_material
       | _ as t -> raise (SyntaxError("Unrecognized object: " ^ t, !numLine)))
          materials scene  tokens
      in
      parse_line materials new_scene
    with  End_of_file -> scene
  in
  try
    parse_line (Hashtbl.create 10) (None, [], [])
  with
  | Invalid_argument _ -> raise (SyntaxError( "Not enough arguments", !numLine))

(** Save the scene, from a triplet (camera option, object list, light list) *)
let save_camera cam file =
  Printf.fprintf file "camera %f %f %f %f %f %f %f %d %d\n" cam.cam_origin.x cam.cam_origin.y cam.cam_origin.z cam.pvise.x cam.pvise.y cam.pvise.z cam.angle (int_of_float cam.width) (int_of_float cam.height);;

let save_material mat cpt file =
  Printf.fprintf file "material mat%d %d %d %d %f %f %f %f\n" cpt (int_of_float mat.mat_color.red) (int_of_float mat.mat_color.green) (int_of_float mat.mat_color.blue) mat.ka mat.kd mat.ks mat.beta;;
  
let save_light light_list file =
  let rec aux list =
    match list with
    |[]->()
    |hd::tl->(Printf.fprintf file "light %f %f %f %d %d %d %f\n" hd.light_dir.x hd.light_dir.y hd.light_dir.z (int_of_float hd.light_color.red) (int_of_float hd.light_color.green) (int_of_float hd.light_color.blue) hd.intensity;aux tl) in
  aux light_list;;


let save_sphere sphere_list file =
  let rec aux cpt list =
    match list with
    |[]->()
    |hd::tl->(save_material hd.mat cpt file;
              Printf.fprintf file "sphere %f %f %f %f mat%d\n" hd.center.x hd.center.y hd.center.z hd.radius cpt;(aux (cpt+1) tl)) in
  aux 0 sphere_list;;
  

let save_scene (camera,objects,lights) fileName =
  let file = open_out fileName in
  let _ = save_camera camera file in
  let _ = save_sphere objects file in
  let _ = save_light lights file in
  close_out file;;
                                
