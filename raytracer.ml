(*open SceneLoader;;*)

let pi = 4. *. (atan 1.);;
  
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

type plan = {plan_origin:point;
            p_dir:vecteur};;

type triangle = {v0:point;
                 v1:point;
                 v2:point};;

  
let make_point x y z = {x=x;
                        y=y;
                        z=z};;

let make_vect x y z = make_point x y z;;

let make_color r g b = {red=r;
                        green=g;
                        blue=b};;

let make_materiau col ka kd ks beta = {mat_color=col;
                                       ka=ka;
                                       kd=kd;
                                       ks=ks;
                                       beta=beta};;
  
let make_distant_light light_dir light_color intensity = {light_dir=light_dir;
                                                          light_color=light_color;
                                                          intensity=intensity};;

let make_ray ray_origin ray_dir = {ray_origin=ray_origin;
                                   ray_dir=ray_dir};;
  
let make_camera cam_origin height width angle pvise = {cam_origin=cam_origin;
                                                       height=height;
                                                       width=width;
                                                       angle=angle;
                                                       pvise=pvise};;
  
let make_sphere x y z radius mat = {center=make_point x y z;
                                    radius=radius;
                                    mat=mat};;

let make_plan x y z p_dir = {plan_origin=make_point x y z;
                             p_dir=p_dir};;
  
let make_triangle x0 y0 z0 x1 y1 z1 x2 y2 z2 = {v0=make_point x0 y0 z0;
                                                v1=make_point x1 y1 z1;
                                                v2=make_point x2 y2 z2};;
  

  
let (+|) v1 v2 = {x=v1.x +. v2.x;
                  y=v1.y +. v2.y;
                  z=v1.z +. v2.z};;


let (-|) v1 v2 = {x=v1.x -. v2.x;
                  y=v1.y -. v2.y;
                  z=v1.z -. v2.z};;


let scalaire scal v1 = {x=v1.x *. scal;
                        y=v1.y *. scal;
                        z=v1.z *. scal};;

let prod_scal v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z;;  

let prod_vect v1 v2 ={x=(v1.y*.v2.z)-.(v1.z*.v2.y);
                      y=(v1.z*.v2.x)-.(v1.x*.v2.z);
                      z=(v1.x*.v2.y)*.(v1.y*.v2.x)};;

let norme v1 = sqrt (v1.x**2. +. v1.y**2. +. v1.z**2.);;

let normalisation v1 = let n = norme v1 in
                       {x=v1.x /. n;
                        y=v1.y /. n;
                        z=v1.z /. n};;


let vect_null = make_vect 0. 0. 0.;;
let ia = 0.2;;

let intersect ray sphere =
  let xd = ray.ray_dir.x and
      yd = ray.ray_dir.y and
      zd = ray.ray_dir.z and
      x0 = ray.ray_origin.x and
      y0 = ray.ray_origin.y and
      z0 = ray.ray_origin.z and
      xc = sphere.center.x and
      yc = sphere.center.y and
      zc = sphere.center.z in
  let a = xd**2. +. yd**2. +. zd**2. and
      b = 2. *. (xd *. (x0 -. xc) +. yd *. (y0 -. yc) +. zd *. (z0 -. zc)) and
      c = ((x0 -. xc)**2. +. (y0 -. yc)**2. +. (z0 -. zc)**2.) -. sphere.radius**2. in
  let delta = b**2. -. 4.*.a*.c in
  if delta < 0.
  then (infinity,vect_null,sphere)
  else
    if delta = 0.
    then let t = (-.b) /. (2.*.a) in
         let dist = make_vect (t*.(xd-.x0)) (t*.(yd-.y0)) (t*.(zd-.z0)) in
         (norme dist, (dist -| sphere.center),sphere)
    else
      let t1 = (-.b +. sqrt delta) /. (2.*.a) and
          t2 = (-.b -. sqrt delta) /. (2.*.a) in
      let dist1 = make_vect (t1*.(xd-.x0)) (t1*.(yd-.y0)) (t1*.(zd-.z0)) and
          dist2 = make_vect (t2*.(xd-.x0)) (t2*.(yd-.y0)) (t2*.(zd-.z0)) in
      let n1 = norme dist1 and
          n2 = norme dist2 in
      if t1 >= 0. && t2 < 0.
      then (n1, (dist1 -| sphere.center), sphere)
      else
	if t2 >= 0. && t1 < 0.
	then (n2, (dist2 -| sphere.center), sphere)
	else
	  if t1 >= 0. && t2 >= 0.
	  then
	    if n1 <= n2
	    then (n1, (dist1 -| sphere.center), sphere)
	    else (n2, (dist2 -| sphere.center), sphere)
	  else (infinity,vect_null,sphere);;

  
let calc_proche ray list_sphere =
  let min = ref (infinity,vect_null,list_sphere.(0)) in
  for i=0 to (Array.length list_sphere)-1 do
    let aux = intersect ray list_sphere.(i) in
    match aux with
      (dist,normale,sphere) -> match !min with (mdst,_,_) -> if dist < mdst then min:=aux;
  done;
  !min;;

let intersect_plan ray plan =
  let nominateur = prod_scal (plan.plan_origin -| ray.ray_origin) plan.p_dir and
      denominateur = prod_scal ray.ray_dir plan.p_dir in
  if denominateur = 0. || (nominateur /. denominateur) < 0.
  then
    (infinity,plan.p_dir,plan)
  else
    let t = nominateur /. denominateur in
    let d = make_vect (t*.(ray.ray_dir.x-.ray.ray_origin.x)) (t*.(ray.ray_dir.y-.ray.ray_origin.y)) (t*.(ray.ray_dir.z-.ray.ray_origin.z)) in
    ((norme d),plan.p_dir,plan);;
  

let calc_proche_plan ray list_plan =
  let min = ref (infinity,vect_null,list_plan.(0)) in
  for i=0 to (Array.length list_plan)-1 do
    let aux = intersect_plan ray list_plan.(i) in
    match aux with
      (dist,normale,plan) -> match !min with (mdst,_,_) -> if dist < mdst then min:=aux;
  done;
  !min;;

  
let intersect_triangle ray triangle =
  let v0v2 = (triangle.v2 -| triangle.v0) in
  let v0v1 = (triangle.v1 -| triangle.v0) in
  let m = prod_scal (prod_vect ray.ray_dir v0v2) v0v1 in
  let _ = Printf.printf "%f %f %f\n" v0v1.x v0v1.y v0v1.z in
  if m = 0.
  then
    let _ = Printf.printf "test2\n" in
    (infinity,vect_null,triangle)
  else
    let v0O = (ray.ray_origin -| triangle.v0) in
    let u = (prod_scal v0O (prod_vect ray.ray_dir v0v2)) /. m in
    let _ = Printf.printf "test1\n" in
    if u < 0. || u > 1.
    then
      (infinity,vect_null,triangle)
    else
      let v = (prod_scal ray.ray_dir (prod_vect v0O v0v1)) /. m in
      if v < 0. || (u+.v) > 1.
      then
        (infinity,vect_null,triangle)
      else
        let t = (prod_scal v0v2 (prod_vect v0O v0v1)) /. m in
        let d = make_vect (t*.(ray.ray_dir.x-.ray.ray_origin.x)) (t*.(ray.ray_dir.y-.ray.ray_origin.y)) (t*.(ray.ray_dir.z-.ray.ray_origin.z)) in
        ((norme d),vect_null,triangle);;
    


let calc_proche_triangle ray list_triangle =
  let min = ref (infinity,vect_null,list_triangle.(0)) in
  for i=0 to (Array.length list_triangle)-1 do
    let aux = intersect_triangle ray list_triangle.(i) in
    match aux with
      (dist,normale,plan) -> match !min with (mdst,_,_) -> if dist < mdst then min:=aux;
  done;
  !min;;

  
let top_color value = (max 0 (min 255 value));;

let illumination x y light sphere n oq shadow =
  let mat = sphere.mat and
      v = normalisation (vect_null -| oq) and
      llight = normalisation (scalaire (-.1.) light.light_dir) and
      normale = normalisation n in
  let h = normalisation (llight +| v) in
  
  let kared = mat.ka *. (mat.mat_color.red/.255.) and
      kagreen = mat.ka *. (mat.mat_color.green/.255.) and
      kablue = mat.ka *. (mat.mat_color.blue/.255.) and

      kdred = mat.kd *. (mat.mat_color.red/.255.) and
      kdgreen = mat.kd *. (mat.mat_color.green/.255.) and
      kdblue = mat.kd *. (mat.mat_color.blue/.255.) and

      ksred = mat.ks *. (mat.mat_color.red/.255.) and
      ksgreen = mat.ks *. (mat.mat_color.green/.255.) and
      ksblue = mat.ks *. (mat.mat_color.blue/.255.) in

  let iared = kared *. ia *. light.intensity and
      iagreen = kagreen *. ia *. light.intensity and
      iablue = kablue *. ia *. light.intensity in

  let idred = kdred *. (prod_scal llight normale) *. (light.intensity *. (light.light_color.red/.255.)) and
      idgreen = kdgreen *. (prod_scal llight normale) *. (light.intensity *. (light.light_color.green/.255.)) and
      idblue = kdblue *. (prod_scal llight normale) *. (light.intensity *. (light.light_color.blue/.255.)) in

  let isred = ksred *. ((prod_scal h normale)**mat.beta) *. (light.intensity *. (light.light_color.red/.255.)) and
      isgreen = ksgreen *. ((prod_scal h normale)**mat.beta) *. (light.intensity *. (light.light_color.green/.255.)) and
      isblue = ksblue *. ((prod_scal h normale)**mat.beta) *. (light.intensity *. (light.light_color.blue/.255.)) in

  let ired = int_of_float (iared +. idred +. isred) and
      igreen = int_of_float (iagreen +. idgreen +. isgreen) and
      iblue = int_of_float (iablue +. idblue +. isblue) in
  if shadow=true
  then
    (Graphics.set_color (Graphics.rgb (top_color (int_of_float iared)) (top_color (int_of_float iagreen)) (top_color (int_of_float iablue)));
     Graphics.plot x y)
  else
    (Graphics.set_color (Graphics.rgb (top_color ired) (top_color igreen) (top_color iblue));
     Graphics.plot x y);;


let remove_sphere_from_list list_sphere sphere =
  let libre = ref 0 in
  let array = Array.make ((Array.length list_sphere)-1) (make_sphere 0. 0. 0. 0. sphere.mat) in
  for i=0 to ((Array.length list_sphere)-1) do
    if list_sphere.(i) != sphere
    then
      (array.(!libre)<-list_sphere.(i);
       libre:=!libre+1;)
  done;
  array;;
      
let primary_ray cam list_sphere light =
  let image_ratio = cam.height /. cam.width and
      half_width = tan (cam.angle /. 2.) in
  let half_height = image_ratio *. half_width in
  let up = make_vect 0. 1. 0. in
  let op = normalisation (cam.pvise -| cam.cam_origin) in
  let u = normalisation (prod_vect up op) in
  let v = normalisation (prod_vect op u) in
  let c = (cam.pvise -| (scalaire half_width u)) -| (scalaire half_height v) in
  let x = (scalaire ((2.*.half_width)/.cam.width) u) in
  let y = (scalaire ((2.*.half_height)/.cam.height) v) in
  for i=0 to (int_of_float cam.height)-1 do
    for j=0 to (int_of_float cam.width)-1 do
      let q = c +| (scalaire (float_of_int j) x) +| (scalaire (float_of_int i) y) in
      let dir = normalisation (q -| cam.cam_origin) in
      let ray = make_ray cam.cam_origin dir in
      let dist = calc_proche ray list_sphere in
      match dist with (norme,normale,sphere) ->
	if norme != infinity
	then
	  let intersection = normale +| sphere.center in
	  let invdir = (scalaire (-.1.) light.light_dir) in
	  let invray = make_ray intersection invdir in
          let distinter =
            if (Array.length list_sphere) = 1
            then
              (infinity,vect_null,list_sphere.(0))
            else
              calc_proche invray (remove_sphere_from_list list_sphere sphere) in
	  match distinter with (normeinv,_,_) ->
	    if normeinv != infinity
	    then
              (Graphics.set_color Graphics.black;
               Graphics.plot j i)
              (*illumination j i light sphere normale q true*)
	    else
	      illumination j i light sphere normale q false;
    done;
  done;;


let primary_ray_plan cam list_plan light =
  let image_ratio = cam.height /. cam.width and
      half_width = tan (cam.angle /. 2.) in
  let half_height = image_ratio *. half_width in
  let up = make_vect 0. 1. 0. in
  let op = normalisation (cam.pvise -| cam.cam_origin) in
  let u = normalisation (prod_vect up op) in
  let v = normalisation (prod_vect op u) in
  let c = (cam.pvise -| (scalaire half_width u)) -| (scalaire half_height v) in
  let x = (scalaire ((2.*.half_width)/.cam.width) u) in
  let y = (scalaire ((2.*.half_height)/.cam.height) v) in
  for i=0 to (int_of_float cam.height)-1 do
    for j=0 to (int_of_float cam.width)-1 do
      let q = c +| (scalaire (float_of_int j) x) +| (scalaire (float_of_int i) y) in
      let dir = normalisation (q -| cam.cam_origin) in
      let ray = make_ray cam.cam_origin dir in
      let dist = calc_proche_plan ray list_plan in
      match dist with (norme,normale,sphere) ->
	if norme != infinity
	then
          (Graphics.set_color Graphics.black;
           Graphics.plot j i)
    done;
  done;;


let primary_ray_triangle cam list_triangle light =
  let image_ratio = cam.height /. cam.width and
      half_width = tan (cam.angle /. 2.) in
  let half_height = image_ratio *. half_width in
  let up = make_vect 0. 1. 0. in
  let op = normalisation (cam.pvise -| cam.cam_origin) in
  let u = normalisation (prod_vect up op) in
  let v = normalisation (prod_vect op u) in
  let c = (cam.pvise -| (scalaire half_width u)) -| (scalaire half_height v) in
  let x = (scalaire ((2.*.half_width)/.cam.width) u) in
  let y = (scalaire ((2.*.half_height)/.cam.height) v) in
  for i=0 to (int_of_float cam.height)-1 do
    for j=0 to (int_of_float cam.width)-1 do
      let q = c +| (scalaire (float_of_int j) x) +| (scalaire (float_of_int i) y) in
      let dir = normalisation (q -| cam.cam_origin) in
      let ray = make_ray cam.cam_origin dir in
      let dist = calc_proche_triangle ray list_triangle in
      match dist with (norme,normale,sphere) ->
	if norme != infinity
	then
          (Graphics.set_color Graphics.black;
           Graphics.plot j i)
    done;
  done;;



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
                                


let init_graph width height =
  Graphics.open_graph "";
  Graphics.set_window_title "RayTracer Demo";
  Graphics.resize_window width height;
  Graphics.set_color (Graphics.rgb 30 144 255);
  Graphics.fill_rect 0 0 width height;;


let wait() =
  let _ = Printf.printf "Appuyez sur Entrer pour quitter\n" in
  let c = read_line () in
  if((String.compare c "q")=0)
  then false
  else true;;


let generate_sphere_floor x y z n radius mat =
  let array = Array.make (n*n) (make_sphere 0. 0. 0. 0. mat) in
  let half_radius = radius /. 2. in
  for i=0 to (n*n)-1 do
    let k = float_of_int (i/n) and
        l = float_of_int (i mod n) in
    array.(i)<-(make_sphere (x+.k*.radius) (x+.l*.half_radius) (z+.(l*.half_radius)) radius mat);
  done;
  array;;
  



 

let save_file (cam,objects,lights) =
  let _ = Printf.printf "Voulez-vous sauvegarder la scene ?\n0.Oui\n1.Non\n" in
  let choix = read_line () in
  if((String.compare choix "0")=0)
  then
    let _ = Printf.printf "Nom du fichier a sauvegarder:\n" in
    let file = read_line () in
    save_scene (cam,objects,lights) file;;

let load_file () =
  let _ = Printf.printf "Nom du fichier a charger:\n" in
  let file = read_line () in
  match load_scene file with
  |(Some(cam),objects,lights) -> (init_graph 700 500;
				  primary_ray cam (Array.of_list objects) (Array.of_list lights).(0);
				  cam,objects,lights)
  |_->(make_camera vect_null 0. 0. 0. vect_null,[],[]);;


let main_file () =
  match load_file () with
  |(cam,objects,lights) -> save_file (cam,objects,lights);;



let main_written () =
  init_graph 700 500;
  let mat = make_materiau (make_color 255. 105. 180.) 0.4 0.1 0.4 10. in
  let mat2 =  make_materiau (make_color 255. 0. 0.) 0.4 0.1 0.4 10. in
  let l = make_distant_light (make_vect (+.1.) (-.1.) (+.0.)) (make_color 255. 255. 255.) 1500. in
  let sphere1 = make_sphere (-.0.1) 0.11 0.9 0.05 mat in
  let sphere2 = make_sphere 0. 0. 10. 1. mat in
  let list_sphere = [|sphere1;sphere2|] in
  let floor = generate_sphere_floor (-.1.85) 0. 10. 30 0.3 mat2 in
  let sphereset = Array.append list_sphere floor in
  let cam = make_camera (make_point 0. 0. 0.) 500. 700. ((8.*.pi)/.3.) (make_vect 0. 0. 5.) in
  (primary_ray cam sphereset l;
   save_file (cam,(Array.to_list sphereset),l::[]));;


let choose_main () =
  let _ = Printf.printf "0.Lancer le main du raytracer\n1.Charger une scene a partir d'un fichier\n" in
  let c = read_line () in
  if((String.compare c "0")=0)
  then main_written ()
  else main_file ();;


let main_plan () =
  init_graph 700 500;
  let l = make_distant_light (make_vect (+.1.) (-.1.) (+.0.)) (make_color 255. 255. 255.) 1500. in
  let plan1 = make_plan 0.5 0.5 0.5 (make_vect 1. 1. 0.) in
  let list_plan = [|plan1|] in
  let cam = make_camera (make_point 0. 0. 0.) 500. 700. ((8.*.pi)/.3.) (make_vect 0. 0. 5.) in
  primary_ray_plan cam list_plan l;;


let main_triangle () =
  init_graph 700 500;
  let l = make_distant_light (make_vect (+.1.) (-.1.) (+.0.)) (make_color 255. 255. 255.) 1500. in
  let triangle1 = make_triangle (-.2.) 3. 7. 2. 1. 6. 3. 0. 8. in
  let list_triangle = [|triangle1|] in
  let cam = make_camera (make_point 0. 0. 0.) 500. 700. ((8.*.pi)/.3.) (make_vect 0. 0. 5.) in
  primary_ray_triangle cam list_triangle l;;


  (*main_triangle ();;*)
  choose_main ();;
  wait ();;
      
  
