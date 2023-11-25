open Raylib
open Maths

let w = 1000
let h = 1000
let zero = Vector3.create 0. 0. 0.

type r3 = float*float*float

type status = {
    camera : Camera3D.t;
    pos : r3;
    cubes : Vector3.t list;
}

let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  {
      camera = Camera3D.create zero zero (Vector3.create 0. 1. 0.) 45. CameraProjection.Perspective;
      pos = (0.,0.,10.);
      cubes = List.init 5 (fun i -> Vector3.create 0. 0. (float_of_int i)) 
   }

let r3_to_vec3 pos =
    let x,y,z = pos in
    Vector3.create x y z

let vec3_to_r3 v = 
    let open Vector3 in 
    let a,b,c = x v, y v, z v in (a,b,c)

let ceil_vec3 v = let open Vector3 in create (ceil (x v)) (ceil (y v)) (ceil (z v))

let make_them_fall cubes = 
    let is_at x' y' z' cube =
        let open Vector3 in 
        x cube = x' && y cube = y' && z cube = z' 
    in
    let move = Fun.id in
    let rec aux cs acc =
        match cs with
        [] -> acc
        | t::q -> let tx,ty,tz = vec3_to_r3 t in 
          aux q ((move t)::acc)
    in aux cubes [] |> List.rev
 
let rec loop s =
     if Raylib.window_should_close ()  then Raylib.close_window () else 
         begin_drawing ();
         clear_background Color.black;
         begin_mode_3d s.camera;
         List.iteri (fun i pos -> draw_cube pos 1.0 1.0 1.0 (color_from_hsv (float_of_int (i*360/(List.length s.cubes))) 1. 1.)) s.cubes;
         end_mode_3d ();
         draw_text "Space S X arrows" 10 10 20 Color.white;
         end_drawing ();
         Camera3D.set_position s.camera (r3_to_vec3 s.pos);
         Camera3D.set_target s.camera (r3_to_vec3 (s.pos $+ (0.0,0.0,-1.0))); 
         loop {
             camera = s.camera;
             cubes = if is_key_pressed Key.Space then (ceil_vec3 (r3_to_vec3 s.pos))::s.cubes else make_them_fall s.cubes;
             pos = (let x,y,z = s.pos in let d = 0.5 in
                  x +. (if is_key_down Key.Right then d else if is_key_down Key.Left then -.d else 0.), 
                  y +. (if is_key_down Key.Up then d else if is_key_down Key.Down then -.d else 0.), 
                  z +. (if is_key_down Key.S then d else if is_key_down Key.X then -.d else 0.) )
          }

let () =  setup () |> loop; 
