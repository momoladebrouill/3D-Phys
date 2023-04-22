open Raylib

let w = 1000
let h = 1000
let zero = Vector3.create 0. 0. 0.

type r3 = {
    d : float;
    theta :float;
    phi : float;
}

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
      pos = {
          theta = 0.;
          phi = 0.;
          d = 10.
      };
      cubes = List.init 5 (fun i -> Vector3.create (float_of_int (i/5)) (float_of_int (i mod 5)) 0.)
   }

let r3_to_vec3 pos =
    Vector3.create (cos pos.theta *. pos.d) ((sin pos.theta +. cos pos.phi) *. pos.d) (sin pos.phi *. pos.d)

let ceil_vec3 v = let open Vector3 in create (ceil (x v)) (ceil (y v)) (ceil (z v))

let vec3_to_tuple v = let open Vector3 in x v, y v, z v

let make_them_fall cubes = 
    let is_at x' y' z' cube =
        let open Vector3 in 
        x cube = x' && y cube = y' && z cube = z' 
    in
    let rec aux cs acc =
        match cs with
        [] -> acc
        | t::q -> let tx,ty,tz = vec3_to_tuple t in 
          aux q ((if (List.exists (is_at (tx -. 1.0) ty tz) cubes || tx < 1.) then t else Vector3.create (tx -. 1.) ty tz )::acc)
    in aux cubes []
 
let rec loop s =
     if Raylib.window_should_close () then Raylib.close_window () else 
         begin_drawing ();
         clear_background Color.black;
         begin_mode_3d s.camera;
         List.iteri (fun i pos -> draw_cube pos 1.0 1.0 1.0 (color_from_hsv (float_of_int (i*360/(List.length s.cubes))) 1. 1.)) s.cubes;
         end_mode_3d ();
         end_drawing ();
         Camera3D.set_position s.camera (r3_to_vec3 s.pos);
         loop {
             camera = s.camera;
             cubes = if is_key_pressed Key.Space then (ceil_vec3 (r3_to_vec3 s.pos))::s.cubes else make_them_fall s.cubes |> List.rev;
             pos = {
                 phi = s.pos.phi +. (if is_key_down Key.Up then 0.01 else if is_key_down Key.Down then -0.01 else 0.);
                 theta = s.pos.theta +. (if is_key_down Key.Left then 0.01 else if is_key_down Key.Right then -0.01 else 0.);
                 d = s.pos.d +. (if is_key_down Key.A then 1. else if is_key_down Key.Q then -1. else 0.);
             }
          }

let () =  
    setup () |> loop; 
