open Raylib
open Maths

let w = 1000
let h = 1000
let zero = Vector3.create 0. 0. 0. 

type direction = {
  vertical : float;
  horizontal : float;
}

type status = {
    camera : Camera3D.t;
    pos : r3;
    target : direction;
    cubes : r3 list;
    t : float;
}

let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  if is_window_ready () then
  {
      camera = Camera3D.create zero zero (Vector3.create 0. 1. 0.) 45. CameraProjection.Perspective;
      pos = (0.0,0.0,10.0);
      target = {
        vertical = 0.0;
        horizontal = 0.0;
      };
      t = 0.0;
      cubes = List.init 100 (fun i -> (float_of_int (i mod 10), float_of_int (i/10), 0.0); )
   }
   else raise (Invalid_argument "window not ready")

let r3_to_vec3 pos =
    let x,y,z = pos in
    Vector3.create x z y

let fst (a, b, c) = a
let snd (a, b, c) = b
let trd (a, b, c) = c
let round_r3 (a, b, c) = (float_of_int (int_of_float a), float_of_int (int_of_float b), float_of_int (int_of_float c))
let make_them_fall cubes = 
    let move pos = 
      let next_pos = (fst pos, snd pos,(trd pos) -. 1.0) in
      if (trd pos) < 0.0 || List.exists ((=) next_pos) cubes then pos else next_pos

    in
    List.map move cubes

let sinus t cubes =
  let sinus cube =
    let x,y,_ = cube in
    (x, y,  1. +. (sin (t +. x +. y)))
  in
  List.map sinus cubes
let rec loop s =
     if Raylib.window_should_close () then Raylib.close_window () else 
         let dir =
          let open Maths in
          (
          cos s.target.horizontal *. cos s.target.vertical,
          cos s.target.horizontal *. sin s.target.vertical,
          sin s.target.horizontal
          ) in
         clear_background Color.black;
         begin_mode_3d s.camera;
         draw_cube zero 1.0 1.0 1.0 Color.raywhite; 
         draw_cube zero 10.0 1.0 10.0 Color.raywhite; 
         draw_grid 100 10.0;
         let l = List.length s.cubes in
         List.iteri (fun i pos -> 
          draw_cube (r3_to_vec3 pos) 1.0 1.0 1.0 (color_from_hsv (foi (i*360/l)) 1. 1.);
          ()
          ) s.cubes;

         Camera3D.set_position s.camera (r3_to_vec3 s.pos);
         Camera3D.set_target s.camera (r3_to_vec3 (s.pos $+ (dir $* 5.))); 
         end_mode_3d ();
         draw_text ("Space S X arrows"^(string_of_float s.target.horizontal)) 10 10 20 Color.white;
         begin_drawing ();
         end_drawing ();
         let pos' = s.pos 
          $+ ((if is_key_down Key.W then (1.0,0.,0.) else (0.,0.,0.) $+ if is_key_down Key.S then (-1.0,0.,0.) else (0.,0.,0.)) $* sin s.target.horizontal)
          $+ ((if is_key_down Key.A then (0.,1.0,0.) else (0.,0.,0.) $+ if is_key_down Key.D then (0.,-1.0,0.) else (0.,0.,0.)) $* cos s.target.horizontal)
          $+ if is_key_down Key.Space then (0.,0.,1.0) else (0.,0.,0.) $+ if is_key_down Key.Left_shift then (0.,0.,-1.0) else (0.,0.,0.) 
          in
         let target' = {
          horizontal = s.target.horizontal +. 0.02*.(if is_key_down Key.Up then 1.0 else 0.0 -. if is_key_down Key.Down then 1.0 else 0.0);
          vertical = s.target.vertical +. 0.02*.(if is_key_down Key.Right then 1.0 else 0.0 -. if is_key_down Key.Left then 1.0 else 0.0);
         } in

         loop {
             camera = s.camera;
             cubes = if is_key_pressed Key.U then (round_r3 s.pos)::s.cubes 
              else sinus s.t s.cubes;
             pos = pos';
             t = s.t +. 0.1;
             target = target';
          }

let () = () |> setup |> loop; 
