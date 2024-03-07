open Raylib
open Maths

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

let sinus t cubes =
  let sinus cube =
    let x,y,_ = cube in
    (x, y,  1. +. (sin (t +. x +. y)))
  in
  List.map sinus cubes

let draw s = 
     let dir = (
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
     Camera3D.set_target s.camera (r3_to_vec3 (s.pos +$ (dir *$ 5.))); 
     end_mode_3d ();
     draw_text "Space S X arrows" 10 10 20 Color.white;
     begin_drawing ();
     end_drawing ()

let rec loop s =
     if Raylib.window_should_close () then Raylib.close_window () else 
         draw s;
         let pos' = s.pos 
          +$ ((if is_key_down Key.W then (1.0,0.,0.) else (0.,0.,0.) +$ if is_key_down Key.S then (-1.0,0.,0.) else (0.,0.,0.)) *$ sin s.target.horizontal)
          +$ ((if is_key_down Key.A then (0.,1.0,0.) else (0.,0.,0.) +$ if is_key_down Key.D then (0.,-1.0,0.) else (0.,0.,0.)) *$ cos s.target.horizontal)
          +$ if is_key_down Key.Space then (0.,0.,1.0) else (0.,0.,0.) +$ if is_key_down Key.Left_shift then (0.,0.,-1.0) else (0.,0.,0.) 
          in
         let target' = {
          horizontal = s.target.horizontal +. 0.02*.(if is_key_down Key.Up then 1.0 else 0.0 -. if is_key_down Key.Down then 1.0 else 0.0);
          vertical = s.target.vertical +. 0.02*.(if is_key_down Key.Right then 1.0 else 0.0 -. if is_key_down Key.Left then 1.0 else 0.0);
         } in

         loop {
             camera = s.camera;
             cubes = sinus s.t s.cubes;
             pos = pos';
             t = s.t +. 0.1;
             target = target';
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

let () = () |> setup |> loop; 
