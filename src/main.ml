open Raylib
open Force
open Maths  
open Constantes
open Graph

type direction = {
  vertical : float;
  horizontal : float;
}

type camera_status = {
    camera : Camera3D.t;
    position : vec;
    target : direction;
}

type status = {
    t : int; (*temps*)
    l : point array; (*le tableau des points*)
    z : float; (*zoom*) 
    shift : vec; (*shift de la cam*)
    k_ressort : float; (*la constante de ressort*)
    penche : bool; (*5.0 ou 0.0 selon si la gravité est inclinée ou non*)
    cam : camera_status;
}


let couleur_fond = Color.create 0 0 100 1

let draw st = 
  (*let pxf (x,_) = (x *. st.z +. (fst st.shift)) in
  let pyf (_,y)= (y *. st.z +. (snd st.shift)) in
  let px a = a |> pxf |> iof in 
  let py a = a |> pyf |> iof in
  let draw_tri a b c col =
      draw_triangle  
        (Vector2.create (pxf a) (pyf a))
        (Vector2.create (pxf b) (pyf b))
        (Vector2.create (pxf c) (pyf c))
        col
      in
  let draw_vec src f col = 
            let f = f *$ fac_newt in
            let end_force = src +$ f in 
             begin (*draw arrow*)
            draw_line (px src) (py src) (px end_force) (py end_force) col;
            let pi = 3.14159265359 in
            let angl = 2.0*.pi/.(norme f) in
            let dir_left = end_force -$ ((rotate f angl) *$ 0.25) in
            let dir_right = end_force -$ ((rotate f (-.angl)) *$ 0.25) in
            draw_tri dir_left dir_right end_force col;
            end in*)
  begin_drawing ();
  let s = st.cam in
  let dir = (
      cos s.target.horizontal *. cos s.target.vertical,
      cos s.target.horizontal *. sin s.target.vertical,
      sin s.target.horizontal
      ) in
  clear_background Color.black;
  begin_mode_3d s.camera;
     draw_cube zero_r 1.0 1.0 1.0 Color.raywhite; 
     draw_cube zero_r 10.0 1.0 10.0 Color.raywhite; 
     draw_grid 100 10.0;
     Camera3D.set_position s.camera (r3_to_vec3 s.position);
     Camera3D.set_target s.camera (r3_to_vec3 (s.position +$ (dir *$ 5.))); 
    let posa = (0, foi h)  in
    let midpos = Graph.random st.l in 
    (*draw_rectangle 0 (py posa) w (500.0*.st.z |> iof)   Color.gray; *)
    Graph.iteri (fun i s ->
    (*    let f =  (bilan_des_forces s i st.l {penche = st.penche; l = st.l; k_ressort = st.k_ressort}) in
        if is_key_down Key.F then (*juste les forces*)
          List.iter (fun (f,col) -> draw_vec s.pos f col) f

        else if is_key_down Key.G then  (* l'accélération*)
          draw_vec s.pos (somme_forces f) Color.orange

        else if is_key_down Key.S  && i >= ring * (rings-1) then (*la surface*)
            draw_tri s.pos (droite st.l i).pos midpos.pos Color.red

        else if not (is_key_down Key.S) then*)
        begin 
            (*draw_text (string_of_int i) (px s.pos) (py s.pos) 10 Color.raywhite;*)
            (*List.iter (fun (posb,_) ->
                 let a  = Vector2.create (pxf s.pos) (pyf s.pos) in
                 let b  = Vector2.create (pxf posb.pos) (pyf posb.pos) in
                draw_line_ex a b 2.0 Color.raywhite) (linked_to st.l i); *)
            draw_cube (r3_to_vec3 s.pos) 10.0 10.0 10.0 Color.yellow;
        end 
     
          ) 
      st.l;
     end_mode_3d ();
     draw_text "Space S X arrows" 10 10 20 Color.white;
      draw_line 10 10 (10 + iof (100.0*.st.z)) 10 Color.white;
      draw_text (string_of_float st.z ^
    "+/- pour le zoom
    F pour le mode forces
    Arrows pour le deplacement
    Space pour le saut temporel
    Q pour recentrer
    R pour la bascule (" ^ string_of_bool st.penche ^ ")
    " ^ string_of_float st.k_ressort ^ "N/m force de ressort elastique, modifiable avec h/y" 
        ) 10 20 20 Color.raywhite;
     begin_drawing ();
     end_drawing ()

let update_cam s =
   let pos' = s.position 
          +$ ((if is_key_down Key.W then (1.0,0.,0.) else (0.,0.,0.) +$ if is_key_down Key.S then (-1.0,0.,0.) else (0.,0.,0.)) *$ sin s.target.horizontal)
          +$ ((if is_key_down Key.A then (0.,1.0,0.) else (0.,0.,0.) +$ if is_key_down Key.D then (0.,-1.0,0.) else (0.,0.,0.)) *$ cos s.target.horizontal)
          +$ if is_key_down Key.Space then (0.,0.,1.0) else (0.,0.,0.) +$ if is_key_down Key.Left_shift then (0.,0.,-1.0) else (0.,0.,0.)
          in
    let target' = {
          horizontal = s.target.horizontal +. 0.02*.(if is_key_down Key.Up then 1.0 else 0.0 -. if is_key_down Key.Down then 1.0 else 0.0);
          vertical = s.target.vertical +. 0.02*.(if is_key_down Key.Right then 1.0 else 0.0 -. if is_key_down Key.Left then 1.0 else 0.0);
      } in
    {s with position = pos'; target = target'}

let rec loop st =
  if Raylib.window_should_close () then Raylib.close_window () else 
  let integrationDomain = Domain.spawn (fun _ -> Integration.integrate {l=st.l;k_ressort = st.k_ressort;penche = st.penche}) in
  let time_jumping = not (is_key_down Key.I) in
  if time_jumping || st.t mod 30 = 0 then draw st;
  let rmed = (Array.fold_left (fun a x -> a +$ x.pos) zero st.l) *$ (1.0/.(foi n)) 
  in
  (*let vitesse = -10.0 in
  let shift' = List.fold_left (+$) st.shift
      [
          if is_key_down Key.Up then 0.0,-.vitesse,0.0 else zero; 
          if is_key_down Key.Down then 0.0,vitesse,0.0 else zero; 
          if is_key_down Key.Left then -.vitesse,0.0,0.0 else zero; 
          if is_key_down Key.Right then vitesse,0.0,0.0 else zero; 
     ] 
  in*)
  (*let ideal = foi (w/2), foi (h/2),0.0 in
  let shift' = if st.penche then (shift' *$ 0.9) +$ ((ideal -$ rmed *$ st.z) *$ 0.1) else shift'  in*)
  let l' = Domain.join integrationDomain in
  loop {
      t = st.t + 1;
      l = l';
      shift = zero;
      z = max 0.0 (st.z +. let vitesse = 0.01 in if is_key_down Key.Kp_add then vitesse else if is_key_down Key.Kp_subtract then -.vitesse else 0.0) ;(*zoom*)
      k_ressort = max 0.0 (st.k_ressort +. let vitesse = 1.0 in if is_key_down Key.H then vitesse else if is_key_down Key.Y then -.vitesse else 0.0);
      penche = if is_key_pressed Key.R then not st.penche else st.penche;
      cam = update_cam st.cam 
  }

let setup () =
  Raylib.init_window w h "Blob";
  Raylib.set_target_fps 60;
  if is_window_ready () then
  {
      t = 0;
      l = Graph.initial ();
      shift = zero;
      z = 1.0;
      penche = false;
      k_ressort = k_ressort;
      cam = {
        camera = Camera3D.create zero_r zero_r (Vector3.create 0. 1. 0.) 45. CameraProjection.Perspective;
        position = (0.0,-20.0,10.0);
        target = {
            vertical = 0.0;
            horizontal = 0.0;
        }
      }
  }
  else failwith "window not ready"

let () =  
  setup () |> loop;

  
