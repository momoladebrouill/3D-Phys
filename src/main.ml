open Raylib
open Force
open Maths  
open Constantes
open Graph



type status = {
    t : int; (*temps*)
    l : point array; (*le tableau des points*)
    z : float; (*zoom*) 
    shift : vec; (*shift de la cam*)
    k_ressort : float; (*la constante de ressort*)
    penche : bool; (*5.0 ou 0.0 selon si la gravité est inclinée ou non*)
    cam : Camera3D.t;
}


let couleur_fond = Color.create 0 0 100 1

let draw st = 
  let draw_tri a b c col =
      draw_triangle_3d  
        (r3_to_vec3 a)
        (r3_to_vec3 b)
        (r3_to_vec3 c)
        col
      in
  let draw_vec src f col = 
            let f = f *$ fac_newt in
            let end_force = src +$ f in 
            let mid_force = src +$ (f *$ 0.75) in
             begin 
             (*draw arrow*)
            draw_line_3d (r3_to_vec3 src) (r3_to_vec3 end_force) col;
            draw_cylinder_ex (r3_to_vec3 mid_force) (r3_to_vec3 end_force) 1.0 0.0 10  col;
            end in
  begin_drawing ();
  clear_background Color.black;
  begin_mode_3d st.cam;
  draw_cube zero_r 1.0 1.0 1.0 Color.raywhite; 
  draw_cube zero_r 10.0 1.0 10.0 Color.raywhite; 
  draw_grid 100 10.0;
    let midpos = Graph.random st.l in 
    (*draw_rectangle 0 (py posa) w (500.0*.st.z |> iof)   Color.gray; *)
    Graph.iteri (fun i s ->
        let f =  (bilan_des_forces s i st.l {penche = st.penche; l = st.l; k_ressort = st.k_ressort}) in
        if is_key_down Key.F then (*juste les forces*)
          List.iter (fun (f,col) -> draw_vec s.pos f col) f

        else if is_key_down Key.G then  (* l'accélération*)
          draw_vec s.pos (somme_forces f) Color.orange

        else if is_key_down Key.S  && i >= ring * (rings-1) then (*la surface*)
            draw_tri s.pos (droite st.l i).pos midpos.pos Color.red

        else if not (is_key_down Key.S) then
        begin 
            (*draw_text (string_of_int i) (px s.pos) (py s.pos) 10 Color.raywhite;*)
            List.iter (fun (posb,_) ->
                 let a  = r3_to_vec3 s.pos in
                 let b  = r3_to_vec3 posb.pos in
                draw_line_3d a b  Color.gray) (linked_to st.l i); 
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


let rec loop st =
  if Raylib.window_should_close () then Raylib.close_window () else 
  let integrationDomain = Domain.spawn (fun _ -> Integration.integrate {l=st.l;k_ressort = st.k_ressort;penche = st.penche}) in
  let time_jumping = not (is_key_down Key.I) in
  if time_jumping || st.t mod 30 = 0 then draw st;
  let rmed = r3_to_vec3 ((Array.fold_left (fun a x -> a +$ x.pos) zero st.l) *$ (1.0/.(foi n))) 
  in
  
  update_camera (addr st.cam) CameraMode.Third_person;
  Vector3.set_x (Camera3D.target st.cam) (Vector3.x rmed);
  Vector3.set_y (Camera3D.target st.cam) (Vector3.y rmed);
  Vector3.set_z (Camera3D.target st.cam) (Vector3.z rmed);
  let l' = Domain.join integrationDomain in
  loop {
      t = st.t + 1;
      l = l';
      shift = zero;
      z = max 0.0 (st.z +. let vitesse = 0.01 in if is_key_down Key.Kp_add then vitesse else if is_key_down Key.Kp_subtract then -.vitesse else 0.0) ;(*zoom*)
      k_ressort = max 0.0 (st.k_ressort +. let vitesse = 1.0 in if is_key_down Key.H then vitesse else if is_key_down Key.Y then -.vitesse else 0.0);
      penche = if is_key_pressed Key.R then not st.penche else st.penche;
      cam = st.cam 
  }

let setup () =
  Raylib.init_window w h "Blob";
  Raylib.set_target_fps 60;
  if is_window_ready () then
    let camera = Camera3D.create zero_r zero_r (Vector3.create 0. 1. 0.) 45. CameraProjection.Perspective 
    in let open Camera3D in
    set_position camera (Vector3.create 0. 2. 4.);
    set_target camera (Vector3.create 0. 2. 0.);
    set_up camera (Vector3.create 0. 1. 0.);
    set_fovy camera 120.;
    set_projection camera CameraProjection.Perspective;
    disable_cursor ();
  {
      t = 0;
      l = Graph.initial ();
      shift = zero;
      z = 1.0;
      penche = false;
      k_ressort = k_ressort;
      cam = camera;
  }
  else failwith "window not ready"

let () =  
  setup () |> loop;
  Icosphere.demo ()

  
