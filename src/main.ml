open Raylib
open Force
open Maths  
open Constantes
open Rk4
open Graph

type status = {
    t : int; (*temps*)
    l : point array; (*le tableau des points*)
    z : float; (*zoom*) 
    shift : vec; (*shift de la cam*)
    k_ressort : float; (*la constante de ressort*)
    penche : float; (*1.0 ou 0.0 selon si la gravité est inclinée*)
}


let couleur_fond = Color.create 0 0 100 1

let rec loop st =
  if Raylib.window_should_close () then Raylib.close_window () else 
  
  let integrationDomain = Domain.spawn (fun _ -> Rk4.runge_kunta {l=st.l;k_ressort = st.k_ressort;penche = st.penche} 0) in
  let pxf (x,_) = (x *. st.z +. (fst st.shift)) in
  let pyf (_,y)= (y *. st.z +. (snd st.shift)) in
  let px a = a |> pxf |> iof in 
  let py a = a |> pyf |> iof in
  let time_jumping = not (is_key_down Key.Space) in
  if time_jumping || st.t mod 30 = 0 then begin 
        (*affichage*)
  begin_drawing ();
  clear_background couleur_fond;
    let posa = (0, foi h)  in
    let midpos = Vector2.create (pxf st.l.(0).pos) (pyf st.l.(0).pos) in 
    draw_rectangle 0 (py posa) w (500.0*.st.z |> iof)   Color.gray; 
    Graph.iteri (fun i s ->
        let fac_newt = 0.001 in
        let f =  (bilan_des_forces s i st.l st.k_ressort st.penche) in
        if is_key_down Key.F then (*juste les forces*)
          List.iter (fun (f,col) -> 
            let end_force = s.pos +$ (f *$ (fac_newt*.st.z)) in 
             begin (*draw arrow*)
            draw_line (px s.pos) (py s.pos) (px end_force) (py end_force) col;
            let dir = (end_force -$ s.pos) in
            let dir = dir /$ (norme dir) in
            let dir_left = rotate dir (3.14/.4.0) in
            let dir_right = rotate dir (-3.14/.4.0) in
            let dir_left = s.pos +$ dir_left in
            let dir_right = s.pos +$ dir_right in
            draw_triangle (Vector2.create (pxf end_force) (pyf end_force)) (Vector2.create (pxf dir_left) (pyf dir_left)) (Vector2.create (pxf dir_right) (pyf dir_right)) col;
            end
            ) f
        else if is_key_down Key.G then  (* l'accélération*)
          let end_force = s.pos +$ ((somme_forces f) *$ (fac_newt*.st.z)) in 
            draw_line (px s.pos) (py s.pos) (px end_force) (py end_force) Color.orange
        else if is_key_down Key.S  && i < ring (* ring * (rings-1)*) then (*la surface*)
            draw_triangle (Vector2.create (pxf s.pos) (pyf s.pos)) (Vector2.create (pxf st.l.(droite i).pos) (pyf st.l.(droite i).pos)) midpos Color.red
         else
          begin 
         draw_rectangle  (px s.pos) (py s.pos) 2 2 (if List.exists (fun (j,_)->snd st.l.(j).pos < snd s.pos) (linked_to i) then Color.raywhite else Color.red);
         draw_text (string_of_int i) (px s.pos) (py s.pos) 10 Color.raywhite;
          List.iter (fun (posb,_) -> 
          draw_line (px s.pos) (py s.pos) (px st.l.(posb).pos) (py st.l.(posb).pos) Color.raywhite) (linked_to i) end)  
      st.l;
  draw_line 10 10 (10 + iof (100.0*.st.z)) 10 Color.white;
  draw_text (string_of_float st.z ^
"+/- pour le zoom
F pour le mode forces
Arrows pour le deplacement
Space pour le saut temporel
Q pour recentrer
R pour la bascule
" ^ string_of_float st.k_ressort ^ "N/m force de ressort elastique, modifiable avec h/y" 
    ) 10 20 20 Color.raywhite;
  end_drawing ();
  end;
  let vitesse = -10.0 in
  let shift' = List.fold_left (+$) st.shift
      [
          if is_key_down Key.Up then 0.0,-.vitesse else zero; 
          if is_key_down Key.Down then 0.0,vitesse else zero; 
          if is_key_down Key.Left then -.vitesse,0.0 else zero; 
          if is_key_down Key.Right then vitesse,0.0 else zero; 
     ] 
  in
  let rmed = (Array.fold_left (fun a x -> a +$ x.pos) zero st.l) *$ (1.0/.(foi n)) in
  let ideal = foi (w/2), foi (h/2) in
  let shift' = if is_key_down Key.A then (shift' *$ 0.9) +$ ((ideal -$ rmed *$ st.z) *$ 0.1) else shift'  in
  let l' = Domain.join integrationDomain in
  loop {
      t = st.t + 1;
      l = l';
      shift = shift';
      z = max 0.0 (st.z +. let vitesse = 0.01 in if is_key_down Key.Kp_add then vitesse else if is_key_down Key.Kp_subtract then -.vitesse else 0.0) ;
      k_ressort = max 0.0 (st.k_ressort +. let vitesse = 1.0 in if is_key_down Key.H then vitesse else if is_key_down Key.Y then -.vitesse else 0.0);
      penche = if is_key_down Key.R then 1.0 else 0.0
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
      penche = 0.0;
      k_ressort = k_ressort;
  }
  else failwith "window not ready"

let () =  
  setup () |> loop;

  
