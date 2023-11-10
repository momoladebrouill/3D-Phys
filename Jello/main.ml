open Raylib
open Force
open Maths  
open Constantes
open Rk4

type status = {
    t : int; (*temps*)
    l : point array; (*le tableau des points*)
    z : float; (*zoom*) 
    shift : vec; (*shift de la cam*)
    k_ressort : float;
}


let rec loop st =
  if Raylib.window_should_close () then Raylib.close_window () else 
  let px (x,_) = iof (x *. st.z +. (fst st.shift)) in 
  let py (_,y) = iof (y *. st.z +. (snd st.shift)) in
  let time_jumping = not (is_key_down Key.Space) in
  if time_jumping || st.t mod 30 = 0 then begin 
        (*affichage*)
  clear_background Color.darkblue;
    let posa = foi (-w), foi h in
    draw_rectangle (px posa) (py posa) ((foi (2*w))*.st.z |> iof) (500.0*.st.z |> iof)  Color.gray; 
    Array.iteri (fun i s ->
        (*dessin des forces*)
        let fac_newt = 0.1 in
        if is_key_down Key.F then (*juste les forces*)
          let f =  (bilan_des_forces s i st.l dt st.k_ressort) in
          List.iter (fun (f,col) -> 
            let end_force = s.pos +$ (f *$ (fac_newt*.st.z)) in 
            draw_line (px s.pos) (py s.pos) (px end_force) (py end_force) col) f
        else if is_key_down Key.G then  (* l'accélération*)
          let f =  (bilan_des_forces s i st.l dt st.k_ressort) in
          let end_force = s.pos +$ ((somme_forces f) *$ (fac_newt*.st.z)) in 
            draw_line (px s.pos) (py s.pos) (px end_force) (py end_force) Color.orange;
            (Printf.printf "%f %f" (fst st.shift) (snd st.shift); flush_all ())
        (*dessin des ressorts*)  
        else
          begin (*draw_text (string_of_int i) (px s.pos) (py s.pos) 10 Color.raywhite;*)
          List.iter (fun (posb,fac) -> let d = dist s.pos st.l.(posb).pos  in
          draw_line (px s.pos) (py s.pos) (px st.l.(posb).pos) (py st.l.(posb).pos) (Raylib.color_from_hsv 0.0 (abs_f (d -. d_eq*.fac)/.50.0) 1.0 )) (linked_to i) end
    ) st.l;

  draw_text (string_of_float st.z ^
"+/- pour le zoom
F pour le mode forces
Arrows pour le deplacement
Space pour le saut temporel
Q pour recentrer
" ^ string_of_float st.k_ressort ^ "N/m force de ressort elastique, modifiable avec h/y" 
    ) 0 0 20 Color.raywhite;
  begin_drawing ();
  end_drawing ();
  end;
  let vitesse = -100.0 in
  let shift' = List.fold_left (+$) st.shift
      [
          if is_key_down Key.Up then 0.0,-.vitesse else zero; 
          if is_key_down Key.Down then 0.0,vitesse else zero; 
          if is_key_down Key.Left then -.vitesse,0.0 else zero; 
          if is_key_down Key.Right then vitesse,0.0 else zero; 
     ] 
  in
  let rmed = (Array.fold_left (fun a x -> a +$ x.pos) zero st.l) *$ (1.0/.(foi n)) in
  let ideal = foi (w/2),foi (h/2) in
  let shift' = if is_key_down Key.A then (shift' *$ 0.9) +$ ((ideal -$ rmed *$ st.z) *$ 0.1) else shift'  in
  let l' = Rk4.runge_kunta {l=st.l;k_ressort = st.k_ressort} in
  loop {
      t =st.t+1;
      l = l';
      shift = shift';
      z = max 0.0 (st.z +. let vitesse = 0.01 in if is_key_down Key.Kp_add then vitesse else if is_key_down Key.Kp_subtract then -.vitesse else 0.0) ;
      k_ressort = max 0.0 (st.k_ressort +. let vitesse = 1.0 in if is_key_down Key.H then vitesse else if is_key_down Key.Y then -.vitesse else 0.0)
  }

let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  let d_init_fac = 0.8 in
  {
      t = 0;
      l = Array.init n (fun i -> 
          {
              pos = foi (w/2 + (iof (d_eq*.d_init_fac)) * (i mod w_blob - w_blob/2 )),
                    (foi (h/2)) +. d_eq*.d_init_fac *. (foi (i/h_blob - h_blob/2));
              vit = (0.0,0.0);
              mass = mass (*.(foi i)/.(foi n) *)
          });
      shift = zero;
      z = 1.0;
      k_ressort = k_ressort;
  }

let () =  setup () |> loop 
