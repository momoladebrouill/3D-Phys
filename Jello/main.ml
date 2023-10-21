open Raylib
open Force
open Maths  
open Constantes

type status = {
    t : int; (*temps*)
    l : point array; (*le tableau des points*)
    z : float; (*zoom*) 
    shift : vec; (*shift de la cam*)
    k_ressort : float;
}

let rec loop st =
  if Raylib.window_should_close () then Raylib.close_window () else 
  let time_jumping = not (is_key_down Key.Space) in
  if time_jumping || st.t mod 30 = 0 then begin 
        (*affichage*)
  clear_background Color.black;
  let px (x,_) = iof (x *. st.z +. (fst st.shift)) in 
  let py (_,y) = iof (y *. st.z +. (snd st.shift)) in
    let posa = foi (-w), foi h in
    draw_rectangle (px posa) (py posa) ((2*w |> foi)*.st.z |> iof) (500.0*.st.z |> iof)  Color.white; 
    Array.iteri (fun i s ->
        let pos = s.pos in
        
        (*dessin des forces*)
        let fac_newt = 0.1 in
        if is_key_down Key.F then 
          let f =  (bilan_des_forces s i st.l dt st.k_ressort) in
          List.iter (fun (f,col) -> 
            let end_force = pos +$ (f *$ (fac_newt*.st.z)) in 
            draw_line (px pos) (py pos) (px end_force) (py end_force) col) f
        else if is_key_down Key.G then 
          let f =  (bilan_des_forces s i st.l dt st.k_ressort) in
          let end_force = pos +$ ((somme_vecs f) *$ (fac_newt*.st.z)) in 
            draw_line (px pos) (py pos) (px end_force) (py end_force) Color.orange 
        (*dessin des ressorts*)  
        else
        List.iter (fun (posb,fac) -> let d = dist pos st.l.(posb).pos  in
        draw_line (px pos) (py pos) (px st.l.(posb).pos) (py st.l.(posb).pos) (Raylib.color_from_hsv 0.0 (abs_f (d -. d_eq*.fac)/.50.0) 1.0 )) (linked_to i)
    ) st.l;

  draw_text (string_of_float st.z ^
"+/- pour le zoom
F pour le mode forces
Arrows pour le deplacement
Space pour le saut temporel
" ^ string_of_float st.k_ressort ^ "N/m force de ressort elastique, modifiable avec h/y" 
    ) 0 0 20 Color.raywhite;
  begin_drawing ();
  end_drawing ();
  end;
  let vitesse = -1.0 in
  let shift' = List.fold_left (+$) st.shift
      [
          if is_key_down Key.Up then 0.0,-.vitesse else zero; 
          if is_key_down Key.Down then 0.0,vitesse else zero; 
          if is_key_down Key.Left then -.vitesse,0.0 else zero; 
          if is_key_down Key.Right then vitesse,0.0 else zero; 
     ] 
  in
  let h = 5.0 *. dt in
  let h2 = h/.2.0 in
  let h4 = h*.h/.4.0 in
  let h6 = h/.6.0 in
  let y' = Array.map (fun x -> x.vit) st.l in
  let y = Array.map (fun x -> x.pos) st.l in
  let f h y y' = Array.init n (fun i -> somme_vecs (bilan_des_forces {pos = y.(i);vit = y'.(i); mass = st.l.(i).mass} i st.l h st.k_ressort) *$ (1.0/.mass)) in
  let k1 = f 0.0 y y' in
  let k2 = f h2 (y +% mult h2 y' ) (y' +% mult h2 k1) in
  let k3 = f h2 (y +% mult h2 y' +% mult h4 k1) (y' +% mult h2 k2)  in
  let k4 = f h (y +% mult h y' +% mult (h*.h2) k2) (y' +% mult h k3)  in
  let ny = y +% mult h y' +% mult (h*.h6) (k1 +% k2 +% k3) in
  let ny' = y' +% mult h6 (k1 +% mult 2.0 k2 +% mult 2.0 k3 +% k4) in
  let l' = Array.mapi (fun i x -> { pos = ny.(i); vit = ny'.(i); mass = x.mass}) st.l in
  loop {
      t = st.t+1;
      l = l';
      shift = shift';
      z = max 0.0 (st.z +. let vitesse = 0.01 in if is_key_down Key.Kp_add then vitesse else if is_key_down Key.Kp_subtract then -.vitesse else 0.0) ;
      k_ressort = max 0.0 (st.k_ressort +. let vitesse = 1.0 in if is_key_down Key.H then vitesse else if is_key_down Key.Y then -.vitesse else 0.0)
  }

let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  let d_init_fac = 0.9 in
  {
      t = 0;
      l = Array.init n (fun i -> 
          {
              pos = foi (w/2 + (iof (d_eq*.d_init_fac)) * (i mod w_blob - w_blob/2 )),
                    (foi (h/2)) +. d_eq*.d_init_fac *. (foi (i/h_blob - h_blob/2));
              vit = (0.0,50.0);
              mass = mass (*.(foi i)/.(foi n) *)
          });
      shift = zero;
      z = 1.0;
      k_ressort = k_ressort;
  }

let () =  setup () |> loop 
