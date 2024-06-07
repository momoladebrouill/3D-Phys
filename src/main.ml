open Raylib
open Force
open Maths  
open Constantes
open Rk4

type status = {
    frame : int; (*numero de la frame*)
    l : point array; (*le tableau des points*)
    z : float; (*zoom*) 
    shift : vec; (*shift de la cam*)
    k_ressort : float; (*la constante de ressort*)
    penche : float; (*1.0 ou 0.0 selon si la gravité est inclinée*)
}

let draw st =
  let px (x,_) = iof (x *. st.z +. (fst st.shift)) in 
  let py (_,y) = iof (y *. st.z +. (snd st.shift)) in
  let time_jumping = not (is_key_down Key.Space) in
  if time_jumping || st.frame mod 30 = 0 then begin 
        (*affichage*)
  clear_background (Color.create 0xfd 0xf6 0xe3 0xff);
    let posa = (0, foi h)  in
    draw_rectangle 0 (py posa) w (500.0*.st.z |> iof)   Color.gray; 
    Array.iteri (fun i s ->
        let fac_newt = 0.01 in
        let f =  (bilan_des_forces s i st.l dt st.k_ressort st.penche) in
        if is_key_down Key.F then (*juste les forces*)
          List.iter (fun (f,col) -> 
            let end_force = s.pos +$ (f *$ (fac_newt*.st.z)) in 
            draw_line (px s.pos) (py s.pos) (px end_force) (py end_force) col) f
        else if is_key_down Key.G then  (* l'accélération*)
          let end_force = s.pos +$ ((somme_forces f) *$ (fac_newt*.st.z)) in 
						let a = Vector2.create (foi (px s.pos)) (foi (py s.pos)) in
						let b = Vector2.create (foi (px end_force)) (foi (py end_force)) in
						draw_line_ex a b 5.0 (Color.orange)
        else
          begin 
          List.iter (fun (posb,fac) -> 
          let a = Vector2.create (foi (px s.pos)) (foi (py s.pos)) in
          let b = Vector2.create (foi (px st.l.(posb).pos)) (foi (py st.l.(posb).pos)) in
          draw_line_ex a b 5.0 (Color.create 0xd3 0x36 0x82 0xff)) (linked_to i)
          end)  
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
  begin_drawing ();
  end_drawing ();
  end

let abs_f x = if x > 0.0 then x else -.x

let rec loop st =
  if Raylib.window_should_close () then Raylib.close_window () else 
  
  let integrationDomain = Domain.spawn (fun _ -> Rk4.runge_kunta {l=st.l;k_ressort = st.k_ressort;penche = st.penche} 0) in
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
  let ideal = foi (w/2), foi (h/2) in
  let shift' = if is_key_down Key.A then (shift' *$ 0.9) +$ ((ideal -$ rmed *$ st.z) *$ 0.1) else shift'  in
  let l' = Domain.join integrationDomain in
	let t = Sys.time () in
	draw st;
	let frame' = 0 in
	if Sys.time () > 60.0 then ()  else
  loop {
      frame = frame';
      l = l';
      shift = shift';
      z = max 0.0 (st.z +. let vitesse = 0.01 in if is_key_down Key.Kp_add then vitesse else if is_key_down Key.Kp_subtract then -.vitesse else 0.0) ;
      k_ressort = max 0.0 (st.k_ressort +. let vitesse = 1.0 in if is_key_down Key.H then vitesse else if is_key_down Key.Y then -.vitesse else 0.0);
      penche = if is_key_down Key.R then 1.0 else 0.0
  }

let setup () =
  Raylib.init_window w h "Blob";
  if is_window_ready () then
  let d_init_fac = 0.8 in
  let d = d_eq*.d_init_fac in
  {
      frame = 0;
      l = Array.init n (fun i -> 
          {
              pos = foi (w/2) +. d *. (foi (i mod w_blob) -. 1.0),
                    foi (h/2) +. d *. (foi (i/w_blob));
              vit = (0.0,0.0);
              mass = mass; (*.(foi i)/.(foi n) *)
          });
      shift = zero;
      z = 1.0;
      penche = 0.0;
      k_ressort = k_ressort;
  }
  else failwith "window not ready"

let () =
  setup () |> loop 
