open Raylib

let foi = float_of_int
let iof = int_of_float

(*Screen*)
let k = 120
let w = 16*k
let h = 9*k

(*Constantes*)
let floor_y = foi h (*sol*)
let dt = 1.0/.60.0 
let mass = 20.0 (*masses des particules de reference*)
let d_eq = 25.0
let fac_newt = 0.1
(*Taille du blob carré*)
let w_blob = 10
let h_blob = 10
let n = w_blob * h_blob

type vec = float * float

type point = {
    pos : vec;
    vit : vec;
    mass : float;(*couleur du point*)
}

type status = {
    t : int; (*temps*)
    l : point array; (*le tableau des points*)
    z : float; (*zoom*) 
    shift : vec; (*shift de la cam*)
}

(*Basic functions*)
let zero = 0.0,0.0
let (+$) (a,b) (c,d) = (a+.c, b+.d)
let (-$) (a,b) (c,d) = (a-.c, b-.d)
let ( *$) (a,b) q = (a*.q, b*.q)
let ps (a,b) (c,d) = a*.c +. b*.d  
let (+%) = Array.map2 (+$)
let mult q = Array.map (fun t-> t *$ q)

let next  i = (i+1) mod n
let prev  i = (n+i-1) mod n
let dist (xa,ya) (xb,yb) = sqrt ((xa-.xb)**2.0 +. (ya-.yb)**2.0)

let vect_elem (xa,ya) (xb,yb) =
    let d = dist (xa,ya) (xb,yb) in
    ((xb-.xa)/.d,(yb-.ya)/.d)
    (*let theta = atan2 (ya -. yb) (xa-.xb) in (cos theta,sin theta*) 

let f_ressort a b l0 k = vect_elem b.pos a.pos *$ (-.k*.(dist a.pos b.pos -.l0))
let f_amortisseur src dst d = 
    let er = vect_elem src.pos dst.pos in
    er *$ (d *. ps (src.vit -$ dst.vit) er)

let abs_f x = if x < 0.0 then -.x else x

let attrac a b ma mb = vect_elem b a *$ (ma *. mb /. (dist a b)**2.0)

let linked_to i = (*to make a square*)
  let sqrt2 = sqrt 2.0 in 
  let east = [i+1,1.0; i+w_blob+1,sqrt2; i-w_blob+1,sqrt2] in
  let west = [i-1,1.0; i+w_blob-1,sqrt2; i-w_blob-1,sqrt2] in
  let l = (i - w_blob, 1.0)::(i + w_blob, 1.0)::(if i mod w_blob =  0 then east else if i mod w_blob = w_blob-1 then west else east @ west)
  in List.filter (fun (j,_) -> j >= 0 && j < n) l

let bilan_des_forces s i l dt =
    let l = [
        (0.0,9.81) *$ s.mass, Color.green; (*champs de pesanteur*)  
  ] @ List.concat (List.map (fun (i,d) -> 
      [f_ressort s l.(i) (d_eq*.d) 300.0, Color.purple;(*force elastique avec les autres *)
      f_amortisseur s l.(i) (-15.0), Color.raywhite](*force damped avec les autres *)
      ) (linked_to i)) 
    in if snd (s.pos +$ (s.vit  *$ dt)) > floor_y  then
         let f = List.fold_left (fun x (f,_) -> x +$ f) zero l in (*RN*)
          ((0.0, -.2.0*.(abs_f (snd f))), Color.red)::l 
  else l  

let somme_des_forces i s l dt = List.fold_left (fun x (f,_) -> x +$ f ) zero (bilan_des_forces s i l dt) 

let rec loop status =
  
  if Raylib.window_should_close () then Raylib.close_window () else 
  let t,l,shift = status.t,status.l,status.shift in
  let time_jumping = is_key_down Key.Space in
  if ((time_jumping)) || t mod 30 = 0 then begin 

  clear_background Color.black;
  let px (x,_) = iof (x *. status.z +. (fst shift)) in 
  let py (_,y) = iof (y *. status.z +. (snd shift)) in
    let posa = foi (-w), foi h in
    draw_rectangle (px posa) (py posa) ((2*w |> foi)*.status.z |> iof) (500.0*.status.z |> iof)  Color.white; 
    Array.iteri (fun i s ->
        let f =  (bilan_des_forces s i l dt) in
        let pos = s.pos in
        
        (*dessin des forces*)
        if is_key_down Key.F then 
        List.iter (fun (f,col) -> 
            let end_force = pos +$ (f *$ (fac_newt*.status.z)) in 
            draw_line (px pos) (py pos) (px end_force) (py end_force) col) f
        (*dessin des ressorts*)  
        else
        List.iter (fun (posb,fac) -> let d = dist pos l.(posb).pos  in
        draw_line (px pos) (py pos) (px l.(posb).pos) (py l.(posb).pos) (Raylib.color_from_hsv 0.0 (abs_f (d -. d_eq*.fac)/.50.0) 1.0 )) (linked_to i)
    ) l;

  draw_text (string_of_float status.z ^
"+/- pour le zoom
F pour le mode forces
Arrows pour le deplacement
Space pour le saut temporel"
    ) 0 0 20 Color.raywhite;
  begin_drawing ();
  end_drawing ();
  end;
  let vitesse = -1.0 in
  let shift' = List.fold_left (+$) shift
      [
          if is_key_down Key.Up then 0.0,-.vitesse else zero; 
          if is_key_down Key.Down then 0.0,vitesse else zero; 
          if is_key_down Key.Left then -.vitesse,0.0 else zero; 
          if is_key_down Key.Right then vitesse,0.0 else zero; 
     ] 
  in
  let h = 0.001 in
  let h2 = h/.2.0 in
  let h4 = h*.h/.4.0 in
  let h6 = h/.6.0 in
  let y' = Array.map (fun x -> x.vit) l in
  let y = Array.map (fun x -> x.pos) l in
  let f y y' dt = Array.init n (fun i -> ((somme_des_forces i {pos = y.(i);vit = y'.(i); mass = l.(i).mass} l dt) *$ (1.0/.mass))) in
  let k1 = f y y'  0.01 in
  let k2 = f (y +% mult h2 y' ) (y' +% mult h2 k1) h2 in
  let k3 = f (y +% mult h2 y' +% mult h4 k1) (y' +% mult h2 k2) h2 in
  let k4 = f (y +% mult h y' +% mult (h*.h2) k2) (y' +% mult h k3) h in
  let ny = y +% mult h y' +% mult (h*.h6) (k1 +% k2 +% k3) in
  let ny' = y' +% mult h6 (k1 +% mult 2.0 k2 +% mult 2.0 k3 +% k4) in
  let l' = Array.mapi (fun i x -> { pos = ny.(i); vit = ny'.(i); mass = x.mass}) l in
  loop {
      t = t+1;
      l = l';
      shift = shift';
      z = let vitesse = 0.01 in max 0.0 ( status.z +. (if is_key_down Key.Kp_add then vitesse else 0.0) +. (if is_key_down Key.Kp_subtract then -.vitesse  else 0.0)) 
   }

let setup () =
  Random.self_init (); 
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  let d_init_fac = 0.9 in
  {
      t = 0;
      l = Array.init n (fun i -> 
          {
              pos = foi (w/2 + (iof (d_eq*.d_init_fac)) * (i mod w_blob - w_blob/2 )),
                    (foi (h/2)) +. d_eq*.d_init_fac *. (foi (i/h_blob - h_blob/2));
              vit = (50.0,0.0);
              mass = mass (*.(foi i)/.(foi n) *)
          });
      shift = zero;
      z = 1.0;
  }

let () =  
  setup () |> loop 
