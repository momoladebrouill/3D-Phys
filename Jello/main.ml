open Raylib

let foi = float_of_int
let iof = int_of_float

(*Screen*)
let k = 120
let w = 16*k
let h = 9*k

(*Constantes*)
let floor_y = foi (h+5200) (*sol*)
let dt = 0.1 
let mass = 10.0 (*masses individuelles des particules*)
let d_eq = 100

(*Taille du blob carré*)
let w_blob = 10
let h_blob = 10
let n = w_blob * h_blob

type vec = 
    float * float

type point = {
    pos : vec;
    vit : vec;
    col : float;(*couleur du point*)
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
let ( *$) (a,b) q = (a*.q, b*.q)
let (+%) = Array.map2 (+$)
let mult q = Array.map (fun t-> t *$ q)
let ( *%) l q = Array.map (fun a -> {pos = a.pos *$ q; vit = a.vit *$ q; col = a.col *. q}) l

let next  i = (i+1) mod n
let prev  i = (n+i-1) mod n
let dist (xa,ya) (xb,yb) = sqrt ((xa-.xb)**2.0 +. (ya-.yb)**2.0)

let vect_elem (xa,ya) (xb,yb) =
    let d = dist (xa,ya) (xb,yb) in
    ((xb-.xa)/.d,(yb-.ya)/.d)
    (*let theta = atan2 (ya -. yb) (xa-.xb) in (cos theta,sin theta*) 

let f_ressort a b l0 k =
   vect_elem a b *$ (-.k*.(dist a b -.l0))

let attrac a b ma mb= 
    vect_elem b a *$ (ma *. mb /. (dist a b)**2.0)

let linked_to i = (*to make a square*)
  let sqrt2 = sqrt 2.0 in 
  let east = [i+1,1.0;i+w_blob+1,sqrt2;i-w_blob+1,sqrt2] in
  let west = [i-1,1.0;i+w_blob-1,sqrt2;i-w_blob-1,sqrt2] in
  let l = (i-w_blob,1.0)::(i+w_blob,1.0)::(if i mod w_blob =  0 then east else if i mod w_blob = w_blob-1 then west else east @ west)
  in List.filter (fun (j,_) -> j >= 0 && j < n) l

let bilan_des_forces pos vit c i l=
    [
        (0.0,0.0), Color.green; (*champs de pesanteur*)  
        vit *$ (-0.1), Color.blue; (*force de frottement sur surface*)
        attrac c pos mass 10000.0, Color.yellow (*force newtonienne avec le centre*)
  ] @ List.map (fun (i,d) -> f_ressort l.(i).pos pos ((foi d_eq)*.d) 1.0, Color.purple) (linked_to i) (*force elastique avec les autres *)
 
let rec loop status =
  
  if Raylib.window_should_close () then Raylib.close_window () else 
  let t,l,shift = status.t,status.l,status.shift in
  clear_background Color.black;
  
  begin_drawing ();
  let px (x,_) = iof (x*.status.z +. (fst shift)) in 
  let py (_,y) = iof (y*.status.z +. (snd shift)) in
    let ray = 10.0 in
    let c = (foi (w/2)) +. ray*.cos ((foi t)/.100.0) , (ray*.sin ((foi t)/.100.0) -. 100.0 ) in 
    draw_circle (px c) (py c) 5.0 Color.white;
    let pos = foi (-w)*.status.z, foi h in
    draw_rectangle (px pos) (py pos) (iof ((foi (2*w))*.status.z)) h  Color.white; 
    Array.iteri (fun i s ->
        let pos = s.pos +$ (-5.0,0.0) in
          draw_rectangle (px pos) (py pos) 10 10 (Raylib.color_from_hsv s.col 1.0 1.0);
          (*draw_text (string_of_float s.col) (iof x) (iof y) 20 Color.raywhite;*)
        let f = bilan_des_forces s.pos s.vit c i l in
        let fac_newt = 1.0 in
        List.iter (fun (f,col) -> 
            let end_force = pos +$ (f *$ fac_newt) in 
            draw_line (px pos) (py pos) (px end_force) (py end_force) col) f;
          
        List.iter (fun (posb,_) ->
          let endv = pos +$ ((vect_elem pos l.(posb).pos)*$50.0) in
        draw_line (px pos) (py pos) (px endv) (py endv) Color.gray) (linked_to i)

    ) l;

    draw_text (string_of_float status.z) 0 0 20 Color.raywhite;
  end_drawing ();

  let vitesse = -50.0 in
  let shift' = List.fold_left (+$) shift
      [
          if is_key_down Key.Up then 0.0,-.vitesse else zero; 
          if is_key_down Key.Down then 0.0,vitesse else zero; 
          if is_key_down Key.Left then -.vitesse,0.0 else zero; 
          if is_key_down Key.Right then vitesse,0.0 else zero; 
     ] 
  in
  let h = 0.01 in
  let h2 = h/.2.0 in
  let h4 = h*.h/.4.0 in
  let h6 = h/.6.0 in
  let somme_des_forces i s  = List.fold_left (fun x (f,_) -> x +$ f ) zero (bilan_des_forces s.pos s.vit c i l) in
  let y' = Array.map (fun x -> x.vit) l in
  let y = Array.map (fun x -> x.pos) l in
  let f y y' =
      Array.init n (fun i ->somme_des_forces i {pos = y.(i);vit = y'.(i); col = 10.0}) in
  let k1 = f y y'  in
  let k2 = f (y +% mult h2 y' ) (y' +% mult h2 k1)  in
  let k3 = f (y +% mult h2 y' +% mult h4 k1) (y' +% mult h2 k2)  in
  let k4 = f (y +% mult h y' +% mult (h*.h2) k2) (y' +% mult h k3) in
  let ny = y +% mult h y' +% mult (h*.h6) (k1 +% k2 +% k3) in
  let ny' = y' +% mult h6 (k1 +% mult 2.0 k2 +% mult 2.0 k3 +% k4) in
  let l' = Array.mapi (fun i x -> { pos = ny.(i); vit = ny'.(i); col = x.col}) l in
  loop {
      t = t+1;
      l = l';
      shift = shift';
      z = max 0.0 ( status.z +. (if is_key_down Key.Kp_add then 0.01 else 0.0) +. (if is_key_down Key.Kp_subtract then -0.01 else 0.0)) 
   }

let setup () =
  Random.self_init (); 
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  {
      t = 0;
      l = Array.init n (fun i -> 
          {
              pos = foi (w/2 + (d_eq)*(i mod w_blob - w_blob/2 )),
                    (foi (h/2)) +. (foi d_eq) *. (foi (i/h_blob - h_blob/2));
              vit = zero;
              col = Random.float 50.
          });
      shift = zero;
      z = 1.0;
  }

let () =  
  setup () |> loop 
