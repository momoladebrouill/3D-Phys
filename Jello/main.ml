open Raylib

(*Screen*)
let k = 120
let w = 16*k
let h = 9*k

(*Constantes*)
let floor_y = float_of_int (h+5200) (*sol*)
let dt = 0.01 
let mass = 10.0 (*masses individuelles des particules*)
let foi = float_of_int
let iof = int_of_float

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
    l : point array;
    shift : vec; (*shift de la cam*)
}

(*Basic functions*)
let zero = 0.0,0.0
let (+$) (a,b) (c,d) = (a+.c, b+.d)
let ( *$) (a,b) q = (a*.q, b*.q)
let (+%) = Array.map2 (fun a b -> {pos = a.pos +$ b.pos; vit = a.vit +$ b.vit; col = a.col +. b.col})
let ( *%) l q = Array.map (fun a -> {pos = a.pos *$ q; vit = a.vit *$ q; col = a.col *. q}) l

let next  i = (i+1) mod n
let prev  i = (n+i-1) mod n
let dist (xa,xb) (ya,yb) = sqrt ((xa -.xb)**2.0 +. (ya-.yb)**2.0)

let vect_elem (xa,ya) (xb,yb) =
    let theta = atan2 (ya -. yb) (xa-.xb) in (cos theta,sin theta) 

let f_ressort a b l0 k =
   vect_elem a b *$ (k*.(dist a b -.l0))

let attrac a b ma mb= 
    vect_elem a b *$ (ma *. mb /. (dist a b)**2.0)

let linked_to i = (*to make a square*)
  let east = [i+1,1.0;i+w_blob+1,1.41;i-w_blob+1,1.41] in
  let west = [i-1,1.0;i+w_blob-1,1.41;i-w_blob-1,1.41] in
  let l = (i-w_blob,1.0)::(i+w_blob,1.0)::(if i mod w_blob =  0 then east else if i mod w_blob = w_blob-1 then west else east @ west)
  in List.filter (fun (j,_) -> j >= 0 && j < n) l

let bilan_des_forces pos vit c i l=
    [
        (0.0,0.0), Color.green; (*champs de pesanteur*)  
        vit *$ (-0.1), Color.blue; (*force de frottement sur surface*)
        attrac c pos mass 50.0, Color.yellow (*force newtonienne avec le centre*)
  ] @ List.map (fun (i,d) -> f_ressort l.(i).pos pos (100.0*.d) 1.1, Color.purple) (linked_to i) (*force elastique avec les autres *)
  
let rec loop status =
  if Raylib.window_should_close () then Raylib.close_window () else 
  let t,l,shift = status.t,status.l,status.shift in
  clear_background Color.black;
  
  let ray = 10.0 in
  let c = ((float_of_int (w/2) +. ray*.cos ((float_of_int t)/.100.0) ), (float_of_int (h/2)) +. ray*.sin ((float_of_int t)/.100.0) ) in 

  
  begin_drawing ();
  let cx,cy = ((float_of_int (w/2) +. ray*.cos ((float_of_int t)/.100.0) ), (float_of_int (h/2)) +. ray*.sin ((float_of_int t)/.100.0) ) in 
  let sx,sy = shift in
    draw_circle (iof (cx +. sx)) (iof (cy +. sy)) 5.0 Color.white;
    draw_rectangle 0 (iof (floor_y +. sy)) w (h-(iof (floor_y +. sy))) Color.white; 
    Array.iteri (fun i s ->
        let x,y = s.pos +$ shift in
          draw_rectangle (iof x - 5) (iof y - 5) 10 10 (Raylib.color_from_hsv s.col 1.0 1.0);
          draw_text (string_of_float s.col) (iof x) (iof y) 20 Color.raywhite;
        let f = bilan_des_forces s.pos s.vit c i l in
        let fac_newt = 0.1 in
        List.iter (fun ((fx,fy),col) -> 
           draw_line (iof x) (iof y) (iof (x+.fx*.fac_newt)) (iof (y+.fy*.fac_newt)) col) f) l; 
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

  let somme_des_forces i s  = List.fold_left (fun x (f,_) -> x +$ f ) zero (bilan_des_forces s.pos s.vit c i l) in
  let f dt = (*bilan à l'instant t+dt*)
      Array.mapi(fun  i p->
        let f = somme_des_forces i p in
        {
            pos = p.pos +$  (p.vit *$ dt);
            vit = p.vit +$  (f *$ (dt/.mass));
            col = p.col *. dt;
  })  
    in (* RK4 : *)
  let k1 = l *% dt in
  let k2 = (f (dt/.2.) ( (k1 *% 0.5 ))) *% dt in
  let k3 = (f (dt/.2.) ( (k2 *% 0.5))) *% dt in
  let k4 = (f  dt      ( k3)) *% dt in
  let l' = l +% (k1 *% (1./.6.)) +% (k2 *% (1./.3.)) +% (k3 *% (1./.3.)) +% (k4 *% (1./.6.)) in

  loop {t = t+1; l = l'; shift = shift'}

let setup () =
  Random.self_init (); 
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  {
      t = 0;
      l = Array.init n (fun i -> 
          {
              pos = (float_of_int (w/2 + 100*(i mod w_blob))),(float_of_int (h/2 + 100*i/h_blob));
              vit = 0.*.(Random.float 1.0 -. 0.5),0.*.(Random.float 1.0 -. 0.5);
              col = Random.float 50.
          });
      shift = zero;
  }

let () =  
  setup () |> loop 
