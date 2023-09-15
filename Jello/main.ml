open Raylib
let k = 120
let w = 16*k
let h = 9*k
let floor_y = float_of_int (h-400)


let dt = 0.1

let d_res = 20.0

let mass = 10.0

let n = 20 (*nombre de particules*)

type vec = float * float

type point = {
    pos : vec;
    vit : vec;
    col : float;
}

let zero = (0.,0.)
let ($+) (a,b) (c,d) = ( a+.c , b+.d)
let (#*) (a,b) q = (a*.q,b*.q)
let foi = float_of_int
let setup () =
  Random.self_init (); 
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  let r = 250. in 
  (Array.init n (fun i -> 
       {
         pos = (float_of_int (w/2) +. r *. cos(foi i /. foi n *. 6.28)),(float_of_int (h/2)+. r *.  sin (foi i/. foi n *. 6.28));
         vit = 50.*.(Random.float 1.0 -. 0.5),50.*.(Random.float 1.0 -. 0.5);
         col = Random.float 50.
     })), 0


let next  i = (i+1) mod n
let prev  i = (n+i-1) mod n
let dist (xa,xb) (ya,yb) = sqrt ( (xa -. xb)**2.0 +. (ya-.yb)**2.0 )

let f_ressort (xa,ya) (xb,yb) l0 k =
   let l = dist (xa,xb) (ya,yb) in
   let theta = atan2 (ya -. yb) (xa-.xb) in
   let el = (cos theta,sin theta) 
   in el #* (k*.(l-.l0))
        
let bilan_des_forces (x,y) (vx,vy) (cx,cy) i l=
  [
    (0.0,0.0), Color.green; (*champs de pesanteur*)  
    (vx,vy) #* (-1.0*.0.9), Color.blue; (*force de frottement sur surface*)
    f_ressort (cx,cy) (x,y)  50.0 0.01, Color.yellow; (*force elsatique avec le centre*)
    f_ressort l.(next i).pos (x,y) 10.0 0.05, Color.purple; (*force elastique avec le n+1 *)
    f_ressort l.(prev i).pos (x,y) 10.0 0.05, Color.purple (*force elastique avec le n-1 *)
  ] 
     
let rec loop (l,t) =
  if Raylib.window_should_close () then Raylib.close_window () else 
  clear_background Color.black;
  let c = ((float_of_int (w/2) +. 200.0*.cos ((float_of_int t)/.100.0) ), (float_of_int (h/2)) +. 200.0*.sin ((float_of_int t)/.100.0) ) in 
  let c = (float_of_int (w/2) , float_of_int (h/2)) in 
  begin_drawing ();
  let cx,cy = c in
    draw_circle (int_of_float cx) (int_of_float cy) 5.0 Color.white;
    draw_rectangle 0 (int_of_float floor_y) w (h-(int_of_float floor_y)) Color.white; 
    Array.iteri (fun i s ->
      let x,y = s.pos in
      draw_line (int_of_float cx) (int_of_float cy) (int_of_float x) (int_of_float y) Color.raywhite;
      draw_line (int_of_float (fst l.(next i).pos)) (int_of_float (snd (l.(next i).pos))) (int_of_float x) (int_of_float y) Color.gray;
      draw_rectangle (int_of_float x - 5) (int_of_float y - 5) 10 10 (Raylib.color_from_hsv s.col 1.0 1.0);
      
      let f = bilan_des_forces s.pos s.vit c i l in
      let fac_newt = 10.0 in
      List.iter (fun ((fx,fy),col) -> 
           draw_line (int_of_float x) (int_of_float y) (int_of_float (x+.fx*.fac_newt)) (int_of_float (y+.fy*.fac_newt)) col) f) l; 
  end_drawing ();

  loop (Array.mapi (fun  i s->
      let vx,vy = s.vit in
      let x,y = s.pos in
      let dry,vy = if y +. vy *. dt > floor_y then (floor_y -. y,  -0.1 *. vy) else vy *. dt, vy in
      let (fx,fy) = List.fold_left (fun x (f,_) -> x $+ f ) zero (bilan_des_forces s.pos s.vit c i l) in
      { pos = 
          x +. vx *. dt,
          y +. dry;
       vit = 
          vx +. fx/.mass *. dt,
          vy +. fy/.mass *. dt;
       col = s.col;
      }
    ) l ,(t+1))

let () =  
  setup () |> loop 
