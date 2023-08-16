open Raylib

let w = 500
let h = 500
let dt = 0.5
let k = 100.0
let mass = 1.0
type vec = float * float

type point = {
    pos : vec;
    vit : vec
}

let ($+) (a,b) (c,d) = ( a+.c , b+.d)
let (#*) (a,b) q = (a*.q,b*.q)
let y_of = snd
let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  { pos = 250.,250.; vit = 0.,0.} 


let bilan_des_forces (x,y) (vx,vy)=
   (0.0,1.0) (*poids*)  

let rec loop s =
  if Raylib.window_should_close () then Raylib.close_window () else 
    begin_drawing ();
  clear_background Color.black;
  let x,y = s.pos in
  draw_rectangle (int_of_float x) (int_of_float y) 10 10 Color.raywhite;
  end_drawing ();
  let vx,vy = s.vit in
  let fx,fy = bilan_des_forces s.pos s.vit in
  let x,y = s.pos in
  let dry,vy = if y +. vy *. dt > 400. then (400. -. y,  -0.1 *. vy) else vy *. dt, vy in
  loop { pos = 
      x +. vx *. dt,
      y +. dry;
   vit = 
      vx +. fx/.mass *. dt,
      vy +. fy/.mass *. dt
  }

let () =  
  setup () |> loop 
