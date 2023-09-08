open Raylib

let w = 500
let h = 500
let dt = 0.5
let k = 100.0
let mass = 1.0
type vec = float * float

type point = {
    pos : vec;
    vit : vec;
    col : float
}
let zero = (0.,0.)
let n = 10
let ($+) (a,b) (c,d) = ( a+.c , b+.d)
let (#*) (a,b) q = (a*.q,b*.q)

let y_of = snd
let setup () =
  Raylib.init_window w h "vichy";
  Raylib.set_target_fps 60;
  (List.init n (fun _ -> {pos = 250.,250.;vit = 100.*.(Random.float 1.0 -. 0.5),100.*.(Random.float 1.0 -. 0.5);col = Random.float 255.})), 0


let bilan_des_forces (x,y) (vx,vy) (cx,cy)=
   (0.0,1.0) (*poids*)  
   $+ (if y = 400. then -.vx,10.*.vy else zero) (* rebond sur sol *)
   $+ ((x -. cx,y -. cy) #* (-1.0*.0.01))   (*ressort avec le centre*)
   $+ ((-.vx,-.vy) #* 0.01) (*force de frottement *)

let rec loop (l,t) =
  if Raylib.window_should_close () then Raylib.close_window () else 
  clear_background Color.black;
  let c = (250.0 +. 200.0*.(cos((float_of_int t)/.10.0)), 250.0 +. 200.0*.(sin((float_of_int t)/.10.0))) in 
  begin_drawing ();
  let cx,cy = c in
  List.iter (fun s-> 
      let x,y = s.pos in
      draw_line (int_of_float cx) (int_of_float cy) (int_of_float x) (int_of_float y) Color.raywhite;
      draw_rectangle (int_of_float x - 5) (int_of_float y - 5) 10 10 (Raylib.color_from_hsv s.col 1.0 1.0)) l;
  end_drawing ();

  loop (List.map (fun s->
      let vx,vy = s.vit in
      let fx,fy = bilan_des_forces s.pos s.vit c in
      let x,y = s.pos in
      let dry,vy = if y +. vy *. dt > 400. then (400. -. y,  -0.1 *. vy) else vy *. dt, vy in
      
      { pos = 
          x +. vx *. dt,
          y +. dry;
       vit = 
          vx +. fx/.mass *. dt,
          vy +. fy/.mass *. dt;
        col = s.col
      }
    ) l ,(t+1))

let () =  
  setup () |> loop 
