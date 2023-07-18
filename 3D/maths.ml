
type vec3 =
   { x: float;y:float;z:float}

type ray =
   {origin: vec3;dir: vec3}
 
 let print_vec v = Printf.printf "%f %f %f\n" v.x v.y v.z

 let ($+) a b =
     let xa,ya,za = a in
     let xb,yb,zb = b in
     xa+.xb, ya+.yb, za+.zb

 let ($*) m v =
 { x= v.x *. m;
  y=v.y *. m;
  z=v.z *. m}
 
 let ($-) a b =
  { x= a.x-.b.x;
  y=a.y-.b.y;
  z=a.z-.b.z}
(*
let at r t =
    r.origin $+ (t $* r.dir)
*)
 let dot a b =
     a.x*.b.x+.a.y*.b.y+.a.z*.b.z

(* 
 let () =
     let v = {x=1.0;y=1.1;z=1.1} in
     let v' = {x=1.5;y=2.1;z=0.1} in
     print_vec (v $- v');
     print_vec (at {origin = v; dir = v} 10.);
     print_float ( dot v v'*)
