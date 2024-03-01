open Raylib.Color
open Maths
open Constantes
type force = point * Raylib.Color.t

(*force elastique avec les autres *)
let ressort src dst l0 = vect_elem dst.pos src.pos *$ 
  (-.k_ressort*.(dist src.pos dst.pos -.l0))

(*force de repulstion avec les voisins*)
let repultion src dst  = vect_elem dst.pos src.pos *$ 
  (k_repultion/.(dist src.pos dst.pos)**4.0)

(*force damped avec les autres *)
let amortisseur src dst = 
    let er = vect_elem src.pos dst.pos in
    er *$ 
  (k_damping *. ps (src.vit -$ dst.vit) er)

let gaz n_anneau src dst vol = normal dst.pos src.pos *$ 
   ((1.0/.vol) *. (dist src.pos dst.pos) *. nRT *. (1.0 +. foi n_anneau *. p0)) 

let bilan_des_forces src i l penche =
  let volumes = Array.make rings 0.0 in
  let minx = Graph.fold_left min (fst (Graph.random l).pos ) (Graph.map (fun x -> fst x.pos) l) in
  let v i src =  
        let t = 
          let dst = Graph.droite l i  in
           (fst src.pos -. minx) 
          *. (fst (normal src.pos dst.pos))
          *. dist src.pos dst.pos
        in volumes.(i/ring)<-volumes.(i/ring)+.t
    in Graph.iteri v l;
 
   [
    (gravity +$ if penche then (5.0,0.0) else zero ) *$ (1.0*.src.mass), yellow; (*champs de pesanteur*) 
   ] @
   let volume = volumes.(i/ring) in
   [
       gaz (i/ring) src (Graph.gauche l i) volume, green;
       gaz (i/ring) (Graph.droite l i) src volume, green;
   ]
   @ List.concat 
    (List.map (fun (dst,d) -> 
         [
            ressort src dst d , blue;
            amortisseur src dst, raywhite;
            repultion src dst, red;
         ]) (Graph.linked_to l i)) 
