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

let bilan_des_forces s i l penche =
  let volumes = Array.make rings 0.0 in
  let minx = Graph.fold_left min (fst l.(0).pos) (Graph.map (fun x -> fst x.pos) l) in
  let v i src =  
        let t = 
        let dst = l.(Graph.droite i) in
           (fst src.pos -. minx) 
          *. (fst (normal src.pos dst.pos))
          *. dist src.pos dst.pos
        in volumes.(i/ring)<-volumes.(i/ring)+.t
    in Graph.iteri v l;
 
   [
    (penche*.5.0,9.81) *$ (1.0*.s.mass), yellow; (*champs de pesanteur*) 
   ] @
   let volume = volumes.(i/ring) in
   [
       gaz (i/ring) s l.(Graph.gauche i) volume, green;
       gaz (i/ring) l.(Graph.droite i) s volume, green;
   ]
   @ List.concat 
    (List.map (fun (i,d) -> 
         [
            ressort s l.(i) d , blue;
            amortisseur s l.(i), raywhite;
            repultion s l.(i), red;
         ]) (Graph.linked_to i)) 
