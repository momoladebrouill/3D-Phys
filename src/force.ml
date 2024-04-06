open Maths
open Raylib.Color
open Constantes
type force = point * Raylib.Color.t

(*arguments de la fonction f*)
type args = {
  l : point array;
  k_ressort : float;
  penche : bool
}

(*force elastique avec les autres *)
let ressort src dst l0 k_ressort = vect_elem dst.pos src.pos *$ 
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

let bilan_des_forces src i l {penche;k_ressort} =  
  let maxix, miny = Graph.fold_left (fun (i,a) x -> min x i, max x a) (fst (Graph.random l).pos, fst (Graph.random l).pos) (Graph.map (fun x -> fst x.pos) l) in
  let volumes = Array.make rings (maxix -. miny) in
   [
    (gravity +$ if penche then (5.0,0.0,0.0) else zero ) *$ (1.0*.src.mass), yellow; (*champs de pesanteur*) 
   ] @
   let volume = volumes.(i/ring) in
   [
       gaz (i/ring) src (Graph.gauche l i) volume, green;
       gaz (i/ring) (Graph.droite l i) src volume, green;
   ]
   @ List.concat 
    (List.map (fun (dst,d) -> 
         [
            ressort src dst d k_ressort, blue;
            amortisseur src dst, raywhite;
            repultion src dst, red;
         ]) (Graph.linked_to l i)) 
         
