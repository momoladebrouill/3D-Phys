open Maths
open Raylib.Color
open Constantes
type force = point * Raylib.Color.t

(*arguments de la fonction f*)
type args = {
  l : point array;
  k_ressort : float;
  penche : bool;
  center : vec;
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

let gaz id vol l center= 
    List.fold_left (+$) zero 
      (List.map (fun (surface,norm) -> shmidtz norm*$ ((1.0/.vol) *. nRT *. surface)) 
        (Graph.triangles_with id l center))

let bilan_des_forces src i v l {penche;k_ressort;center} =  
   [
    (gravity +$ if penche then (5.0,0.0,0.0) else zero ) *$ (1.0*.src.mass), yellow; (*champs de pesanteur*) 
    gaz i v l center, green (*pression du gaz*)
   ]
   @ List.concat 
    (List.map (fun (dst,d) -> 
         [
            ressort src dst d k_ressort, Raylib.fade blue 0.4;
            amortisseur src dst, raywhite;
            repultion src dst, red;
         ]) (Graph.linked_to l i)) 
         
