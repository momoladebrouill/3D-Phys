open Raylib.Color
open Maths
open Constantes

type force = point * Raylib.Color.t

(*force elastique avec les autres *)
let ressort a b l0 k = 
    vect_elem b.pos a.pos *$ (-.k*.(dist a.pos b.pos -.l0))

(*force de repulstion avec les voisins*)
let repultion a b  = 
    vect_elem b.pos a.pos *$ (k0/.(dist a.pos b.pos)**6.0)

(*force damped avec les autres *)
let amortisseur src dst d = 
    let er = vect_elem src.pos dst.pos in
    er *$ (d *. ps (src.vit -$ dst.vit) er)

let retour s midpoint k_ressort= 
  vect_elem midpoint s.pos *$  (dist s.pos midpoint *. (k_ressort/.j0))

let bilan_des_forces s i l t  k_ressort penche =
  let midpos = Array.fold_left (+$) zero (Array.map (fun x ->x.pos) l) *$ (1.0/.(foi n)) in  
   [
    (penche*.5.0,9.81) *$ s.mass, green; (*champs de pesanteur*) 
  ] @ List.concat 
    (List.map (fun (i,d) -> 
         [
            ressort s l.(i) (d_eq*.d) k_ressort, purple;
            amortisseur s l.(i) damping, raywhite;
            repultion s l.(i), pink;
            retour s midpos k_ressort, yellow;
         ]) (linked_to i)) 
