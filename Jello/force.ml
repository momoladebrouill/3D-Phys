open Raylib.Color
open Maths
open Constantes

(*force elastique avec les autres *)
let ressort a b l0 k = 
    vect_elem b.pos a.pos *$ (-.k*.(dist a.pos b.pos -.l0))

(*force damped avec les autres *)
let amortisseur src dst d = 
    let er = vect_elem src.pos dst.pos in
    er *$ (d *. ps (src.vit -$ dst.vit) er)

(*force pour éviter qu'ils se touchent*)
let collision src dst =
    let er = vect_elem src.pos dst.pos in
    er *$ ( 50.0/.(dist_square src.pos dst.pos)) (* q1*q2 / 4 pi epsilon0 d²*)  

let somme_vecs = List.fold_left (fun x (f,_) -> x +$ f ) zero  

let bilan_des_forces s i l t k_ressort =
    let l = 
      [
              (0.0,9.81) *$ s.mass, green; (*champs de pesanteur*)  
  ] @ List.concat (List.map (fun (i,d) -> 
      [
              ressort s l.(i) (d_eq*.d) k_ressort, purple;
              amortisseur s l.(i) (-15.0), raywhite; 
              collision s l.(i), orange  
      ]) (linked_to i)) 
    in if snd (s.pos +$ (s.vit  *$ t)) > floor_y  then
         let f = somme_vecs l in (*RN*)
          ((0.0, -.5.0*.(abs_f (snd f))), red)::l 
  else l  
