open Raylib.Color
open Maths
open Constantes

type force = point * Raylib.Color.t

(*force elastique avec les autres *)
let ressort a b l0 k = 
    vect_elem b.pos a.pos *$ (-.k*.(dist a.pos b.pos -.l0))

(*force damped avec les autres *)
let amortisseur src dst d = 
    let er = vect_elem src.pos dst.pos in
    er *$ (d *. ps (src.vit -$ dst.vit) er)

(* TODO : force de repultion entre les particules et avec le sol, en utilisant une force créee sur mesure lors de la collisition https://www.youtube.com/watch?v=9IULfQH7E90*)

(*force pour éviter qu'ils se touchent*)
let rep src dst =
    let er = vect_elem src dst in
    er *$ ( 1.0/.((dist_square src dst)**(2.0)))(*q1*q2 / 4 pi epsilon0 d²*)  

let repultion s l = 
  let poses = List.map (fun x-> x.pos) (Array.to_list l) in
  let l = if (snd s.pos) > floor_y then (s.pos +$ (0.0,(snd s.pos) -. floor_y))::poses else poses in 
  List.fold_left (+$) zero 
    (List.map (fun e -> if dist_square e s.pos > 0.0 then rep e s.pos else zero) l)

let bilan_des_forces s i l t k_ressort =
  [
    (0.0,9.81) *$ s.mass, green; (*champs de pesanteur*) 
    repultion s l, red
  ] @ List.concat 
    (List.map (fun (i,d) -> 
         [
            ressort s l.(i) (d_eq*.d) k_ressort, purple;
            amortisseur s l.(i) (damping), raywhite
         ]) (linked_to i)) 
