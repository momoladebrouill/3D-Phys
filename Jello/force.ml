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

let repultion s dt acc  =
  if (snd s.pos) > floor_y then (0.0, -.abs_f (snd s.vit) *. 100.5)
    (*
      0.0,
      () -. abs_f (snd acc)
    *)
  else zero
(*
  let poses = List.map (fun x-> x.pos) (Array.to_list l) in
  let l = ::poses else poses in 
  List.fold_left (+$) zero 
    (List.map (fun e -> if dist_square e s.pos > 0.0 then rep e s.pos else zero) l)
*)
let bilan_des_forces s i l t k_ressort =
  let forces = [
    (0.0,9.81) *$ s.mass, green; (*champs de pesanteur*) 
  ] @ List.concat 
    (List.map (fun (i,d) -> 
         [
            ressort s l.(i) (d_eq*.d) k_ressort, purple;
            amortisseur s l.(i) damping, raywhite
         ]) (linked_to i)) 
  in (repultion s t ((somme_forces forces) *$ (1.0/.s.mass)), red)::forces
