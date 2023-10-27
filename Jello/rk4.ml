open Maths
open Constantes
open Force

type args = {l : point array; k_ressort : float}

let to_points y y' l =
  Array.init n (fun i-> {pos=y.(i); vit = y'.(i); mass = l.(i).mass}) 

(*fonction qui donne l'acceleration en fonction de dt, la position et la vitesse*)
let f h y y' args = 
  let points = to_points y y' args.l in
  Array.mapi (fun i p -> somme_forces (bilan_des_forces p i points h args.k_ressort) *$ (1.0/.p.mass)) points 

let mult = ( *%)

let runge_kunta args = 
  let h =  dt in
  let h2 = h/.2.0 in
  let h4 = h*.h/.4.0 in
  let h6 = h/.6.0 in
  let y' = Array.map (fun x -> x.vit) args.l in
  let y = Array.map (fun x -> x.pos) args.l in
  let k1 = f 0.0 y y' args in
  let k2 = f h2 (y +% mult h2 y' ) (y' +% mult h2 k1) args in
  let k3 = f h2 (y +% mult h2 y' +% mult h4 k1) (y' +% mult h2 k2) args  in
  let k4 = f h (y +% mult h y' +% mult (h*.h2) k2) (y' +% mult h k3)  args in
  let ny = y +% mult h y' +% mult (h*.h6) (k1 +% k2 +% k3)  in
  let ny' = y' +% mult h6 (k1 +% mult 2.0 k2 +% mult 2.0 k3 +% k4) in
  to_points ny ny' args.l
