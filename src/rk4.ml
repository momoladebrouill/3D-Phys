open Maths
open Constantes
open Force
open Graph

exception Superposition of int

(*arguments de la fonction f*)
type args = {
  l : point array;
  k_ressort : float;
  penche : float
}

(*creation d'un tableau de points a partir d'un tableau de positions et de vitesses*)
let to_points y y' l =
  Graph.init n (fun i-> {pos=y.(i); vit = y'.(i); mass = l.(i).mass}) 

let fix_floor p = 
      {
        pos = fst p.pos,  floor_y -. Random.float 1.1;
        vit = zero;
        mass = p.mass
      }

(*fonction qui donne l'acceleration en fonction de dt, la position et la vitesse*)
let f _ y y' args = 
  let points = to_points y y' args.l in
  Graph.mapi (fun i p -> if snd p.pos > floor_y+.0.1 then raise (Superposition (i)) else
    somme_forces (bilan_des_forces p i points  args.k_ressort args.penche) *$ (1.0/.p.mass)) points 

let mult = ( *%)

let rec runge_kunta args iter = if 1=0 then args.l else  
  if iter > rk_tries then args.l
  else
  try 
      let h =  dt in
      let h2 = h/.2.0 in
      let h4 = h*.h/.4.0 in
      let h6 = h/.6.0 in
      let y' = Graph.map (fun x -> x.vit) args.l in
      let y = Graph.map (fun x -> x.pos) args.l in
      let k1 = f 0.0 y y' args in
      let k2 = f h2 (y +% mult h2 y' ) (y' +% mult h2 k1) args in
      let k3 = f h2 (y +% mult h2 y' +% mult h4 k1) (y' +% mult h2 k2) args  in
      let k4 = f h (y +% mult h y' +% mult (h*.h2) k2) (y' +% mult h k3)  args in
      let ny = y +% mult h y' +% mult (h*.h6) (k1 +% k2 +% k3)  in
      let ny' = y' +% mult h6 (k1 +% mult 2.0 k2 +% mult 2.0 k3 +% k4) in
      to_points ny ny' args.l
  with Superposition i ->
    args.l.(i) <- fix_floor args.l.(i);
    runge_kunta args (iter+1)
