open Maths
open Constantes
open Graph
open Icosphere
open Types
exception Superposition of int


(*creation d'un tableau de points a partir d'un tableau de positions et de vitesses*)
let to_points y y' blob =
  Graph.init n (fun i-> {pos=y.(i); vit = y'.(i); mass = blob.(i).mass}) 

(*fonction qui donne l'acceleration en fonction de dt, la position et la vitesse*)
let f _ y y' args = 
  let blob = to_points y y' args.points in
  Graph.mapi (fun i p -> 
    if collision p.pos  then 
      begin
        y.(i) <- fix_collision p.pos;
        y'.(i) <-zero (*fst p.vit, snd p.vit, -. trd p.vit *. c0*)
      end;
    (somme_forces (Force.bilan_des_forces p i (volume args.points) blob args) p ) /$ p.mass  ) blob

let mult = ( *%)

let runge_kunta args =   
    let h =  dt in
    let h2 = h/.2.0 in
    let h4 = h*.h/.4.0 in
    let h6 = h/.6.0 in
    let y' = Graph.map (fun x -> x.vit) args.points in
    let y = Graph.map (fun x -> x.pos) args.points in
    let k1 = f 0.0 y y' args in
    let k2 = f h2 (y +% mult h2 y' ) (y' +% mult h2 k1) args in
    let k3 = f h2 (y +% mult h2 y' +% mult h4 k1) (y' +% mult h2 k2) args  in
    let k4 = f h (y +% mult h y' +% mult (h*.h2) k2) (y' +% mult h k3)  args in
    let ny = y +% mult h y' +% mult (h*.h6) (k1 +% k2 +% k3)  in
    let ny' = y' +% mult h6 (k1 +% mult 2.0 k2 +% mult 2.0 k3 +% k4) in
    to_points ny ny' args.points

let verlet args =
    let y' = Graph.map (fun x -> x.vit) args.points in
    let prec = Graph.map (fun x -> x.pos) args.points in
    let current = prec +%  (dt *% y') in 
    let next = 2.0 *% current -% prec +% (dt*.dt *% f dt current y' args) in
    to_points next ((1.0 /.dt) *% (next -% current)) args.points

let euler dt args =
    let y' = Graph.map (fun x -> x.vit) args.points in
    let y = Graph.map (fun x -> x.pos) args.points in
    let next = y +% (dt *% y') in
    let next' = y' +% (dt *% f dt y y' args) in
    to_points next next' args.points

let eulern tries args =
  let rec aux n args =
    if n = 0 then args else
    aux (n-1) {args with points = (euler (dt /. (foi tries)) args)}
  in (aux tries args).points

let integrate args =
  if not animate then args.points else
  match meth with
  | "rk" -> runge_kunta args
  | "euler" -> eulern tries args
  | "verlet" -> verlet args
  | _ -> failwith "methode inconnue"
