open Maths
open Constantes
open Graph
open Force
exception Superposition of int


(*creation d'un tableau de points a partir d'un tableau de positions et de vitesses*)
let to_points y y' l =
  Graph.init n (fun i-> {pos=y.(i); vit = y'.(i); mass = l.(i).mass}) 

let fix_floor p = 
      {
        pos = fst p.pos,snd p.pos,  Random.float 1.1;
        vit = zero;
        mass = p.mass
      }

(*fonction qui donne l'acceleration en fonction de dt, la position et la vitesse*)
let f _ y y' args = 
  let points = to_points y y' args.l in
  Graph.mapi (fun i p -> if trd p.pos < 0.0  then raise (Superposition (i)) else
    somme_forces (bilan_des_forces p i points args) *$ (1.0/.p.mass)) points 

let mult = ( *%)

let rec runge_kunta args iter =   
  if iter = 0  then args.l
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
    runge_kunta args (iter-1)

let rec verlet args =
      let y' = Graph.map (fun x -> x.vit) args.l in
      let prec = Graph.map (fun x -> x.pos) args.l in
      let current = prec +%  (dt *% y') in 
      try 
        let next = 2.0 *% current -% prec +% (dt*.dt *% f dt current y' args) in
        to_points next ((1.0 /.dt) *% (next -% current)) args.l
      with Superposition i ->
        args.l.(i) <- fix_floor args.l.(i);
        verlet args

let rec euler dt args =
  let y' = Graph.map (fun x -> x.vit) args.l in
  let y = Graph.map (fun x -> x.pos) args.l in
  try 
    let next = y +% (dt *% y') in
    let next' = y' +% (dt *% f dt y y' args) in
    to_points next next' args.l
  with Superposition i ->
    args.l.(i) <- fix_floor args.l.(i);
    euler dt args

let eulern tries args =
  let rec aux n args =
    if n = 0 then args else
    aux (n-1) {args with l = (euler (dt /. (foi tries)) args)}
  in (aux tries args).l

let integrate args =
  if not animate then args.l else
  match meth with
  | "rk" -> runge_kunta args tries
  | "euler" -> eulern tries args
  | "verlet" -> verlet args
  | _ -> failwith "methode inconnue"
