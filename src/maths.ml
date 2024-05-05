let foi = float_of_int
let iof = int_of_float
type vec = float * float * float
type point = {
    pos : vec;
    vit : vec;
    mass : float;
}
let fst (a,_,_) = a
let snd (_,b,_) = b
let trd (_,_,c) = c

let r3_to_vec3 pos =
    let x,y,z = pos in
    Raylib.Vector3.create x z y

let (+$) (a,b,c) (x,y,z) = (a+.x, b+.y, c+.z)
let (-$) (a,b,c) (x,y,z) = (a-.x, b-.y, c-.z)
let zero = 0.0,0.0,0.0
let zero_r = Raylib.Vector3.create 0.0 0.0 0.0
let ( *$) (a,b,c) q = (a*.q, b*.q, c*.q)
let ( /$) (a,b,c) q = (a/.q, b/.q, c/.q) 
let ps (a,b,c) (x,y,z) = a*.x +. b*.y +. c*.z

let somme_forces l = List.fold_left (fun x (f,_) -> x +$ f ) zero l  

let abs_f x = if x < 0.0 then -.x else x
let sign_f x = if x < 0.0 then -1. else 1.

let dist_square (xa,ya,za) (xb,yb,zb) = (xa-.xb)**2.0 +. (ya-.yb)**2.0 +. (za-.zb)**2.0
let dist a b = sqrt (dist_square a b)
let norme = dist zero


let vect_elem (xa,ya,za) (xb,yb,zb) =
    let d = dist (xa,ya,za) (xb,yb,zb) in
    ((xb-.xa)/.d,(yb-.ya)/.d,(zb-.za)/.d)

let shmidtz = vect_elem zero 

let normal (xa,ya,za) (xb,yb,zb) = (*calcul du vecteur unitaire normal à la droite reliant les points a et b*)
(* actually, on calcule le produit vectoriel avec un vecteur de la troisème dimention omg
  let (x,y) = vect_elem (xa,ya) (xb,yb) in 
  let (x0,y0,_) = cross (0.,0.,1.) (x,y,0.) in
  vect_elem zero (x0,y0)*)
  let d = dist (xa,ya,za) (xb,yb,zb) in
    ((yb-.ya)/.d,(xb-.xa)/.d,(za-.zb)/.d)
  (*if abs_f (yb -.ya) < 1.0 then (0.0,sign_f (ya-.yb)) 
  else 
  let c = (xb-.xa) /. (yb -. ya)
  in vect_elem zero (sign_f (ya-.yb),c)*)
