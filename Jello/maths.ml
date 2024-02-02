open Constantes

let foi = float_of_int
let iof = int_of_float
type vec = float * float
type point = {
    pos : vec;
    vit : vec;
    mass : float;
}
let foi = float_of_int
let iof = int_of_float



let (+$) (a,b) (c,d) = (a+.c, b+.d)
let (-$) (a,b) (c,d) = (a-.c, b-.d)
let zero = 0.0,0.0
let ( *$) (a,b) q = (a*.q, b*.q)
let ps (a,b) (c,d) = a*.c +. b*.d  

let somme_forces l = List.fold_left (fun x (f,_) -> x +$ f ) zero l  

let abs_f x = if x < 0.0 then -.x else x
let sign_f x = if x <0.0 then -1. else 1.

let dist_square (xa,ya) (xb,yb) = (xa-.xb)**2.0 +. (ya-.yb)**2.0
let dist a b = sqrt (dist_square a b)

let vect_elem (xa,ya) (xb,yb) =
    let d = dist (xa,ya) (xb,yb) in
    ((xb-.xa)/.d,(yb-.ya)/.d)

let cross (a1,a2,a3) (b1,b2,b3) =
  (
    a2 *. b3 -. a3 *. b2,
    a3 *. b1 -. a1 *. b3,
    a1 *. b2 -. a2 *. b1
   )

let normal (xa,ya) (xb,yb) = (*calcul du vecteur unitaire normal à la droite reliant les points a et b*)
(* actually, on calcule le produit vectoriel avec un vecteur de la troisème dimention omg
  let (x,y) = vect_elem (xa,ya) (xb,yb) in 
  let (x0,y0,_) = cross (0.,0.,1.) (x,y,0.) in
  vect_elem zero (x0,y0)*)
  let d = dist (xa,ya) (xb,yb) in
    ((ya-.yb)/.d,(xb-.xa)/.d)
  (*if abs_f (yb -.ya) < 1.0 then (0.0,sign_f (ya-.yb)) 
  else 
  let c = (xb-.xa) /. (yb -. ya)
  in vect_elem zero (sign_f (ya-.yb),c)*)
