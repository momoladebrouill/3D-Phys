open Constantes

let foi = float_of_int
let iof = int_of_float

type vec = float * float

type point = {
    pos : vec;
    vit : vec;
    mass : float;
}

let (+$) (a,b) (c,d) = (a+.c, b+.d)
let (-$) (a,b) (c,d) = (a-.c, b-.d)
let zero = 0.0,0.0
let ( *$) (a,b) q = (a*.q, b*.q)
let ps (a,b) (c,d) = a*.c +. b*.d  
let (+%) a b = try
  Array.map2 (+$) a b
  with _ -> Printf.printf "%d %d\n" (Array.length a) (Array.length b); failwith "fdivso"
let ( *%) q = Array.map (fun t-> t *$ q)

let somme_forces l = List.fold_left (fun x (f,_) -> x +$ f ) zero l  

let abs_f x = if x < 0.0 then -.x else x

let dist_square (xa,ya) (xb,yb) = (xa-.xb)**2.0 +. (ya-.yb)**2.0
let dist a b = sqrt (dist_square a b)

let vect_elem (xa,ya) (xb,yb) =
    let d = dist (xa,ya) (xb,yb) in
    ((xb-.xa)/.d,(yb-.ya)/.d)

let linked_to i = (*to make a square*)
  if true  then [((i+1) mod n,1.0)] else []
