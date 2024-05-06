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

let r3_to_vec3 (x,y,z) = Raylib.Vector3.create x z y

let (+$) (a,b,c) (x,y,z) = (a+.x, b+.y, c+.z)
let (-$) (a,b,c) (x,y,z) = (a-.x, b-.y, c-.z)
let ( *$) (a,b,c) q = (a*.q, b*.q, c*.q)
let (/$) (a,b,c) q = (a/.q, b/.q, c/.q) 

let zero = 0.0,0.0,0.0
let zero_r = Raylib.Vector3.create 0.0 0.0 0.0

(*dot product, fin le produit scalaire quoi*)
let ps (a,b,c) (x,y,z) = a*.x +. b*.y +. c*.z
(*cross product*)
let cross (a,b,c) (x,y,z) = (b*.z -. c*.y, c*.x -. a*.z, a*.y -. b*.x)

let abs_f x = if x < 0.0 then -.x else x
let sign_f x = if x < 0.0 then -1. else 1.

let dist_square (xa,ya,za) (xb,yb,zb) = (xa-.xb)**2.0 +. (ya-.yb)**2.0 +. (za-.zb)**2.0
let dist a b = sqrt (dist_square a b)
let norme = dist zero

let vect_elem (xa,ya,za) (xb,yb,zb) =
    let d = dist (xa,ya,za) (xb,yb,zb) in
    ((xb-.xa)/.d,(yb-.ya)/.d,(zb-.za)/.d)
let shmidtz = vect_elem zero 

let det3 (a,b,c) (d,e,f) (g,h,i) =
  a *. (e*.i -. h*.f) -. d*. (b*.i -. h*.c) +. g*. (b*.f -. e*.c)
let rec order a b c center =
  (*on veut que a b c soit dans le sens horraire, depuis le centre center*)
  let a,b,c = a-$center, b-$center, c-$center in
  let det = det3 a b c in
  if det > 0.0 then a+$center,b+$center,c+$center
  else a+$center, c+$center, b+$center
let normal a b c center =
  let a,b,c = order a b c center in
  cross (b-$a) (c-$a)


let somme_forces l p =
  let s = List.fold_left (fun x (f,_) -> x +$ f ) zero l  in
  if trd p.pos < 0.1 then (fst s,snd s,0.0) else s

(*l'aire d'un triangle situé sur les sommets a b c*)
let area (a,b,c) = norme (cross (b-$a) (c-$a)) /. 2.0

(*On approxime le volume de l par le volume d'une ellipsoide*)
let volume l =
  (*WARN : On sait ici que le graphe est un tableau*)
  let l = Array.map (fun x-> x.pos) l in
  let minx,miny,minz = Array.fold_left 
    (fun (a,b,c) (x,y,z) -> (min a x, min b y, min c z)) (max_float,max_float,max_float) l in
  let maxx,maxy,maxz = Array.fold_left 
    (fun (a,b,c) (x,y,z) -> (max a x, max b y, max c z)) (min_float,min_float,min_float) l in
  4.0/.3.0 *. 3.14 *.  (maxx-.minx)*.(maxy-.miny)*.(maxz-.minz)
