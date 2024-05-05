open Constantes
open Maths

type listadj = int list array

type 'a t = 'a Array.t
let fold_left = Array.fold_left
let mapi = Array.mapi
let map = Array.map
let map2 = Array.map2
let init = Array.init
let iteri = Array.iteri

let ( *%) q = map (fun t-> t *$ q)
let ( +%) = map2 (+$) 
let ( -%) = map2 (-$) 

(* le graphe initial*)
let initial () =  
    Array.map (fun ((x,y,z),ind_anneau) ->  
          let r = rayon +. foi ind_anneau *. interstice in
          {
              pos = x*.r,y*.r,z*.r +.rayon +. interstice*.(foi rings);
              vit = 0.0,0.0,0.0;
              mass = mass; 
          })
       (
       Array.concat 
        (List.init rings 
          (fun ring_no -> Array.map (fun a-> a,ring_no) Icosphere.icosphere)
          )
        )
       

let random l = l.(0)



(*renvoie la liste des indices des voisins*)
let g =
  let l = initial () in
  let g = Array.make (Array.length Icosphere.icosphere) [] in
  List.iter (fun (i,j,k) -> 
    g.(i) <- j::g.(i); g.(i) <- k::g.(i);
    g.(j) <- i::g.(j); g.(j) <- k::g.(j);
    g.(k) <- i::g.(k); g.(k) <- j::g.(k)
    ) 
    Icosphere.indices_triangles;
  let g = Array.map Icosphere.uniq g in
  Array.mapi (fun i l' -> 
    List.map (fun j -> j,dist l.(i).pos l.(j).pos) l') g

let linked_to l i =   
  List.map (fun (j,d) -> l.(j), d) g.(i)

let surfaces l = 
  List.map (fun (i,j,k) -> l.(i),l.(j),l.(k)) Icosphere.indices_triangles
