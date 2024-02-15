open Constantes
open Maths

type 'a t = 'a Array.t
let fold_left = Array.fold_left
let mapi = Array.mapi
let map = Array.map
let map2 = Array.map2
let init = Array.init
let iteri = Array.iteri

let ( *%) q = map (fun t-> t *$ q)
let ( +%) = map2 (+$) 

(* le graphe initial*)
let initial () = 
  Array.of_list 
    (List.map (fun (x,y,ind_anneau) ->  
          let r = rayon +. foi ind_anneau *. interstice in
          {
              pos = foi (w/2) +. x *. r,  y*.r;
              vit = 0.0,0.0;
              mass = mass; 
          })
       (
       List.concat 
        (List.init rings 
          (fun ring_no -> List.init ring 
            (fun i-> let theta = 2.0*.3.14*.(foi i)/.(foi ring) in cos theta, sin theta, ring_no)
          )
        )
       )
      )

(*dans mon anneau, celui à ma gauche*)
let gauche i = i/ring * ring + (i+1) mod ring

(*dans mon anneau, celui à ma droite*)
let droite i = i/ring * ring + (i+ring-1) mod ring

(*renvoie la liste des indices des voisins*)
let linked_to i = 
   let ind_anneau = i/ring in
   let d = 2.0*.(rayon +. foi ind_anneau *. interstice) *. sin (theta/.2.0)  in 
  [
    (*sur mon anneau*)
    gauche i, d;
    droite i,d;
  ]
 (*  connecter les anneaux   *)
  (*celui autour*)
  @ if ind_anneau+1 = rings then [] else  
    let c = (interstice**2.0 +. (d +. interstice*.sin (theta/.2.0))**2.0) |> sqrt  in
  [
    ring * (ind_anneau+1) + (i mod ring), interstice; 
    ring * (ind_anneau+1) + ((i+1) mod ring), c; 
    ring * (ind_anneau+1) + ((i+ring-1) mod ring), c; 
  ] 
  (*celui à l'intérieur*)
  @ if ind_anneau = 0 then [] else  
    let c = (interstice**2.0 +. (d -. interstice*.sin (theta/.2.0))**2.0) |> sqrt  in
  [
    ring * (ind_anneau-1) + (i mod ring), interstice; 
    ring * (ind_anneau-1) + ((i+1) mod ring), c; 
    ring * (ind_anneau-1) + ((i+ring-1) mod ring), c; 
  ] 
