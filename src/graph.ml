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
let ( +%) a b = try
  map2 (+$) a b
  with _ ->  failwith "fdivso"

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
       List.concat (List.init rings 
       (fun ring_no -> 
        List.init ring (fun i-> let theta = 2.0*.3.14*.(foi i)/.(foi ring) in 
          cos theta, sin theta, ring_no
          )
        ))
       )
    )
(*dans mon anneau, celui à ma gauche*)
let gauche i = i/ring * ring + (i+1) mod ring
(*dans mon anneau, celui à ma droite*)
let droite i = i/ring * ring + (i+ring-1) mod ring

(*relié au suivant et au précédent*)
let linked_to i = 
   let ind_anneau = i/ring in
   let d = 2.0*.(rayon +. foi ind_anneau *. interstice) *. sin (theta/.2.0)  in 
  [
    (*sur mon anneau*)
    (i/ring) * ring + ((i+1) mod ring), d;
    (i/ring) * ring + ((ring+i-1) mod ring),d;
  ]
 (*connecter les anneaux *)

  @ if i/ring+1 = rings then [] else  
    let c = (interstice**2.0 +. (d +. interstice*.sin (theta/.2.0))**2.0) |> sqrt  in
  [
    ring * (i/ring+1) + (i mod ring), interstice; 
    ring * (i/ring+1) + ((i+1) mod ring), c; 
    ring * (i/ring+1) + ((i+ring-1) mod ring), c; 
  ] 
  @ if i/ring = 0 then [] else  
    let c = (interstice**2.0 +. (d -. interstice*.sin (theta/.2.0))**2.0) |> sqrt  in
  [
    ring * (i/ring-1) + (i mod ring), interstice; 
    ring * (i/ring-1) + ((i+1) mod ring), c; 
    ring * (i/ring-1) + ((i+ring-1) mod ring), c; 
  ] 
