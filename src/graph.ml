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
    (List.map (fun (x,y,mult) ->  
          let r = rayon +. foi mult *. d_eq_rayon in
          {
              pos = foi (w/2) +. x *. r, foi (h/2) +. y*.r;
              vit = 0.0,0.0;
              mass = mass; 
          })
       (
       List.concat (List.init rings 
       (fun ring_no -> 
        List.init ring (fun i-> 
          let theta = 2.0*.3.14*.(foi i)/.(foi ring) in cos theta, sin theta, ring_no)
        ))
       )
    )

let linked_to i = (*relié au suivant et au précédent*)
  [
    (*sur mon anneau*)
    (i/ring) * ring + ((i+1) mod ring),d_eq;
    (i/ring) * ring + ((ring+i-1) mod ring),d_eq;
  ]
 (*connecter les anneaux*)
  @ if i/ring+1 = rings then [] else  
  [
    ring * (i/ring+1) + (i mod ring), d_eq_rayon; 
    ring * (i/ring+1) + ((i+1) mod ring), d_eq_rayon; 
    ring * (i/ring+1) + ((i+ring-1) mod ring), d_eq_rayon; 
  ] 
  @ if i/ring = 0 then [] else  
  [
    ring * (i/ring-1) + (i mod ring), d_eq_rayon; 
    ring * (i/ring-1) + ((i+1) mod ring), d_eq_rayon; 
    ring * (i/ring-1) + ((i+ring-1) mod ring), d_eq_rayon; 
  ] 
