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
    (List.map (fun (x,y) ->  
          {
              pos = (foi (w/2) +. x *.rayon, foi (h/2) +. y*.rayon);
              vit = (0.0,0.0);
              mass = mass; 
          })
       (
        List.init (n) (fun i-> let theta = 2.0*.3.14*.(foi i)/.(foi n) in
          cos theta, sin theta)
        )
    )

let linked_to i = (*relié au suivant et au précédent*)
  [
    (i+1) mod n,d_eq;
    (n+i-1) mod n,d_eq;
  ]
