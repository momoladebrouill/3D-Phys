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
  let d_init = 0.9 *. d_eq in 
  Array.of_list 
    (List.map (fun (x,y) ->  
          {
              pos = (foi (w/2) +. foi x *.d_init, foi (h/2) +. foi y*.d_init);
              vit = (0.0,0.0);
              mass = mass; 
          })
       (
       List.concat [
        List.init (t_blob) (fun i-> i,0);
        List.init (t_blob-2) (fun i-> t_blob-1,i+1);
        List.init (t_blob) (fun i-> t_blob-i-1,t_blob-1); 
        List.init (t_blob-2) (fun i-> 0,t_blob-i-2)]  
        )
    )

let linked_to i = (*relié au suivant et au précédent*)
  [((i+1) mod n,1.0);((n+i-1) mod n,1.0)]
