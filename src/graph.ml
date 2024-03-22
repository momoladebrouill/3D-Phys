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
let ( -%) = map2 (-$) 

(* le graphe initial*)
let initial () = 
  Array.of_list 
    (List.map (fun (x,y,ind_anneau) ->  
          let r = rayon +. foi ind_anneau *. interstice in
          {
              pos = x*.r,x*.r,y*.r +. 2.0*.r;
              vit = 0.0,0.0,0.0;
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

let random l = l.(0)

(*dans mon anneau, celui à ma gauche*)
let gauche_formule i = i/ring * ring + (i+1) mod ring
let gauche l i = l.(gauche_formule i)

(*dans mon anneau, celui à ma droite*)
let droite_formule i = i/ring * ring + (i+ring-1) mod ring
let droite l i = l.(droite_formule i)

(*dans mon anneau, celui en face*)

(*renvoie la liste des indices des voisins*)
let linked_to l i =
    List.map (fun (i,d) -> l.(i),d) begin
   let ind_anneau = i/ring in
   let d = 2.0*.(rayon +. foi ind_anneau *. interstice) *. sin (theta/.2.0)  in 
  [
    (*sur mon anneau*)
    gauche_formule i, d;
    droite_formule i,d;
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
  ] end
