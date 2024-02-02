open Raylib.Color
open Maths
open Constantes
open Graph
type force = point * Raylib.Color.t

(*force elastique avec les autres *)
let ressort a b l0 k = 
    vect_elem b.pos a.pos *$ (-.k*.(dist a.pos b.pos -.l0))

(*force de repulstion avec les voisins*)
let repultion a b  = 
    vect_elem b.pos a.pos *$ (k0/.(dist a.pos b.pos)**12.0)

(*force damped avec les autres *)
let amortisseur src dst d = 
    let er = vect_elem src.pos dst.pos in
    er *$ (d *. ps (src.vit -$ dst.vit) er)

let gaz src dst vol =
  (*thanks Maciej Matyka*)
  let n = normal src.pos dst.pos in
  n*$ 
    ((-1.0/.vol) *. (dist src.pos dst.pos) *. nrT)

let bilan_des_forces s i l k_ressort penche =
  let volume = 
    let maxx, maxy, minx, miny = Graph.fold_left 
      (fun (maxx,maxy,minx,miny) (x,y) -> max maxx x, max maxy y,min minx x, min miny y) 
        (fst l.(0).pos, snd l.(0).pos,fst l.(0).pos, snd l.(0).pos) 
        (Graph.map (fun x ->x.pos) l) in  ((maxy-.miny)*.(maxx-.minx))/.1000.0 (* on suppose que c'est un carré*)
  in
  let gaussian_volume =
    let v i src =  
        let dst = l.((i+1) mod n ) in
          0.5 
          *. abs_f (fst src.pos -. fst dst.pos) 
          *. abs_f (fst (normal src.pos dst.pos))
          *. dist src.pos dst.pos
  in
    Graph.fold_left (+.) 0.0 (Graph.mapi v l)
   in
   [
    (penche*.5.0,9.81) *$ (1.0*.s.mass), green; (*champs de pesanteur*) 
       gaz s l.((i+1) mod n) volume, pink;
       gaz l.((i+n-1) mod n) s volume, pink;
  ] @ List.concat 
    (List.map (fun (i,d) -> 
         [
            ressort s l.(i) (d_eq*.d) k_ressort, purple;
            amortisseur s l.(i) damping, raywhite;
            repultion s l.(i), yellow;
         ]) (Graph.linked_to i)) 
