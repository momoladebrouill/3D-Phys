(*Nombre d'essais max pour Runge-Kutta*)
let rk_tries = 10

(*Screen*)
let w = 16*120
let h = 9*120
let dt = 1.0/.60.0 
let floor_y = float_of_int h (*sol, c'est un hasard que ça corresponde à la taille de l'écran*)

(*Constantes physiques*)
let mass = 50.0 (*masses des particules de referencei (kg)*)
let k_ressort = 1e3 (*constante de raideur (N/m)*)
let damping = -150.0 (*damping du ressort*)
let k_rep = 10.0 
let gravity = (0.0,9.81) (*m.s-2*)
(*Taille du blob *)
let nrT = 9e3
let rayon = 50.0
let rings = 3
let ring = 20
let n = rings * ring
let d_eq = 2.0 *. 3.14/. (float_of_int ring)(*distance d'equilibre des ressorts (m)*)
let d_eq_rayon = rayon/.(float_of_int rings)
let animate = true
