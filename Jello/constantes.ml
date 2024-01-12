(*Nombre d'essais max pour Runge-Kutta*)
let rk_tries = 10

(*Screen*)
let w = 16*120
let h = 9*120
let dt = 1.0/.60.0 
let floor_y = float_of_int h (*sol, c'est un hasard que ça corresponde à la taille de l'écran*)

(*Constantes physiques*)
let mass = 50.0 (*masses des particules de referencei (kg)*)
let d_eq = 25.0 (*distance d'equilibre des ressorts (m)*)
let k_ressort = 1e4 (*constante de raideur (N/m)*)
let damping = -150.0 (*damping du ressort*)
let k0 = 5e6 (*N.m6*)
let j0 = 200.0 (*N.m-1*)
let gravity = (0.0,9.81) (*m.s-2*)
(*Taille du blob *)
let nrT = 9e2
let t_blob = 5
let n = 4*t_blob- 4  
