
(*Screen*)
let w = 16*120
let h = 9*120
let dt = 1.0/.60.0 
let floor_y = float_of_int h (*sol*)

(*Constantes physiques*)
let mass = 50.0 (*masses des particules de referencei (kg)*)
let d_eq = 25.0 (*distance d'equilibre des ressorts (m)*)
let k_ressort = 2000.0 (*constante de raideur (N/m)*)
let damping = -150.0 (*damping du ressort*)
let k0 = 5e6 (*N.m6*)
(*Taille du blob carré*)
let w_blob = 10
let h_blob = 10
let n = w_blob * h_blob
