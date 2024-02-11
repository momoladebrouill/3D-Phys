
(*Screen*)
let w = 16*120
let h = 9*120
let dt = 1.0/.40.0 
let floor_y = float_of_int h (*sol*)

(*Constantes physiques*)
let mass = 25.0 (*masses des particules de referencei (kg)*)
let d_eq = 25.0 (*distance d'equilibre des ressorts (m)*)
let k_ressort = 2000.0 (*constante de raideur (N/m)*)
let damping = -150.0 (*damping du ressort*)
let k0 = 5e6 (*N.m6*)
let j0 = 200.0 (*N.m-1*)
let gravity = (1.0,9.81) (*m.s-2*)
(*Taille du blob carré*)
let w_blob = 8  
let h_blob = 9
let n = w_blob * h_blob
