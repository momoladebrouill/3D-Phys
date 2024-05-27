(* --- Affichage --- *)
let fac_newt = 1e-1(*augmente la norme des vecteurs lors de l'affichage*)
let w = 16*120
let h = 9*120

(* --- Constantes d'intégration --- *)
let meth = "rk" (*euler,rk,verlet*) (*méthode d'intégration*)
let animate = true
let dt = 1.0/.60.0 (*pas de temps (s)*) 

(* --- Constantes physiques --- *)
let mass = 0.25 (*masses des particules (kg)*)
let k_ressort = 750.0 (*constante de raideur (N/m)*)
let k_damping = 0.0 (*constante d'amortissement du ressort*)
let k_repultion = 10.0 (*constante en k_rep/r**4*)
let gravity = (0.0,0.0,-9.81) (*champ gravitationnel m.s-2*)
let nRT = 1e4 (* facteur de corrélation entre la pression et le volume*)
let hauteur_initiale = 100.0 (*hauteur initiale du centre de la sphère*)
let c0 = 0.8 (* energie restante après un choc*)

(* --- Taille du blob --- *)
let rayon = 10.0 (* rayon de la sphère interne*)
let deep = 1(*nombre d'iteration en detail pour la sphère ico*)
