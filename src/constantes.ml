(*
  Nombre d'essais max pour Runge-Kutta,
  Au dela de cette quantité, il renvoie le problème à l'instant t+dt
*)
let rk_tries = 10

(*Affichage*)
let fac_newt = 0.01 
let animate = true 
let meth = "euler"
let w = 16*120
let h = 9*120

(*La hauteur du sol, c'est un hasard que ça corresponde à la taille de l'écran*)
let floor_y = float_of_int h 

(*Constantes physiques*)
let dt = 1.0/.60.0 (*pas de temps (s)*) 
let mass = 25.0 (*masses des particules (kg)*)
let k_ressort = 1e3 (*constante de raideur (N/m)*)
let k_damping = -150.0 (*constante d'amortissement du ressort*)
let k_repultion = 10.0 (*constante en k_rep/r**4*)
let gravity = (0.0,9.81) (*champ gravitationnel m.s-2*)
let nRT = 1e6 (* facteur de corrélation entre la pression et le volume*)
let p0 = 0.4 (*facteur d'augmentation de n pour chaque anneau*) 

(*Taille du blob *)
let rayon = 100.0 (* rayon du premier anneau*)
let rings = 5 (*nombre d'anneaux*)
let ring = 10 (*nombre de points par anneaux*)
let n = rings * ring (*nombre de points*)
let d_eq = 2.0 *. 3.14/. (float_of_int ring) (*longeur d'equilibre des ressorts (m)*)
let interstice = rayon/.8.0 (*distance entre deux anneaux*)
let theta =  2.0*.3.14/.float_of_int ring (*angle entre deux points d'un anneau, depuis le centre*)
