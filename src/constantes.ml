(* --- Affichage --- *)
let fac_newt = 1.0(*augmente la norme des vecteurs lors de l'affichage*)
let w = 16*120
let h = 9*120
let floor_y = float_of_int h  (*La hauteur du sol, c'est la taille de l'écran par hasard*)

(* --- Constantes d'intégration --- *)
let meth = "verlet" (*euler,rk,verlet*) (*méthode d'intégration*)
let tries = 5 (* Nombre d'essais max pour Runge-Kutta 4 lors de problème de collision. 
Au dela de cette quantité, il renvoie le problème à l'instant t+dt.
Pour les autres méthodes, c'est le nombre de sous-iterations *)
let animate = true
let dt = 1.0/.60.0 (*pas de temps (s)*) 

(* --- Constantes physiques --- *)
let mass = 0.25 (*masses des particules (kg)*)
let k_ressort = 5.0 (*constante de raideur (N/m)*)
let k_damping = 1e-3 (*constante d'amortissement du ressort*)
let k_repultion = 10.0(*constante en k_rep/r**4*)
let gravity = (0.0,0.0,-9.81) (*champ gravitationnel m.s-2*)
let nRT = 1e4(* facteur de corrélation entre la pression et le volume*)
let p0 = 0.4 (*facteur d'augmentation de n pour chaque anneau*) 

(* --- Taille du blob --- *)
let rayon = 10.0 (* rayon de la sphère interne*)
let n = Icosphere.n (*nombre de points sur la sphère*)
let rings = 1 (*nombre de sphères*)
let interstice = rayon/.8.0 (*distance entre deux sphères*)
