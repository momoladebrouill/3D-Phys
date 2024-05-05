(* --- Affichage --- *)
let fac_newt = 5e2(*augmente la norme des vecteurs lors de l'affichage*)
let w = 16*120
let h = 9*120
let floor_y = float_of_int h  (*La hauteur du sol, c'est la taille de l'écran par hasard*)

(* --- Constantes d'intégration --- *)
let meth = "rk" (*euler,rk,verlet*) (*méthode d'intégration*)
let tries = 5 (* Nombre d'essais max pour Runge-Kutta 4 lors de problème de collision. 
Au dela de cette quantité, il renvoie le problème à l'instant t+dt.
Pour les autres méthodes, c'est le nombre de sous-iterations *)
let animate = true
let dt = 1.0 (*pas de temps (s)*) 

(* --- Constantes physiques --- *)
let mass = 25.0 (*masses des particules (kg)*)
let k_ressort = 0.1 (*constante de raideur (N/m)*)
let k_damping = 0.005 (*constante d'amortissement du ressort*)
let k_repultion = 10.0 (*constante en k_rep/r**4*)
let gravity = (0.0,0.0,-0.001) (*champ gravitationnel m.s-2*)
let nRT = 0.1 (* facteur de corrélation entre la pression et le volume*)
let p0 = 0.4 (*facteur d'augmentation de n pour chaque anneau*) 

(* --- Taille du blob --- *)
let rayon = 100.0 (* rayon de la sphère interne*)
let n = Icosphere.n (*nombre de points sur la sphère*)
let rings = 1 (*nombre de sphères*)
let interstice = rayon/.8.0 (*distance entre deux sphères*)
