
let x = 0.525731112119133606
let z = 0.850650808352039932
let n = 0.0

let vertices = [
    (-.x, n, z);
    (x, n, z);
    (-.x, n, -.z);
    (x, n, -.z);
    (n, z, x);
    (n, z, -.x);
    (n, -.z, x);
    (n, -.z, -.x);
    (z, x, n);
    (-.z, x, n);
    (z, -.x, n);
    (-.z, -.x, n)
]

let icosahedron = vertices

let indices_triangles = [
(0,4,1);(0,9,4);(9,5,4);(4,5,8);(4,8,1);(8,10,1);(8,3,10);(5,3,8);(5,2,3);(2,7,3);(7,10,3);(7,6,10);(7,11,6);(11,0,6);(0,1,6);(6,1,10);(9,0,11);(9,11,2);(9,2,5);(7,2,11)
]


(* TODO : subdivide icosahedron, then normalize vertices, then return the list of triangles (triplets of vertices) *)


let demo () = ()
