
let foi = float_of_int
let iof = int_of_float
type r3 = float*float*float
let (+$) (a,b,c) (x,y,z) = (a+.x, b+.y, c+.z) 
let (-$) (a,b,c) (x,y,z) = (a-.x, b-.y, c-.z)
let ( *$) (x,y,z) s = (s*.x, s*.y, s*.z)
let w = 1000
let h = 1000

let r3_to_vec3 pos =
    let x,y,z = pos in
    Raylib.Vector3.create x z y
let zero = Raylib.Vector3.create 0. 0. 0.

let fst (a, _, _) = a
let snd (_, b, _) = b
let trd (_, _, c) = c
