
let foi = float_of_int
let iof = int_of_float
type r3 = float*float*float
let (+$) (a,b,c) (x,y,z) = (a+.x, b+.y, c+.z) 
let (-$) (a,b,c) (x,y,z) = (a-.x, b-.y, c-.z)
let zero = 0.0,0.0,0.0
let ( *$) (x,y,z) s = (s*.x, s*.y, s*.z)
