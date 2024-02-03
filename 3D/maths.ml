
type r3 = float*float*float
 
let ($+) a b =
     let xa,ya,za = a in
     let xb,yb,zb = b in
     xa+.xb, ya+.yb, za+.zb
let ($-) a b =
    let xa,ya,za = a in
    let xb,yb,zb = b in
    xa-.xb, ya-.yb, za-.zb
let ($*) a q =
    let xa,ya,za = a in
    xa*.q, ya*.q, za*.q
let iof = int_of_float
let foi = float_of_int
