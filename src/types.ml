open Raylib

type 'a graph = 'a Array.t
type listadj = int list array

type vec = float * float * float

type point = {
    pos : vec;
    vit : vec;
    mass : float;
}

type force = vec * Color.t

(*arguments de la fonction somme des forces*)
type arguments = {
  points : point graph;
  k_ressort : float;
  penche : bool;
  center : vec;
}

type status = {
    blob : point graph; (*le tableau des points*)
    t : int; (*temps*)
    k_ressort : float; (*la constante de ressort*)
    penche : bool; (*5.0 ou 0.0 selon si la gravité est inclinée ou non*)
    cam : Camera3D.t; (*la camera*)
}
