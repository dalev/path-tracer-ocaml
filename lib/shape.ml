open! Base

type t = Sphere of { center : P3.t; radius : float }

let sphere ~center ~radius = Sphere { center; radius }
