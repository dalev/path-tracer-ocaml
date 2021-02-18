open! Base

module Geometry = struct
  type t = Sphere of { center : P3.t; radius : float }

  let transform t ~f =
    match t with
    | Sphere { center; radius } -> Sphere { center = f center; radius }
end

type t = { material : Material.t; geometry : Geometry.t }

let sphere ~material ~center ~radius =
  { material; geometry = Geometry.Sphere { center; radius } }

let transform t ~f = { t with geometry = Geometry.transform t.geometry ~f }
