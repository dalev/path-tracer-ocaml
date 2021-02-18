open! Base

module Geometry = struct
  type t = Sphere of { center : P3.t; radius : float }

  let transform t ~f =
    match t with
    | Sphere { center; radius } -> Sphere { center = f center; radius }

  let intersect t ray t_min t_max =
    match t with
    | Sphere { center; radius } ->
        let open Float.O in
        let d = Ray.direction ray in
        let r2 = radius * radius in
        let f = V3.of_points ~src:(Ray.origin ray) ~tgt:center in
        let b' = V3.dot f d in
        let a = V3.quadrance d in
        let discrim =
          r2 - V3.quadrance (V3.Infix.( - ) (V3.scale d (b' / a)) f)
        in
        if discrim < 0.0 then None
        else
          let sign_b' = Sign.to_float (Float.sign_exn b') in
          let q = b' + (sign_b' * Float.sqrt (a * discrim)) in
          let c = V3.quadrance f - r2 in
          let t_hit = if c > 0.0 then c / q else q / a in
          if t_min <= t_hit && t_hit <= t_max then Some t_hit else None
end

type t = { material : Material.t; geometry : Geometry.t }

let sphere ~material ~center ~radius =
  { material; geometry = Geometry.Sphere { center; radius } }

let transform t ~f = { t with geometry = Geometry.transform t.geometry ~f }

let intersect t ray t_min t_max = Geometry.intersect t.geometry ray t_min t_max
