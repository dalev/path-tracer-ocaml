open! Base

module Geometry = struct
  type t = Sphere of {center: P3.t; radius: float}

  let transform t ~f =
    match t with Sphere {center; radius} -> Sphere {center= f center; radius}

  let bbox t =
    match t with
    | Sphere {center; radius} ->
        let r = V3.of_float radius in
        Bbox.create ~min:(P3.translate center V3.Infix.(~-r)) ~max:(P3.translate center r)

  let normal t pt =
    match t with
    | Sphere {center; radius= _} -> V3.normalize @@ V3.of_points ~src:center ~tgt:pt

  let tex_coord t _surface_pt normal =
    let open Float.O in
    match t with
    | Sphere {center= _; radius= _} ->
        let x, y, z = V3.coords normal in
        let theta = Float.acos (-y) in
        let phi = Float.pi + Float.atan2 (-z) x in
        let u = phi / (2.0 * Float.pi) in
        let v = theta / Float.pi in
        assert (0.0 <= u) ;
        assert (u <= 1.) ;
        assert (0.0 <= v) ;
        assert (v <= 1.) ;
        Texture.Coord.create u v

  let intersect t ray ~t_min ~t_max =
    match t with
    | Sphere {center; radius} ->
        let open Float.O in
        let d = Ray.direction ray in
        let r2 = radius * radius in
        let f = V3.of_points ~src:(Ray.origin ray) ~tgt:center in
        let b' = V3.dot f d in
        let a = V3.quadrance d in
        let discrim = r2 - V3.quadrance (V3.Infix.( - ) (V3.scale d (b' / a)) f) in
        if discrim < 0.0 then None
        else
          let sign_b' = Sign.to_float (Float.sign_exn b') in
          let q = b' + (sign_b' * Float.sqrt (a * discrim)) in
          let c = V3.quadrance f - r2 in
          let t_hit = if c > 0.0 then c / q else q / a in
          if t_min <= t_hit && t_hit <= t_max then Some t_hit else None
end

type t = {material: Material.t; geometry: Geometry.t}

let material t = t.material

let sphere ~material ~center ~radius =
  {material; geometry= Geometry.Sphere {center; radius}}

let transform t ~f = {t with geometry= Geometry.transform t.geometry ~f}
let intersect t ray ~t_min ~t_max = Geometry.intersect t.geometry ray ~t_min ~t_max
let bbox t = Geometry.bbox t.geometry
let normal t pt = Geometry.normal t.geometry pt
let tex_coord t ~surface ~normal = Geometry.tex_coord t.geometry surface normal
