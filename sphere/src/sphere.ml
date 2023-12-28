open! Base
open Path_tracer

type t =
  { material : Material.t
  ; center : P3.t
  ; radius : float
  }

let create ~material ~center ~radius = { material; center; radius }
let material t = t.material
let center t = t.center
let radius t = t.radius
let transform t ~f = { t with center = f t.center }

let bbox { center; radius; _ } =
  let r = V3.of_float radius in
  Bbox.create ~min:(P3.translate center V3.Infix.(~-r)) ~max:(P3.translate center r)
;;

let normal t pt = V3.normalize @@ V3.of_points ~src:t.center ~tgt:pt
let one_over_pi = 1.0 /. Float.pi
let one_over_two_pi = 1.0 /. (2.0 *. Float.pi)

let tex_coord (_ : t) normal =
  let open Float.O in
  let { V3.x; y; z } = normal in
  let theta = Float.acos (-y) in
  let phi = Float.pi + Float.atan2 (-z) x in
  let u = phi * one_over_two_pi in
  let v = theta * one_over_pi in
  Texture.Coord.create u v
;;

let intersect { center; radius; _ } ray ~t_min ~t_max =
  let open Float.O in
  let d = Ray.direction ray in
  let r2 = radius * radius in
  let f = V3.of_points ~src:(Ray.origin ray) ~tgt:center in
  let b' = V3.dot f d in
  let a = V3.quadrance d in
  let discrim = r2 - V3.quadrance (V3.Infix.( - ) (V3.scale d (b' / a)) f) in
  if discrim < 0.0
  then None
  else (
    let sign_b' =
      (* [Sign.to_float] is not correct here: if the ray origin is precisely the circle center, then b' = 0 *)
      if b' >= 0.0 then 1.0 else -1.0
    in
    let q = Stdlib.Float.fma sign_b' (Float.sqrt (a * discrim)) b' in
    let c = V3.quadrance f - r2 in
    let t_hit = if c > 0.0 then c / q else q / a in
    if t_min <= t_hit && t_hit <= t_max then Some t_hit else None)
;;

let hit t t_hit ray =
  let open Float.O in
  let point = Ray.point_at ray t_hit in
  let normal = normal t point in
  let hit_front = V3.dot (Ray.direction ray) normal < 0.0 in
  let normal = if hit_front then normal else V3.Infix.( ~- ) normal in
  let tex_coord = tex_coord t normal in
  let shader_space = Shader_space.create normal point in
  let m = t.material in
  let emit = Material.emit m tex_coord in
  let omega_i = Shader_space.omega_i shader_space ray in
  let do_scatter = Material.scatter m shader_space tex_coord ~omega_i ~hit_front in
  { Hit.shader_space; emit; do_scatter }
;;
