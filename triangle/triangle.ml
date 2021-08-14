open Base
open Path_tracer

module Make (Face : sig
  type t

  val vertices : t -> P3.t * P3.t * P3.t
end) =
struct
  include Face

  let create = Fn.id

  module Hit = struct
    type t =
      { t_hit : float
      ; u : float
      ; v : float
      ; tri : Face.t
      }

    let t_hit t = t.t_hit

    let g_normal t =
      let a, b, c = vertices t.tri in
      let e1 = V3.of_points ~tgt:b ~src:a in
      let e2 = V3.of_points ~tgt:c ~src:a in
      V3.normalize @@ V3.cross e1 e2
    ;;

    let point t =
      let u = t.u
      and v = t.v in
      let w = 1.0 -. u -. v in
      let a, b, c = vertices t.tri in
      let open P3.Infix in
      P3.scale a w + P3.scale b u + P3.scale c v
    ;;

    let tex_coord t = Texture.Coord.create t.u t.v
    let face t = t.tri
  end

  let bbox t =
    let a, b, c = vertices t in
    let lo = P3.map2 ~f:Float.min in
    let hi = P3.map2 ~f:Float.max in
    Bbox.create ~min:(lo (lo a b) c) ~max:(hi (hi a b) c)
  ;;

  let intersect t r ~t_min ~t_max =
    let epsilon = 1e-6 in
    let a, b, c = vertices t in
    let e1 = V3.of_points ~tgt:b ~src:a in
    let e2 = V3.of_points ~tgt:c ~src:a in
    let dir = Ray.direction r in
    let pvec = V3.cross dir e2 in
    let det = V3.dot e1 pvec in
    let open Float.O in
    if Float.abs det < epsilon
    then None
    else (
      let det_inv = 1.0 / det in
      let tvec = V3.of_points ~tgt:(Ray.origin r) ~src:a in
      let u = det_inv * V3.dot tvec pvec in
      let qvec = V3.cross tvec e1 in
      let v = det_inv * V3.dot dir qvec in
      if 0.0 <= u && u <= 1.0 && 0.0 <= v && u + v <= 1.0
      then (
        let t_hit = det_inv * V3.dot e2 qvec in
        if t_min <= t_hit && t_hit <= t_max
        then Some { Hit.t_hit; u; v; tri = t }
        else None)
      else None)
  ;;
end
