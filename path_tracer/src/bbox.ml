include Base

type t =
  { min : P3.t
  ; max : P3.t
  }
[@@deriving sexp_of]

let create ~min ~max = { min; max }
let min t = t.min
let max t = t.max
let center { min; max } = P3.scale P3.Infix.(min + max) 0.5

let union t t' =
  let min = P3.map2 t.min t'.min ~f:Float.min in
  let max = P3.map2 t.max t'.max ~f:Float.max in
  create ~min ~max
;;

let union_opt o o' =
  match o, o' with
  | None, other | other, None -> other
  | Some t, Some t' -> Some (union t t')
;;

let longest_axis { min; max } =
  let { P3.x; y; z } = P3.Infix.(max - min) in
  let m = Float.max x (Float.max y z) in
  let open Float.O in
  if m = x then Axis.X else if m = y then Axis.Y else Axis.Z
;;

let surface_area { min; max } =
  let { P3.x; y; z } = P3.Infix.(max - min) in
  let open Float.O in
  let a = Stdlib.Float.fma x y (Stdlib.Float.fma y z (z * x)) in
  2.0 * a
;;

let[@inline] hit_range t ray ~t_min ~t_max =
  let invd = Ray.direction_inv ray in
  let o = Ray.origin ray in
  let[@inline] f p = V3.Infix.( * ) (P3.to_v3 P3.Infix.(p - o)) invd in
  let t0' = f t.min
  and t1' = f t.max in
  let a = V3.max_coord @@ V3.map2 ~f:Float.min t0' t1'
  and b = V3.min_coord @@ V3.map2 ~f:Float.max t0' t1' in
  let t_min = Float.max t_min a
  and t_max = Float.min t_max b in
  t_min, t_max
;;

let is_hit t ray ~t_min ~t_max =
  let a, b = (hit_range [@inlined]) t ray ~t_min ~t_max in
  Float.( <= ) a b
;;

let mem t p =
  let[@inline] leq f =
    let m = f p in
    Float.O.(f t.min <= m && m <= f t.max)
  in
  leq P3.x && leq P3.y && leq P3.z
;;
