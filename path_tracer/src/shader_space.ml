open Base

type t =
  { rotation : Quaternion.t
  ; origin : P3.t
  ; normal : V3.t
  }

let epsilon = 1e-9

let create normal origin =
  let open Float.O in
  let { V3.x; y; z } = normal in
  let rotation =
    if z > 1.0 - epsilon
    then Quaternion.id
    else if z < epsilon - 1.0
    then Quaternion.create 0.0 V3.unit_y
    else
      Quaternion.normalize @@ Quaternion.create (1.0 + z) (V3.create ~x:y ~y:(-x) ~z:0.0)
  in
  { rotation; origin; normal }
;;

let world_origin t = t.origin
let world_normal t = t.normal
let rotate t v = Quaternion.transform t.rotation v

let rotate_inv t v =
  let q' = Quaternion.conj t.rotation in
  Quaternion.transform q' v
;;

let reflect (_ : t) v =
  let { V3.x; y; z } = v in
  let x = ~-.x in
  let y = ~-.y in
  V3.create ~x ~y ~z
;;

let refract (_ : t) wi index =
  let wi_z = V3.z wi in
  let c = Float.min wi_z 1.0 in
  let perp = V3.scale V3.Infix.(V3.create ~x:0.0 ~y:0.0 ~z:c - wi) index in
  let para =
    V3.create ~x:0.0 ~y:0.0 ~z:(-.Float.sqrt (Float.abs (1.0 -. V3.quadrance perp)))
  in
  V3.Infix.(perp + para)
;;

let world_ray t dir_ss =
  let dir = rotate_inv t dir_ss in
  Ray.create (P3.translate t.origin (V3.scale dir 1e-3)) dir
;;

let unit_square_to_hemisphere u v =
  let open Float.O in
  let r = Float.sqrt u in
  let theta = v * 2.0 * Float.pi in
  let x = r * Float.cos theta in
  let y = r * Float.sin theta in
  let z = Float.sqrt (1.0 - u) in
  V3.create ~x ~y ~z
;;

let omega_i t ray =
  let world_dir = V3.Infix.( ~- ) (V3.normalize (Ray.direction ray)) in
  rotate t world_dir
;;
