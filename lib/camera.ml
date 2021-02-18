open Base

type t = { translate : V3.t; rotate : Quaternion.t }

let create ~eye ~target ~up ~vertical_fov_deg:_ =
  let neg_z = V3.Infix.(~-V3.unit_z) in
  let xlate = V3.of_points ~src:eye ~tgt:target in
  let xlate' = V3.normalize xlate in
  let _, _, z = V3.coords xlate' in
  let rot1 =
    (* rot1 rotates eye->target to the -Z axis *)
    let theta = Float.acos ~-.z in
    let axis = V3.cross xlate' neg_z in
    Quaternion.rotation axis theta
  in
  let rot2 =
    (* CR dalev: rotate up to the camera's Y axis*)
    ignore up;
    Quaternion.id
  in
  let q = Quaternion.mul rot2 rot1 in
  { translate = xlate; rotate = q }

let transform t p =
  let v = P3.to_v3 p in
  let v' = V3.Infix.( + ) v t.translate in
  P3.of_v3 (Quaternion.transform t.rotate v')
