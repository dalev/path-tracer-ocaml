open Base

type t = {
  translate : V3.t;
  rotate : Quaternion.t;
  lower_left_x : float;
  lower_left_y : float;
  view_x : float;
  view_y : float;
}

let to_radians deg = deg *. Float.pi /. 180.0

let create ~eye ~target ~up ~aspect ~vertical_fov_deg =
  let neg_z = V3.Infix.(~-V3.unit_z) in
  let translate = V3.of_points ~src:eye ~tgt:target in
  let xlate' = V3.normalize translate in
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
  let rotate = Quaternion.mul rot2 rot1 in
  let half_height = Float.tan (0.5 *. to_radians vertical_fov_deg) in
  let half_width = aspect *. half_height in
  let lower_left_x = ~-.half_width in
  let lower_left_y = ~-.half_height in
  let view_x = 2.0 *. half_width in
  let view_y = 2.0 *. half_height in
  { translate; rotate; lower_left_x; lower_left_y; view_x; view_y }

let transform t p =
  let v = P3.to_v3 p in
  let v' = V3.Infix.( + ) v t.translate in
  P3.of_v3 (Quaternion.transform t.rotate v')

let ray t dx dy =
  let dir =
    V3.create
      ~x:(t.lower_left_x +. (t.view_x *. dx))
      ~y:(t.lower_left_y +. (t.view_y *. dy))
      ~z:(-1.0)
  in
  Ray.create P3.origin dir
