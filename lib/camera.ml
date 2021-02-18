open Base

module Mat4 = struct
  type 'a v4 = V4 of 'a * 'a * 'a * 'a

  type t = float v4 v4

  let v4 a b c d = V4 (a, b, c, d)

  let dot4 (V4 (a, b, c, d)) (V4 (a', b', c', d')) =
    let open Float.O in
    (a * a') + (b * b') + (c * c') + (d * d')

  let look_at ~eye ~tgt ~up =
    let z' = V3.normalize (V3.Infix.( - ) tgt eye) in
    let x' = V3.normalize (V3.cross z' (V3.normalize up)) in
    let y' = V3.normalize (V3.cross x' z') in
    let e v =
      let x, y, z = V3.coords v in
      v4 x y z ~-.(V3.dot eye v)
    in
    let e' v =
      let x, y, z = V3.coords v in
      v4 ~-.x ~-.y ~-.z (V3.dot eye v)
    in
    v4 (e x') (e y') (e' z') (v4 0.0 0.0 0.0 1.0)

  let mul (V4 (r0, r1, r2, r3)) v =
    let f = dot4 v in
    v4 (f r0) (f r1) (f r2) (f r3)

  let homogeneous p =
    let x, y, z = V3.coords (P3.to_v3 p) in
    v4 x y z 1.0

  let transform t p =
    let v = homogeneous p in
    let (V4 (x, y, z, w)) = mul t v in
    P3.of_v3 (V3.scale (V3.create ~x ~y ~z) (1.0 /. w))
end

type t = {
  translate : V3.t;
  rotate : Quaternion.t;
  look_at : Mat4.t;
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
  let look_at = Mat4.look_at ~eye:(P3.to_v3 eye) ~tgt:(P3.to_v3 target) ~up in
  { translate; rotate; look_at; lower_left_x; lower_left_y; view_x; view_y }

let _broken_transform t p =
  let v = P3.to_v3 p in
  let v' = V3.Infix.( + ) v t.translate in
  P3.of_v3 (Quaternion.transform t.rotate v')

let transform t p = Mat4.transform t.look_at p

let ray t dx dy =
  let dir =
    V3.create
      ~x:(t.lower_left_x +. (t.view_x *. dx))
      ~y:(t.lower_left_y +. (t.view_y *. dy))
      ~z:(-1.0)
  in
  Ray.create P3.origin dir
