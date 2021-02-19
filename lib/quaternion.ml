open Base

type t = { r : float; v : V3.t }

let create r v = { r; v }

let id = { r = 1.0; v = V3.zero }

let normalize { r; v } =
  let open Float.O in
  let s = 1.0 / Float.sqrt ((r * r) + V3.quadrance v) in
  { r = r * s; v = V3.scale v s }

let rotation axis angle =
  let axis = V3.normalize axis in
  let half_angle = angle /. 2.0 in
  let r = Float.cos half_angle in
  let v = V3.scale axis (Float.sin half_angle) in
  normalize { r; v }

let mul { r; v } { r = r'; v = v' } =
  let r'' = (r *. r') -. V3.dot v v' in
  let v'' =
    let open V3.Infix in
    V3.cross v v' + V3.scale v' r + V3.scale v r'
  in
  { r = r''; v = v'' }

let conj (t : t) : t =
  let open V3.Infix in
  { t with v = ~-(t.v) }

let transform t v =
  let q = mul (mul t { r = 0.0; v }) (conj t) in
  q.v
