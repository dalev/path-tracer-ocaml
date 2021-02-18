include Base

type t = { min : P3.t; max : P3.t }

let create ~min ~max = { min; max }

let min t = t.min

let max t = t.max

let center { min; max } = P3.scale P3.Infix.(min + max) 0.5

let union t t' =
  let min = P3.map2 ~f:Float.min t.min t'.min in
  let max = P3.map2 ~f:Float.max t.max t'.max in
  create ~min ~max

let longest_axis { min; max } =
  let open Float.O in
  let x, y, z = P3.coords P3.Infix.(max - min) in
  let m = Float.max x (Float.max y z) in
  if m = x then Axis.X else if m = y then Axis.Y else Axis.Z

let surface_area { min; max } =
  let v = V3.of_points ~src:min ~tgt:max in
  let v' = V3.yzx v in
  2.0 *. V3.dot v v'
