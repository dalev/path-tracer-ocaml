open Base

module Coord = struct
  type t =
    { u : float
    ; v : float
    }

  let create u v = { u; v }
  let t00 = create 0.0 0.0
  let t01 = create 0.0 1.0
  let t10 = create 1.0 0.0
  let t11 = create 1.0 1.0
end

type t = Coord.t -> Color.t

let eval t coord = t coord
let solid c _ = c

let checker ~width ~height even odd =
  let width = Float.of_int (width - 1)
  and height = Float.of_int (height - 1) in
  let parity a = Float.to_int a land 1 in
  fun coord ->
    let { Coord.u; v } = coord in
    let x' = u *. width
    and y' = v *. height in
    let tex = if parity x' = parity y' then even else odd in
    eval tex coord
;;
