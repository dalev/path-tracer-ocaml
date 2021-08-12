open Base

module Coord = struct
  type t =
    { u : float
    ; v : float
    }

  let create u v = { u; v }
end

type t = Coord.t -> Color.t

let eval t coord = t coord
let solid c _ = c

let checker ~width ~height even odd coord =
  let { Coord.u; v } = coord in
  let scale_coord c dim = Float.to_int (c *. Float.of_int (dim - 1)) in
  let x' = scale_coord u width in
  let y' = scale_coord v height in
  let parity a = a land 1 in
  let tex = if parity x' = parity y' then even else odd in
  eval tex coord
;;
