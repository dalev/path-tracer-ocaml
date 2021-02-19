type t =
  | Solid of Color.t
  | Checker of { width : int; height : int; even : t; odd : t }

module Coord = struct
  type t = float * float

  let create u v = (u, v)
end

let solid c = Solid c

let checker ~width ~height even odd = Checker { width; height; even; odd }

let rec eval t (u, v) =
  match t with
  | Solid c -> c
  | Checker { width; height; even; odd } ->
      let scale_coord c dim = Float.to_int (c *. Float.of_int (dim - 1)) in
      let x' = scale_coord u width in
      let y' = scale_coord v height in
      let same_parity = x' land 1 = y' land 1 in
      let tex = if same_parity then even else odd in
      eval tex (u, v)
