type t = Solid of Color.t

module Coord = struct
  type t = float * float

  let create u v = (u, v)
end

let solid c = Solid c

let eval t _coord = match t with Solid c -> c
