type t = {origin: P3.t; direction: V3.t; direction_inv: V3.t}

let create origin direction =
  let direction_inv = V3.map direction ~f:(fun x -> 1.0 /. x) in
  {origin; direction; direction_inv}

let origin t = t.origin
let direction t = t.direction
let direction_inv t = t.direction_inv
let point_at ray time = P3.translate (origin ray) @@ V3.scale (direction ray) time
