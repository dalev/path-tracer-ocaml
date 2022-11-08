include V3

let create ~r ~g ~b = V3.create ~x:r ~y:g ~z:b
let to_rgb { V3.x; y; z } = x, y, z
let of_v3 v = v
let to_v3 v = v
let black = zero
let white = one
