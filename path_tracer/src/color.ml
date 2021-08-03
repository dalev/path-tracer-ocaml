type t = V3.t

let create ~r ~g ~b = V3.create ~x:r ~y:g ~z:b
let to_rgb = V3.coords
let of_v3 v = v
let to_v3 v = v
let black = create ~r:0.0 ~g:0.0 ~b:0.0
let white = create ~r:1.0 ~g:1.0 ~b:1.0
let map = V3.map
let scale = V3.scale
let lerp = V3.lerp

module Infix = V3.Infix