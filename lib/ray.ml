type t = { origin : P3.t; direction : V3.t }

let create origin direction = { origin; direction }

let origin t = t.origin

let direction t = t.direction
