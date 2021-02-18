type t = { eye : P3.t; target : P3.t; up : V3.t }

let create ~eye ~target ~up = { eye; target; up }
