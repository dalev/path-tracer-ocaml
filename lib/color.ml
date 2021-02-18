open! Base

type t = { r : float; g : float; b : float }

let create ~r ~g ~b = { r; g; b }

let of_v3 v =
  let r, g, b = V3.coords v in
  { r; g; b }
