open! Base

type t = {r: float; g: float; b: float}

let create ~r ~g ~b = {r; g; b}

let of_v3 v =
  let r, g, b = V3.coords v in
  {r; g; b}

let black = {r= 0.0; g= 0.0; b= 0.0}
let white = {r= 1.0; g= 1.0; b= 1.0}

let map c ~f =
  let r = f c.r in
  let g = f c.g in
  let b = f c.b in
  {r; g; b}

let map2 c c' ~f =
  let r = f c.r c'.r in
  let g = f c.g c'.g in
  let b = f c.b c'.b in
  {r; g; b}

let[@inline] scale t s = map t ~f:(( *. ) s)

module Infix = struct
  let ( + ) = map2 ~f:( +. )

  let ( - ) = map2 ~f:( -. )

  let ( * ) = map2 ~f:( *. )
end
[@ocamlformat "module-item-spacing=sparse"]
