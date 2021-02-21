open Base

type p3 = {x: float; y: float; z: float}

let to_string {x; y; z} = Printf.sprintf "(%f, %f, %f)" x y z
let pp f t = Caml.Format.pp_print_string f (to_string t)

module V3 = struct
  type t = p3

  let pp = pp
  let create ~x ~y ~z : t = {x; y; z}
  let coords {x; y; z} = (x, y, z)
  let x t = t.x
  let y t = t.y
  let z t = t.z
  let yzx {x; y; z} = {x= y; y= z; z= x}
  let unit_x = create ~x:1.0 ~y:0.0 ~z:0.0
  let unit_y = create ~x:0.0 ~y:1.0 ~z:0.0
  let unit_z = create ~x:0.0 ~y:0.0 ~z:1.0
  let axis {x; y; z} a = match (a : Axis.t) with X -> x | Y -> y | Z -> z
  let map {x; y; z} ~f = {x= f x; y= f y; z= f z}
  let map2 a b ~f = {x= f a.x b.x; y= f a.y b.y; z= f a.z b.z}
  let of_float a = {x= a; y= a; z= a}
  let zero = of_float 0.0
  let one = of_float 1.0

  module Infix = struct
    let ( + ) = map2 ~f:( +. )
    let ( - ) = map2 ~f:( -. )
    let ( * ) = map2 ~f:( *. )
    let ( / ) = map2 ~f:( /. )
    let ( ~- ) = map ~f:( ~-. )
  end

  let of_points ~src ~tgt = Infix.( - ) tgt src

  (* These two inlines are to avoid alloc in Bbox.is_hit *)
  let[@inline] min_coord {x; y; z} = Float.min x (Float.min y z)
  let[@inline] max_coord {x; y; z} = Float.max x (Float.max y z)

  (* This inline is to reduce alloc in sphere intersect *)
  let[@inline] dot v w =
    let open Infix in
    let {x; y; z} = v * w in
    x +. y +. z

  let[@inline] scale v s = map ~f:(( *. ) s) v
  let[@inline] quadrance v = dot v v
  let lerp t v w = Infix.( + ) (scale v (1.0 -. t)) (scale w t)

  let normalize v =
    let scalar = 1.0 /. Float.sqrt (quadrance v) in
    scale v scalar

  let cross {x= a; y= b; z= c} {x= d; y= e; z= f} =
    {x= (b *. f) -. (c *. e); y= (c *. d) -. (a *. f); z= (a *. e) -. (b *. d)}
end

module P3 = struct
  include V3

  let origin = V3.zero
  let to_v3 = Fn.id
  let of_v3 = Fn.id
  let translate t v = V3.Infix.(t + v)
end
