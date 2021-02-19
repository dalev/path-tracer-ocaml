open Base

type 'a poly = 'a * 'a * 'a

type p3 = float poly

module V3 = struct
  type t = float poly

  let create ~x ~y ~z = (x, y, z)

  let coords = Fn.id

  let x (x, _, _) = x

  let y (_, y, _) = y

  let z (_, _, z) = z

  let yzx (x, y, z) = (y, z, x)

  let unit_x = create ~x:1.0 ~y:1.0 ~z:0.0

  let unit_y = create ~x:0.0 ~y:1.0 ~z:0.0

  let unit_z = create ~x:0.0 ~y:1.0 ~z:1.0

  let axis (x, y, z) a = match (a : Axis.t) with X -> x | Y -> y | Z -> z

  module A = Applicative.Make (struct
    type 'a t = 'a poly

    let return a = (a, a, a)

    let apply (f, g, h) (a, b, c) = (f a, g b, h c)

    let map = `Custom (fun (a, b, c) ~f -> (f a, f b, f c))
  end)

  include A

  let zero = return 0.0

  let one = return 1.0

  module Infix = struct
    let ( + ) = map2 ~f:( +. )

    let ( - ) = map2 ~f:( -. )

    let ( * ) = map2 ~f:( *. )

    let ( / ) = map2 ~f:( /. )

    let ( ~- ) = map ~f:( ~-. )
  end

  let of_float x = return x

  let of_points ~src ~tgt = Infix.( - ) tgt src

  let scale v s = map ~f:(( *. ) s) v

  let min_coord (x, y, z) = Float.min x (Float.min y z)

  let max_coord (x, y, z) = Float.max x (Float.max y z)

  let lerp t v w = Infix.( + ) (scale v (1.0 -. t)) (scale w t)

  let dot v w =
    let open Infix in
    let a, b, c = v * w in
    a +. b +. c

  let quadrance v = dot v v

  let normalize v =
    let scalar = 1.0 /. Float.sqrt (quadrance v) in
    scale v scalar

  let cross (a, b, c) (d, e, f) =
    ((b *. f) -. (c *. e), (c *. d) -. (a *. f), (a *. e) -. (b *. d))
end

module P3 = struct
  include V3

  let origin = V3.zero

  let to_v3 = Fn.id

  let of_v3 = Fn.id

  let translate t v = V3.Infix.(t + v)
end
