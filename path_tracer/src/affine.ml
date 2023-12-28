open Base

type p3 =
  { x : float
  ; y : float
  ; z : float
  }
[@@deriving sexp_of]

let to_string { x; y; z } = Printf.sprintf "(%f, %f, %f)" x y z
let pp f t = Stdlib.Format.pp_print_string f (to_string t)

module V3 = struct
  type t = p3 =
    { x : float
    ; y : float
    ; z : float
    }
  [@@deriving sexp_of]

  let pp = pp
  let create ~x ~y ~z : t = { x; y; z }
  let x t = t.x
  let y t = t.y
  let z t = t.z
  let unit_x = create ~x:1.0 ~y:0.0 ~z:0.0
  let unit_y = create ~x:0.0 ~y:1.0 ~z:0.0
  let unit_z = create ~x:0.0 ~y:0.0 ~z:1.0

  let axis a =
    match (a : Axis.t) with
    | X -> x
    | Y -> y
    | Z -> z
  ;;

  let[@inline] map { x; y; z } ~f = { x = f x; y = f y; z = f z }
  let[@inline] map2 a b ~f = { x = f a.x b.x; y = f a.y b.y; z = f a.z b.z }
  let[@inline] map3 a b c ~f = { x = f a.x b.x c.x; y = f a.y b.y c.y; z = f a.z b.z c.z }
  let of_float a = { x = a; y = a; z = a }
  let zero = of_float 0.0
  let one = of_float 1.0

  module Infix = struct
    let[@inline] ( + ) a b = map2 ~f:( +. ) a b
    let[@inline] ( - ) a b = map2 ~f:( -. ) a b
    let[@inline] ( * ) a b = map2 ~f:( *. ) a b
    let[@inline] ( / ) a b = map2 ~f:( /. ) a b
    let[@inline] ( ~- ) a = map ~f:( ~-. ) a
  end

  let[@inline] of_points ~src ~tgt = Infix.( - ) tgt src
  let[@inline] fma u v w = map3 ~f:Stdlib.Float.fma u v w

  (* These two inlines are to avoid alloc in Bbox.is_hit *)
  let[@inline] min_coord { x; y; z } = Float.min x (Float.min y z)
  let[@inline] max_coord { x; y; z } = Float.max x (Float.max y z)

  (* This inline is to reduce alloc in sphere intersect *)
  let[@inline] dot v w = Stdlib.Float.fma v.x w.x (Stdlib.Float.fma v.y w.y (v.z *. w.z))
  let[@inline] scale v s = map ~f:(( *. ) s) v
  let[@inline] quadrance v = dot v v
  let lerp t v w = Infix.( + ) (scale v (1.0 -. t)) (scale w t)

  let normalize v =
    let scalar = 1.0 /. Float.hypot v.x (Float.hypot v.y v.z) in
    scale v scalar
  ;;

  let cross { x = a; y = b; z = c } { x = d; y = e; z = f } =
    let[@inline] h w x y z = Stdlib.Float.fma w x @@ Float.neg (y *. z) in
    { x = h b f c e; y = h c d a f; z = h a e b d }
  ;;
end

module P3 = struct
  include V3

  let origin = V3.zero
  let to_v3 = Fn.id
  let of_v3 = Fn.id
  let translate t v = Infix.(t + v)
end
