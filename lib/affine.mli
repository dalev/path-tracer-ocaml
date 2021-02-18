open Base

type p3

module V3 : sig
  type t

  val create : x:float -> y:float -> z:float -> t

  val coords : t -> float * float * float

  val yzx : t -> t

  val zero : t

  val unit_x : t

  val unit_y : t

  val unit_z : t

  val axis : t -> Axis.t -> float

  val quadrance : t -> float

  val dot : t -> t -> float

  val cross : t -> t -> t

  val scale : t -> float -> t

  val lerp : float -> t -> t -> t

  val normalize : t -> t

  val of_float : float -> t

  val of_points : src:p3 -> tgt:p3 -> t

  val map2 : t -> t -> f:(float -> float -> float) -> t

  module Infix : sig
    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( * ) : t -> t -> t

    val ( / ) : t -> t -> t

    val ( ~- ) : t -> t
  end
end

module P3 : sig
  type t = p3

  val create : x:float -> y:float -> z:float -> t

  val coords : t -> float * float * float

  val axis : t -> Axis.t -> float

  val origin : t

  val translate : t -> V3.t -> t

  val to_v3 : t -> V3.t

  val of_v3 : V3.t -> t

  val scale : t -> float -> t

  val map2 : t -> t -> f:(float -> float -> float) -> t

  module Infix : sig
    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( * ) : t -> t -> t

    val ( / ) : t -> t -> t

    val ( ~- ) : t -> t
  end
end
