type p3

module V3 : sig
  type t

  val create : x:float -> y:float -> z:float -> t

  val zero : t

  val unit_x : t

  val unit_y : t

  val unit_z : t

  val quadrance : t -> float

  val dot : t -> t -> float

  val cross : t -> t -> t

  val scale : t -> float -> t

  val normalize : t -> t

  val of_points : src:p3 -> tgt:p3 -> t

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

  val origin : t

  val to_v3 : t -> V3.t
end
