open Base
open Path_tracer

module Make (Face : sig
  type t

  val vertices : t -> P3.t * P3.t * P3.t
end) : sig
  type t

  module Hit : sig
    type t

    val t_hit : t -> float
    val g_normal : t -> V3.t
    val point : t -> P3.t
    val tex_coord : t -> Texture.Coord.t
    val face : t -> Face.t
  end

  val create : Face.t -> t
  val bbox : t -> Bbox.t
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> Hit.t option
end
