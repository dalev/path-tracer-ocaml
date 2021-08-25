open Base
open Path_tracer

module Make (Face : sig
  type t

  val vertices : t -> P3.t * P3.t * P3.t
  val material : t -> Material.t
  val tex_coords : t -> Texture.Coord.t * Texture.Coord.t * Texture.Coord.t
end) : sig
  type t = Face.t

  module Hit : sig
    type t

    val t_hit : t -> float
    val to_hit : t -> Ray.t -> Path_tracer.Hit.t
  end

  val bbox : t -> Bbox.t
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> Hit.t option
end
