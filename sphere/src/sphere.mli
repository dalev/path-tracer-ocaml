open! Base
open Path_tracer

type t

val create : material:Material.t -> center:P3.t -> radius:float -> t
val center : t -> P3.t
val material : t -> Material.t
val radius : t -> float
val transform : t -> f:(P3.t -> P3.t) -> t
val bbox : t -> Bbox.t
val normal : t -> P3.t -> V3.t
val tex_coord : t -> V3.t -> Texture.Coord.t
val intersect : t -> Ray.t -> t_min:float -> t_max:float -> float option
val hit : t -> float -> Ray.t -> Hit.t
