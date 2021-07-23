open! Base

type t

val create : material:Material.t -> center:P3.t -> radius:float -> t
val material : t -> Material.t
val transform : t -> f:(P3.t -> Affine.p3) -> t
val bbox : t -> Bbox.t
val normal : t -> P3.t -> V3.t
val tex_coord : t -> V3.t -> Texture.Coord.t
val intersect : t -> Ray.t -> t_min:float -> t_max:float -> float option
val hit : t -> float -> Ray.t -> Hit.t
