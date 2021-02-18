type t

val sphere : material:Material.t -> center:P3.t -> radius:float -> t

val bbox : t -> Bbox.t

val normal : t -> P3.t -> V3.t

val tex_coord : t -> surface:P3.t -> normal:V3.t -> Texture.Coord.t

val transform : t -> f:(P3.t -> P3.t) -> t
(** Move the shape according to the translation given by [f] *)

val intersect : t -> Ray.t -> t_min:float -> t_max:float -> float option

val material : t -> Material.t
