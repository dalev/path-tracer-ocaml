type t

val sphere : material:Material.t -> center:P3.t -> radius:float -> t

val transform : t -> f:(P3.t -> P3.t) -> t
(** Move the shape according to the translation given by [f] *)

val intersect : t -> Ray.t -> float -> float -> float option

val material : t -> Material.t
