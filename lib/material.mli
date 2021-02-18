type t

val lambertian : Texture.t -> t

val scatter : t -> Texture.Coord.t -> Scatter.t
