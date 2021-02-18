type t

val lambertian : Texture.t -> t

val scatter : t -> Shader_space.t -> Texture.Coord.t -> Scatter.t

val emit : t -> Texture.Coord.t -> Color.t
