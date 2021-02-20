type t

val lambertian : Texture.t -> t
val metal : Texture.t -> t
val glass : t

val scatter :
     t
  -> Shader_space.t
  -> Texture.Coord.t
  -> omega_i:V3.t
  -> hit_front:bool
  -> float
  -> Scatter.t

val emit : t -> Texture.Coord.t -> Color.t
