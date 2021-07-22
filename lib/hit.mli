type t

val create : t_hit:float -> Shape.t -> Ray.t -> t
val shader_space : t -> Shader_space.t
val scatter : t -> float -> Scatter.t
val emit : t -> Color.t
