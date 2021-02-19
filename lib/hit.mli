type t

val create : t_hit:float -> Shape.t -> Ray.t -> t

val t_hit : t -> float

val point : t -> P3.t

val omega_i : t -> V3.t

val shader_space : t -> Shader_space.t

val scatter : t -> Scatter.t

val emit : t -> Color.t
