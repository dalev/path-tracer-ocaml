type t

val sample : t -> Shader_space.t -> float -> float -> V3.t

val eval : t -> V3.t -> Shader_space.t -> float

val diffuse : t
