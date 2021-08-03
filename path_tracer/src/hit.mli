type t =
  { shader_space : Shader_space.t
  ; emit : Color.t
  ; do_scatter : float -> Scatter.t
  }

val shader_space : t -> Shader_space.t
val scatter : t -> float -> Scatter.t
val emit : t -> Color.t
