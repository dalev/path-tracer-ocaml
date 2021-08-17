type t

val create : V3.t -> P3.t -> t
val world_ray : t -> V3.t -> Ray.t
val world_origin : t -> P3.t
val world_normal : t -> V3.t
val reflect : t -> V3.t -> V3.t
val refract : t -> V3.t -> float -> V3.t
val unit_square_to_hemisphere : float -> float -> V3.t
val omega_i : t -> Ray.t -> V3.t
