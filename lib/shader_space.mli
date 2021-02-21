type t

val create : V3.t -> P3.t -> t
val rotate : t -> V3.t -> V3.t
val rotate_inv : t -> V3.t -> V3.t
val reflect : t -> V3.t -> V3.t
val refract : t -> V3.t -> float -> V3.t
val unit_square_to_hemisphere : float -> float -> V3.t
