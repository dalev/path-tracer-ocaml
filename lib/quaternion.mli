type t

val create : float -> V3.t -> t

val rotation : V3.t -> float -> t
(** Given angle in radians *)

val conj : t -> t
val id : t
val mul : t -> t -> t
val normalize : t -> t
val transform : t -> V3.t -> V3.t
