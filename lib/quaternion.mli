type t

val rotation : V3.t -> float -> t
(** Given angle in radians *)

val id : t

val transform : t -> V3.t -> V3.t
