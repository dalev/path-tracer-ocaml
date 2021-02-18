type t

val create : eye:P3.t -> target:P3.t -> up:V3.t -> vertical_fov_deg:float -> t

val transform : t -> P3.t -> P3.t
(** [transform] takes a world-space into camera-space *)
