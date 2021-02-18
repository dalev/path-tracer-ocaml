type t

val create :
  eye:P3.t ->
  target:P3.t ->
  up:V3.t ->
  aspect:float ->
  vertical_fov_deg:float ->
  t

val ray : t -> float -> float -> Ray.t

val transform : t -> P3.t -> P3.t
(** [transform] takes a world-space into camera-space *)
