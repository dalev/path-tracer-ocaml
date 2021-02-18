type t

val create : P3.t -> V3.t -> t

val origin : t -> P3.t

val direction : t -> V3.t

val direction_inv : t -> V3.t
