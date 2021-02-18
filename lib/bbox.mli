type t

val create : min:P3.t -> max:P3.t -> t

val is_hit : t -> Ray.t -> t_min:float -> t_max:float -> bool

val min : t -> P3.t

val max : t -> P3.t

val center : t -> P3.t

val union : t -> t -> t

val longest_axis : t -> Axis.t

val surface_area : t -> float
