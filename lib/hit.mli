type t

val create : t_hit:float -> Shape.t -> t

val t_hit : t -> float

val scatter : t -> Scatter.t
