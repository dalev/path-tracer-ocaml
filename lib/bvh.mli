type t

val create : Shape.t list -> t

val intersect : t -> Ray.t -> t_min:float -> t_max:float -> Hit.t option
