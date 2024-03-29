type t

val create : r:float -> g:float -> b:float -> t
val to_rgb : t -> float * float * float

(** [of_v3] maps x -> red, y -> green, z -> blue *)
val of_v3 : V3.t -> t

val to_v3 : t -> V3.t
val black : t
val white : t
val zero : t
val scale : t -> float -> t
val map : t -> f:(float -> float) -> t
val lerp : float -> t -> t -> t
val max_coord : t -> float
val fma : t -> t -> t -> t

module Infix : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
end
