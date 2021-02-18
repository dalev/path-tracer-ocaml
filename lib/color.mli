type t

val create : r:float -> g:float -> b:float -> t

val rgb : t -> float * float * float

val of_v3 : V3.t -> t
(** [of_v3] maps x -> red, y -> green, z -> blue *)

val black : t

val scale : t -> float -> t

module Infix : sig
  val ( + ) : t -> t -> t

  val ( * ) : t -> t -> t
end
