type t

module Coord : sig
  type t = private
    { u : float
    ; v : float
    }

  val create : float -> float -> t
  val t00 : t
  val t10 : t
  val t01 : t
  val t11 : t
end

val solid : Color.t -> t
val checker : width:int -> height:int -> t -> t -> t
val eval : t -> Coord.t -> Color.t
