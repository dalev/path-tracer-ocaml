type t

module Coord : sig
  type t

  val create : float -> float -> t
end

val solid : Color.t -> t

val checker : width:int -> height:int -> t -> t -> t

val eval : t -> Coord.t -> Color.t
