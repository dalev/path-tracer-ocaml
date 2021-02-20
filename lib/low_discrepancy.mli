type t

module Sample : sig
  type t

  val get : t -> int -> float
end

val create : int -> t
val step : t -> Sample.t
val split_at : t -> int -> t * t
