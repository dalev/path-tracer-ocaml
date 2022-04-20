module Binomial : sig
  val pow_falling : int -> int -> int
  val factorial : int -> int
  val binomial : int -> int -> int

  type t

  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  val create : order:int -> pixel_radius:int -> t
end
