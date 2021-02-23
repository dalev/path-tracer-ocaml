module Sample : sig
  (** A sample vector. *)
  type t

  val length : t -> int
  (** [length t] is the dimension given to [create] *)

  val get : t -> int -> float
  (** [get s i] selects the i'th component of the sample vector [s] (0-based). *)
end

(** [t] is a representation of an unbounded sequence of sample vectors. *)
type t

val create : dimension:int -> t
(** [create ~dimension:d] produces a sequence of sample vectors in the unit hypercube \[0, 1)^d. *)

val step : t -> t * Sample.t
(** [step] produces a sample vector and the next state. *)

val split_at : t -> int -> t * t
(** [split_at t n] produces [lhs, rhs] where [lhs] contains the first [n] 
  sample vectors in [t], and [rhs] contains the remainder. *)
