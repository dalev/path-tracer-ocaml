module Sample : sig
  (** A sample vector. *)
  type t

  (** [length t] is the dimension given to [create] *)
  val length : t -> int

  (** [get s i] selects the i'th component of the sample vector [s] (0-based). *)
  val get : t -> int -> float

  (** [s.%{i}] is the same as [get s i]*)
  val ( .%{} ) : t -> int -> float
end

(** [t] is a representation of an unbounded sequence of sample vectors. *)
type t

(** [create ~dimension:d] produces a sequence of sample vectors in the unit hypercube \[0, 1)^d. *)
val create : dimension:int -> t

(** [step] produces a sample vector and the next state. *)
val step : t -> t * Sample.t

(** [split_at t n] produces [lhs, rhs] where [lhs] contains the first [n] 
  sample vectors in [t], and [rhs] contains the remainder. *)
val split_at : t -> int -> t * t

module Simple : sig
  type t

  val create : dimensions:int -> t
  val get : t -> offset:int -> dimension:int -> float
end
