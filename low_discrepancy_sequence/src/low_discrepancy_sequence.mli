(** [t] is a representation of an unbounded sequence of sample vectors. *)
type t

(** [create ~dimension:d] produces a sequence of sample vectors in the unit hypercube \[0, 1)^d. *)
val create : dimension:int -> t

(** [get t ~offset ~dimension] produces the [dimension]'th entry in the [offset]'th sample-vector *)
val get : t -> offset:int -> dimension:int -> float
