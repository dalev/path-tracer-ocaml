open Base

type t [@@deriving sexp_of]

module Header : sig
  type t [@@deriving sexp_of]
end

val header : t -> Header.t
val of_bigstring : Base_bigstring.t -> t Or_error.t
