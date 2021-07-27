open Base

type t

module Header : sig
  type t [@@deriving sexp_of]
end

val header : t -> Header.t
val of_bigstring : Bigstringaf.t -> t Or_error.t
