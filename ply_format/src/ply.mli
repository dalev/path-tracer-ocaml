open Base

type t [@@deriving sexp_of]

module Header : sig
  type t [@@deriving sexp_of]
end

module Data : sig
  module Column : sig
    type row = int array

    type t =
      | Floats of floatarray
      | Ints of int array
      | Rows of row array
  end

  type t =
    ( string
    , (string, Column.t, String.comparator_witness) Map.t
    , String.comparator_witness )
    Map.t
end

val header : t -> Header.t
val data : t -> Data.t
val of_bigstring : Base_bigstring.t -> t Or_error.t
