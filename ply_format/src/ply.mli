open Base
open Stdio

type t

module Header : sig
  type t = string list [@@deriving sexp_of]
end

val header : t -> Header.t
val of_in_channel : In_channel.t -> t
