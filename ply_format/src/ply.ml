open! Base
open Stdio

module Header = struct
  type t = string list [@@deriving sexp_of]
end

type t = { header : Header.t }

let header t = t.header

let of_in_channel (ic : In_channel.t) =
  let rev_lines = List.init 3 ~f:(fun _ -> In_channel.input_line_exn ic) in
  { header = List.rev rev_lines }
;;
