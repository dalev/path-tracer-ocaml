open! Base
open Stdio

module Header = struct
  type t = string list [@@deriving sexp_of]
end

type t = { header : Header.t }

let header t = t.header
let b = Bytes.unsafe_of_string_promise_no_mutation

let read_header_lines ic =
  let rec loop lines =
    let l = In_channel.input_line_exn ic in
    if String.( = ) l "end_header" then List.rev lines else loop (l :: lines)
  in
  loop []
;;

let of_in_channel (ic : In_channel.t) =
  let buf = Bytes.create 4 in
  (match In_channel.really_input ic ~buf ~pos:0 ~len:4 with
  | None -> failwith "Could not read ply header (not enough bytes)"
  | Some () -> ());
  if Bytes.( <> ) buf (b "ply\n")
  then
    Printf.failwithf
      "expected file to start with \"ply\\n\", but got: %s"
      (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
      ();
  let header = read_header_lines ic in
  { header }
;;
