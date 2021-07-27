open! Base
open Stdio

let error_s = Or_error.error_s
let errorf = Or_error.errorf
let error_string = Or_error.error_string

module Format = struct
  type t =
    | Ascii
    | Binary_little_endian
    | Binary_big_endian
  [@@deriving sexp]

  let of_string s =
    match [%of_sexp: t] @@ Sexp.Atom s with
    | t -> Ok t
    | exception _ -> error_s [%message "unrecognized format" ~format:(s : string)]
  ;;
end

module Header = struct
  type t = string list [@@deriving sexp_of]

  let format t =
    match List.find t ~f:(fun line -> String.is_prefix line ~prefix:"format ") with
    | None -> error_s [%message "header has no format line" ~header:(t : t)]
    | Some line ->
      (match String.split ~on:' ' line with
      | [ "format"; format_str; "1.0" ] -> Format.of_string format_str
      | _ -> error_s [%message "cannot parse format line" ~line:(line : string)])
  ;;
end

type t = { header : Header.t }

let header t = t.header
let b = Bytes.unsafe_of_string_promise_no_mutation

let read_header_lines ic =
  let rec loop lines =
    match In_channel.input_line ic with
    | Some "end_header" -> Ok (List.rev lines)
    | Some l -> loop (l :: lines)
    | None -> error_string "missing \"end_header\" line"
  in
  loop []
;;

let of_in_channel (ic : In_channel.t) =
  let buf = Bytes.create 4 in
  match In_channel.really_input ic ~buf ~pos:0 ~len:4 with
  | None -> error_string "Could not read ply header (not enough bytes)"
  | Some () ->
    if Bytes.( <> ) buf (b "ply\n")
    then
      errorf
        "expected file to start with \"ply\\n\", but got: %s"
        (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
    else (
      let ( let* ) m f = Or_error.bind m ~f in
      let* header = read_header_lines ic in
      let* _fmt = Header.format header in
      Ok { header })
;;
