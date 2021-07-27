open! Base
open Stdio
open Ply_format
module Bigstring = Base_bigstring

let main ply =
  printf "== PLY Summary ==\n{%s}\n" (ply |> [%sexp_of: Ply.t] |> Sexp.to_string_hum)
;;

let () =
  let argv = Sys.get_argv () in
  if Array.length argv <> 2 then failwith "expected argument: path to .ply file";
  let shared = false in
  let fd = Unix.openfile argv.(1) [ Unix.O_RDONLY ] 0o600 in
  let (bs : Bigstring.t) =
    Bigarray.array1_of_genarray
    @@ Unix.map_file fd Bigarray.char Bigarray.c_layout shared [| -1 |]
  in
  let ply = Ply.of_bigstring bs in
  Unix.close fd;
  main (Or_error.ok_exn ply)
;;
