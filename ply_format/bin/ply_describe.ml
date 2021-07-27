open! Base
open Stdio
open Ply_format

let main ply =
  printf
    "== Header ==\n{%s}\n"
    (Ply.header ply |> [%sexp_of: Ply.Header.t] |> Sexp.to_string_hum)
;;

let () =
  let argv = Sys.get_argv () in
  if Array.length argv <> 2 then failwith "expected argument: path to .ply file";
  let ply = In_channel.with_file argv.(1) ~binary:true ~f:Ply.of_in_channel in
  main ply
;;
