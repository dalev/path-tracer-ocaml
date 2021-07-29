open! Base
open Stdio
open Ply_format
module Bigstring = Base_bigstring

let main ply =
  printf "== PLY Summary ==\n{%s}\n" (ply |> [%sexp_of: Ply.t] |> Sexp.to_string_hum);
  let d = Ply.data ply in
  match Map.find d "vertex_indices" with
  | None -> failwith "ply data contains no vertex_indices property"
  | Some inner_map ->
    (match Map.find inner_map "rows" with
    | None -> failwith "BUG: vertex_indices has no rows property"
    | Some (Floats _) -> failwith "got Floats, expected Rows"
    | Some (Ints _) -> failwith "got Ints, expected Rows"
    | Some (Rows faces) ->
      let h = Hashtbl.create (module Int) in
      Array.iter faces ~f:(fun face ->
          let face_size = Array.length face in
          Hashtbl.update h face_size ~f:(function
              | None -> 1
              | Some n -> n + 1));
      printf "\n== Face sizes ==\n";
      Hashtbl.iteri h ~f:(fun ~key:size ~data:count -> printf "%d-gons: %d\n" size count))
;;

let () =
  let start = Time_now.nanoseconds_since_unix_epoch () in
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
  main (Or_error.ok_exn ply);
  let elapsed_ns =
    Float.of_int63 @@ Int63.O.(Time_now.nanoseconds_since_unix_epoch () - start)
  in
  printf "\nFinished in %s ms\n" (Float.to_string_hum @@ (elapsed_ns *. 1e-6))
;;
