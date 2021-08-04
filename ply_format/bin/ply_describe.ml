open! Base
open Stdio
open Ply_format
module Bigstring = Base_bigstring
module FArray = Caml.Float.Array

type p3 = float * float * float [@@deriving sexp_of]

let main ply =
  printf "== PLY Summary ==\n{%s}\n" (ply |> [%sexp_of: Ply.t] |> Sexp.to_string_hum);
  let d = Ply.data ply in
  let v =
    match Map.find d "vertex" with
    | None -> failwith "ply data has no vertex property"
    | Some v -> v
  in
  let floats_exn : Ply.Data.Column.t -> floatarray = function
    | Floats fs -> fs
    | Ints _ | Rows _ -> failwith "floats_exn: expected Floats"
  in
  let () =
    let ( .%{} ) = FArray.get in
    let xs = floats_exn @@ Map.find_exn v "x" in
    let ys = floats_exn @@ Map.find_exn v "y" in
    let zs = floats_exn @@ Map.find_exn v "z" in
    List.iter2_exn [ "x"; "y"; "z" ] [ xs; ys; zs ] ~f:(fun fld floats ->
        let is_finite = FArray.for_all Float.is_finite floats in
        printf "%s all finite: %b\n" fld is_finite);
    let reduce f fs = FArray.fold_left f fs.%{0} fs in
    let min = reduce Float.min in
    let max = reduce Float.max in
    let lo = min xs, min ys, min zs in
    let hi = max xs, max ys, max zs in
    printf "\nbbox = %s\n" @@ Sexp.to_string_mach ([%sexp_of: p3 * p3] (lo, hi))
  in
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
