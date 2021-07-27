open! Base
open Stdio

let ( let* ) m f = Or_error.bind m ~f
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

module Type = struct
  type t =
    | Char
    | Uchar
    | Short
    | Ushort
    | Int
    | Uint
    | Float
    | Double
  [@@deriving sexp]

  let of_string s =
    match [%of_sexp: t] @@ Sexp.Atom s with
    | t -> Ok t
    | exception _ -> error_s [%message "unrecognized type" ~type_:(s : string)]
  ;;
end

module Property = struct
  type t =
    | Atom of
        { type_ : Type.t
        ; name : string
        }
    | List of
        { length_type : Type.t
        ; elt_type : Type.t
        ; name : string
        }
  [@@deriving sexp_of]

  let parse line =
    match String.split ~on:' ' line with
    | [ "property"; "list"; length_type; elt_type; name ] ->
      let* length_type = Type.of_string length_type in
      let* elt_type = Type.of_string elt_type in
      Ok (List { length_type; elt_type; name })
    | [ "property"; type_; name ] ->
      let* type_ = Type.of_string type_ in
      Ok (Atom { type_; name })
    | _ -> error_s [%message "cannot parse property" ~line (line : string)]
  ;;
end

module Element = struct
  type t =
    { name : string
    ; count : int
    ; properties : Property.t list
    }
  [@@deriving sexp_of]

  let create ~name ~count properties = { name; count; properties }
end

module Header = struct
  type t =
    { format : Format.t
    ; elements : Element.t list
    }
  [@@deriving sexp_of]

  let format t = t.format
  let elements t = t.elements

  let parse_format t =
    match List.find t ~f:(fun line -> String.is_prefix line ~prefix:"format ") with
    | None -> error_s [%message "header has no format line" ~lines:(t : string list)]
    | Some line ->
      (match String.split ~on:' ' line with
      | [ "format"; format_str; "1.0" ] -> Format.of_string format_str
      | _ -> error_s [%message "cannot parse format line" ~line:(line : string)])
  ;;

  let parse_elements t =
    let is_element = String.is_prefix ~prefix:"element " in
    let is_property = String.is_prefix ~prefix:"property " in
    let rec loop lines rev_elts =
      match lines with
      | [] -> Ok (List.rev rev_elts)
      | hd :: tl ->
        (match String.split ~on:' ' hd with
        | [ "element"; name; count_str ] ->
          let count = Int.of_string count_str in
          let props, tl = List.split_while tl ~f:is_property in
          let* props = List.map props ~f:Property.parse |> Or_error.all in
          let elt = Element.create ~name ~count props in
          loop tl (elt :: rev_elts)
        | _ -> error_s [%message "expected element" ~line:(hd : string)])
    in
    loop (List.filter t ~f:(fun line -> is_element line || is_property line)) []
  ;;

  let parse ic =
    let rec loop lines =
      match In_channel.input_line ic with
      | Some "end_header" -> Ok (List.rev lines)
      | Some l -> loop (l :: lines)
      | None -> error_string "missing \"end_header\" line"
    in
    let* lines = loop [] in
    let* format = parse_format lines in
    let* elements = parse_elements lines in
    Ok { format; elements }
  ;;
end

module Data = struct
  type t = unit
end

type t =
  { header : Header.t
  ; data : Data.t
  }

let header t = t.header

let check_file_magic ic =
  let b = Bytes.unsafe_of_string_promise_no_mutation in
  let buf = Bytes.create 4 in
  match In_channel.really_input ic ~buf ~pos:0 ~len:4 with
  | None -> error_string "Could not read ply header (not enough bytes)"
  | Some () ->
    if Bytes.( <> ) buf (b "ply\n")
    then
      errorf
        "expected file to start with \"ply\\n\", but got: %s"
        (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
    else Ok ()
;;

let read_binary_le h _ic =
  let (_ : Element.t list) = Header.elements h in
  Ok ()
;;

let of_in_channel (ic : In_channel.t) =
  let* () = check_file_magic ic in
  let* header = Header.parse ic in
  let fmt = Header.format header in
  let* data =
    match (fmt : Format.t) with
    | Binary_little_endian -> read_binary_le header ic
    | Binary_big_endian | Ascii ->
      error_s [%message "to do: handle message format" ~format:(fmt : Format.t)]
  in
  Ok { header; data }
;;
