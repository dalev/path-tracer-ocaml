open! Base
module Bigstring = Core_kernel.Bigstring
module Bigsubstring = Core_kernel.Bigsubstring

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

  let size = function
    | Char | Uchar -> 1
    | Short | Ushort -> 2
    | Int | Uint | Float -> 4
    | Double -> 8
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

  let is_atomic = function
    | Atom _ -> true
    | List _ -> false
  ;;

  let size_exn = function
    | Atom { type_; _ } -> Type.size type_
    | List _ -> failwith "BUG: Property.size_exn of List"
  ;;

  module Values = struct
    type t = Floats of floatarray

    let sexp_of_t = function
      | Floats fs -> [%message "floatarray" ~length:(Caml.Float.Array.length fs : int)]
    ;;
  end

  let create_values ts ~len =
    let offset = ref 0 in
    List.map ts ~f:(fun t ->
        match t with
        | List _ -> failwith "BUG: create_values of List"
        | Atom { type_; name } ->
          let size = Type.size type_ in
          let pos = !offset in
          offset := !offset + size;
          (match (type_ : Type.t) with
          | Float ->
            let a = Caml.Float.Array.create len in
            let extract i s =
              let base = Bigsubstring.base s in
              let pos = Bigsubstring.pos s + pos in
              let n = Int32.float_of_bits @@ Bigstringaf.get_int32_le base pos in
              Caml.Float.Array.set a i n
            in
            extract, (name, Values.Floats a)
          | _ -> assert false))
  ;;
end

module Element = struct
  type t =
    { name : string
    ; count : int
    ; properties : Property.t list
    }
  [@@deriving sexp_of]

  let name t = t.name
  let count t = t.count
  let properties t = t.properties
  let create ~name ~count properties = { name; count; properties }

  let width t =
    if List.for_all t.properties ~f:Property.is_atomic
    then `Fixed (List.sum (module Int) t.properties ~f:Property.size_exn)
    else `Variable
  ;;
end

module Input = struct
  type t = { mutable buf : Bigsubstring.t }

  let take t ~len =
    let prefix = Bigsubstring.prefix t.buf len in
    t.buf <- Bigsubstring.drop_prefix t.buf len;
    prefix
  ;;

  let consume_substring t ~pos ~len =
    let s = Bigsubstring.sub t.buf ~pos ~len |> Bigsubstring.to_string in
    t.buf <- Bigsubstring.drop_prefix t.buf (pos + len);
    s
  ;;

  let create buf = { buf = Bigsubstring.create buf }
  let length t = Bigsubstring.length t.buf

  let read_line t =
    let base = Bigsubstring.base t.buf in
    let pos = Bigsubstring.pos t.buf in
    let len = Bigsubstring.length t.buf in
    match Bigstring.find '\n' base ~pos ~len with
    | None ->
      let line = t.buf in
      t.buf <- Bigsubstring.create (Bigstring.create 0);
      Some (Bigsubstring.to_string line)
    | Some base_nl_idx ->
      let line_len = base_nl_idx - pos in
      let line = Bigsubstring.prefix t.buf line_len in
      t.buf <- Bigsubstring.drop_prefix t.buf (line_len + 1);
      Some (Bigsubstring.to_string line)
  ;;
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
      match Input.read_line ic with
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
  type t =
    ( string
    , (string, Property.Values.t, String.comparator_witness) Map.t
    , String.comparator_witness )
    Map.t

  let sexp_of_t =
    Map.sexp_of_m__t
      (module String)
      (Map.sexp_of_m__t (module String) Property.Values.sexp_of_t)
  ;;
end

type t =
  { header : Header.t
  ; data : Data.t
  }
[@@deriving sexp_of]

let header t = t.header

let check_file_magic ic =
  if Input.length ic < 4
  then error_string "Could not read ply header (not enough bytes)"
  else (
    match Input.consume_substring ic ~pos:0 ~len:4 with
    | "ply\n" -> Ok ()
    | other -> errorf "expected file to start with \"ply\\n\", but got: %s" other)
;;

let read_binary_le h ic =
  Ok
    (List.map (Header.elements h) ~f:(fun e ->
         let name = Element.name e in
         let count = Element.count e in
         let ps = Element.properties e in
         match Element.width e with
         | `Fixed width ->
           let extractors, values = List.unzip @@ Property.create_values ps ~len:count in
           let values = Map.of_alist_exn (module String) values in
           for i = 0 to count - 1 do
             let buf = Input.take ic ~len:width in
             List.iter extractors ~f:(fun extract -> extract i buf)
           done;
           name, values
         | `Variable -> name, Map.empty (module String)))
;;

let of_bigstring (b : Bigstring.t) =
  let ic = Input.create b in
  let* () = check_file_magic ic in
  let* header = Header.parse ic in
  let fmt = Header.format header in
  let* data =
    match (fmt : Format.t) with
    | Binary_little_endian -> read_binary_le header ic
    | Binary_big_endian | Ascii ->
      error_s [%message "to do: handle message format" ~format:(fmt : Format.t)]
  in
  Ok { header; data = Map.of_alist_exn (module String) data }
;;
