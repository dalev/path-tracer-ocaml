open! Base
module Bigstring = Base_bigstring

let ( let* ) m f = Or_error.bind m ~f
let error_s = Or_error.error_s
let errorf = Or_error.errorf
let error_string = Or_error.error_string

module Input : sig
  type t

  val create : Bigstring.t -> t
  val length : t -> int
  val take : t -> len:int -> string
  val read_line : t -> string option
  val base : t -> Bigstring.t
end = struct
  type t =
    { buf : Bigstring.t
    ; mutable pos : int
    }

  let base t = t.buf

  let take t ~len =
    let prefix = Bigstring.To_string.sub t.buf ~pos:t.pos ~len in
    t.pos <- t.pos + len;
    prefix
  ;;

  let create buf = { buf; pos = 0 }
  let length t = Bigstring.length t.buf - t.pos

  let read_line t =
    let base = t.buf in
    let pos = t.pos in
    let len = length t in
    match Bigstring.find '\n' base ~pos ~len with
    | None ->
      t.pos <- pos + len;
      Some (Bigstring.To_string.sub t.buf ~pos ~len)
    | Some nl_idx ->
      let len = nl_idx - pos in
      t.pos <- nl_idx + 1;
      Some (Bigstring.To_string.sub t.buf ~pos ~len)
  ;;
end

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

  let float_accessor_exn = function
    | Float -> fun base ~pos -> Int32.float_of_bits @@ Bigstringaf.get_int32_le base pos
    | Double -> fun base ~pos -> Int64.float_of_bits @@ Bigstringaf.get_int64_le base pos
    | ty -> raise_s [%message "expected Float|Double" ~type_:(ty : t)]
  ;;

  let int_accessor_exn = function
    | Char -> Bigstring.get_int8
    | Uchar -> Bigstring.get_uint8
    | Short -> Bigstring.get_int8
    | Ushort -> Bigstring.get_uint8
    | Int -> Bigstring.get_int32_le
    | Uint -> Bigstring.get_uint32_le
    | ty -> raise_s [%message "expected integer type" ~type_:(ty : t)]
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
    type t =
      | Floats of floatarray
      | Ints of int array

    let sexp_of_t = function
      | Floats fs -> [%message "floatarray" ~length:(Caml.Float.Array.length fs : int)]
      | Ints is -> [%message "int array" ~length:(Array.length is : int)]
    ;;
  end

  let create_values ts ~len =
    let field_offset = ref 0 in
    List.map ts ~f:(fun t ->
        match t with
        | List _ -> failwith "BUG: create_values of List"
        | Atom { type_; name } ->
          let size = Type.size type_ in
          let offset = !field_offset in
          field_offset := !field_offset + size;
          (match (type_ : Type.t) with
          | Float | Double ->
            let get_float = Type.float_accessor_exn type_ in
            let a = Caml.Float.Array.create len in
            let extract ic ~row_start i =
              let base = Input.base ic in
              let field_start = row_start + offset in
              let n = get_float base ~pos:field_start in
              Caml.Float.Array.set a i n
            in
            extract, (name, Values.Floats a)
          | Char | Uchar | Short | Ushort | Int | Uint ->
            let get_int = Type.int_accessor_exn type_ in
            let a = Array.create ~len 0 in
            let extract ic ~row_start i =
              let base = Input.base ic in
              let field_start = row_start + offset in
              let n = get_int base ~pos:field_start in
              Array.set a i n
            in
            extract, (name, Values.Ints a)))
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

  let fixed_width_parser t ~width ic =
    let ps = t.properties in
    let extractors, values = List.unzip @@ Property.create_values ps ~len:t.count in
    let values = Map.of_alist_exn (module String) values in
    for i = 0 to t.count - 1 do
      let row_start = width * i in
      List.iter extractors ~f:(fun extract -> extract ic ~row_start i)
    done;
    t.name, values
  ;;

  let list_parser ~length_type:_ ~elt_type:_ ~name (_ : Input.t) =
    name, Map.empty (module String)
  ;;

  let parse t =
    match t.properties with
    | [ Property.List { length_type; elt_type; name } ] ->
      list_parser ~length_type ~elt_type ~name
    | ps ->
      if List.for_all ps ~f:Property.is_atomic
      then (
        let width = List.sum (module Int) t.properties ~f:Property.size_exn in
        fixed_width_parser t ~width)
      else failwith "TO DO: parse mixed list/non-list element"
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
    let prefix = Input.take ic ~len:4 in
    match prefix with
    | "ply\n" -> Ok ()
    | other -> errorf "expected file to start with \"ply\\n\", but got: %s" other)
;;

let read_binary_le h ic =
  let read_element e = Element.parse e ic in
  Ok (List.map (Header.elements h) ~f:read_element)
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
