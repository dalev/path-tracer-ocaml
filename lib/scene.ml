open Base

type t = { shapes : Shape.t list }

let create camera shapes =
  let shapes =
    List.map shapes ~f:(fun s -> Shape.transform s ~f:(Camera.transform camera))
  in
  { shapes }
