type t =
  | X
  | Y
  | Z

let all = [ X; Y; Z ]

let tuple_selector t (a, b, c) =
  match t with
  | X -> a
  | Y -> b
  | Z -> c
;;