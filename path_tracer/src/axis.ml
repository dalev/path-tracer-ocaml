type t =
  | X
  | Y
  | Z

let all = [ X; Y; Z ]
let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c

let tuple_selector = function
  | X -> first
  | Y -> second
  | Z -> third
;;