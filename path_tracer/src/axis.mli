type t =
  | X
  | Y
  | Z

val all : t list
val tuple_selector : t -> 'a * 'a * 'a -> 'a
