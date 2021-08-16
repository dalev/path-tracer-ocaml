type t = private
  { row : int
  ; col : int
  ; width : int
  ; height : int
  }

val area : t -> int
val split : t -> max_area:int -> t list
val create : width:int -> height:int -> t
val iter : t -> f:(x:int -> y:int -> unit) -> unit
