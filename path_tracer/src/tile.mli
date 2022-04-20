type t = private
  { row : int
  ; col : int
  ; width : int
  ; height : int
  }

val create : width:int -> height:int -> t
val extend : t -> radius:int -> t
val area : t -> int
val split : t -> max_area:int -> t list

(** [iter t ~f] invokes [f] on global coordinates *)
val iter : t -> f:(x:int -> y:int -> unit) -> unit

(** [iter t ~f] invokes [f] on local coordinates ([0 <= x < t.width], [0 <= y < t.height]) *)
val iter_local : t -> f:(x:int -> y:int -> unit) -> unit
