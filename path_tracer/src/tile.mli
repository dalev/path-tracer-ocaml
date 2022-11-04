type t = private
  { row : int
  ; col : int
  ; width : int
  ; height : int
  }

val create : width:int -> height:int -> t
val create' : width:int -> height:int -> row:int -> col:int -> t
val width : t -> int
val height : t -> int
val extend : t -> radius:int -> t
val area : t -> int
val split : t -> max_area:int -> t list

(** [iter_global t ~f] invokes [f] on global coordinates *)
val iter_global : t -> f:(x:int -> y:int -> unit) -> unit

(** [iter_local t ~f] invokes [f] on local coordinates ([0 <= x < t.width], [0 <= y < t.height]) *)
val iter_local : t -> f:(x:int -> y:int -> unit) -> unit

(** [iter t ~f] invokes [f] on both local & global coordinates *)
val iter
  :  t
  -> f:(local_x:int -> local_y:int -> global_x:int -> global_y:int -> unit)
  -> unit

val fold
  :  t
  -> init:'a
  -> f:(local_x:int -> local_y:int -> global_x:int -> global_y:int -> 'a -> 'a)
  -> 'a
