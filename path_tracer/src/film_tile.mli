open! Base

type t

val border : t -> int
val create : Tile.t -> filter_kernel:Filter_kernel.t -> t
val write_pixel : t -> x:int -> y:int -> Color.t -> unit
val iter : t -> f:(global_x:int -> global_y:int -> Color.t -> unit) -> unit
