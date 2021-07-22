type 'a t

val create : 'a array -> 'a t
val length : 'a t -> int
val base_index : 'a t -> int -> int
val get : 'a t -> int -> 'a
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val iter : 'a t -> f:('a -> 'b) -> unit
val split_at : 'a t -> int -> 'a t * 'a t
val partition_in_place : 'a t -> on_lhs:('a -> bool) -> 'a t * 'a t
val to_array : 'a t -> 'a array
val to_array_map : 'a t -> f:('a -> 'b) -> 'b array
