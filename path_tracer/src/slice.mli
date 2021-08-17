type 'a t

val create : 'a array -> 'a t
val length : 'a t -> int
val base_index : 'a t -> int -> int
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val iter : 'a t -> f:('a -> unit) -> unit
val map_reduce : 'a t -> transform:('a -> 'b) -> combine:('b -> 'b -> 'b) -> 'b
val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a
val split_at : 'a t -> int -> 'a t * 'a t
val tail : 'a t -> 'a t
val partition_in_place : 'a t -> on_lhs:('a -> bool) -> 'a t * 'a t
val to_array : 'a t -> 'a array
val to_array_map : 'a t -> f:('a -> 'b) -> 'b array
