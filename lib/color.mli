type t

val create : r:float -> g:float -> b:float -> t

val of_v3 : V3.t -> t
(** [of_v3] maps x -> red, y -> green, z -> blue *)
