open Base

type t

val create : Camera.t -> Shape.t list -> background:(Ray.t -> Color.t) -> t

val path_tracer :
     t
  -> (cx:float -> cy:float -> int -> Low_discrepancy_sequence.Sample.t -> Color.t)
     Staged.t
