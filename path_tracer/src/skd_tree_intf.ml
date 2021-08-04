open Base

module type Leaf = sig
  type t
  type elt
  type elt_hit

  val elt_hit_t : elt_hit -> float
  val length_cutoff : int
  val of_elts : elt array -> t
  val elt_bbox : elt -> Bbox.t
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> elt_hit option
  val length : t -> int
  val depth : t -> int
end

module type S = sig
  type t
  type elt
  type elt_hit

  val create : elt list -> t
  val depth : t -> int
  val length : t -> int
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> elt_hit option
  val leaf_length_histogram : t -> (int, int) Hashtbl.t
end
