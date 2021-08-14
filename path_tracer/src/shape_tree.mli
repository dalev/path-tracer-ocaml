include module type of Shape_tree_intf
module Make (L : Leaf) : S with type elt := L.elt and type elt_hit := L.elt_hit

module Array_leaf (Elt : sig
  type t
  type hit

  val hit_t : hit -> float
  val length_cutoff : int
  val bbox : t -> Bbox.t
  val length : t -> int
  val depth : t -> int
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> hit option
end) : Leaf with type elt = Elt.t and type elt_hit = Elt.hit
