include module type of Shape_tree_intf
module Make : functor (L : Leaf) -> S with type elt := L.elt and type elt_hit := L.elt_hit
