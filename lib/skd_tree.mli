include module type of Skd_tree_intf
module Make : functor (L : Leaf) -> S with type elt := L.elt
