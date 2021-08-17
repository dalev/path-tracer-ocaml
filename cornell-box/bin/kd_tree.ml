open! Base
module P3 = Path_tracer.P3
module V3 = Path_tracer.V3
module Bbox = Path_tracer.Bbox
module Slice = Path_tracer.Slice

let fst3 (x, _, _) = x
let rotate (x, y, z) = y, z, x

let lex cmp1 cmp2 cmp3 a b =
  let c1 = cmp1 a b in
  if c1 <> 0
  then c1
  else (
    let c2 = cmp2 a b in
    if c2 <> 0 then c2 else cmp3 a b)
;;

let make_cmp' f g a b = Float.compare (f a) (g b)
let make_cmp f a b = make_cmp' f f a b

let lower_median slice =
  let i = (Slice.length slice - 1) / 2 in
  Slice.get slice i
;;

let partition ~median slice cmp =
  let on_lhs elt = cmp elt median < 0 in
  let lhs, rhs = Slice.partition_in_place slice ~on_lhs in
  lhs, Slice.tail rhs
;;

module Make (P : sig
  type t

  val x : t -> float
  val y : t -> float
  val z : t -> float
  val bbox : t -> Bbox.t
end) =
struct
  let cmp_x = make_cmp P.x
  let cmp_y = make_cmp P.y
  let cmp_z = make_cmp P.z
  let cmp_xyz = lex cmp_x cmp_y cmp_z
  let cmp_yzx = lex cmp_y cmp_z cmp_x
  let cmp_zxy = lex cmp_z cmp_x cmp_y
  let comparators = cmp_xyz, cmp_yzx, cmp_zxy

  type t =
    | Empty
    | Leaf of
        { elt : P.t
        ; bbox : Bbox.t
        }
    | Branch of
        { elt : P.t
        ; elt_bbox : Bbox.t
        ; subtree_bbox : Bbox.t
        ; lhs : t
        ; rhs : t
        }

  let point p = P3.create ~x:(P.x p) ~y:(P.y p) ~z:(P.z p)

  let search t ~center ~f =
    let rec loop t =
      match t with
      | Empty -> ()
      | Leaf { elt; bbox } -> if Bbox.mem bbox center then f elt
      | Branch { elt; elt_bbox; subtree_bbox; lhs; rhs } ->
        if Bbox.mem subtree_bbox center
        then (
          if Bbox.mem elt_bbox center then f elt;
          loop lhs;
          loop rhs)
    in
    loop t
  ;;

  let tree_bbox = function
    | Empty -> failwith "tree_bbox: Empty"
    | Leaf { bbox; _ } -> bbox
    | Branch { subtree_bbox; _ } -> subtree_bbox
  ;;

  let create ps =
    let f ~compare =
      let ps = Array.copy ps in
      Array.sort ps ~compare;
      Slice.create ps
    in
    let ps_x = f ~compare:cmp_xyz
    and ps_y = f ~compare:cmp_yzx
    and ps_z = f ~compare:cmp_zxy in
    let tmp_lo = Array.create ~len:(1 + (Array.length ps / 2)) ps.(0) in
    let tmp_hi = Array.create ~len:(1 + (Array.length ps / 2)) ps.(0) in
    let split s ~on_lhs =
      let i = ref 0
      and j = ref 0 in
      Slice.iter s ~f:(fun elt ->
          if on_lhs elt
          then (
            tmp_lo.(!i) <- elt;
            Int.incr i)
          else (
            tmp_hi.(!j) <- elt;
            Int.incr j));
      let lhs, rhs = Slice.split_at s (Slice.length s / 2) in
      assert (Slice.length lhs <= Slice.length rhs);
      for i = 0 to Slice.length lhs - 1 do
        Slice.set lhs i tmp_lo.(i)
      done;
      for i = 0 to Slice.length rhs - 1 do
        Slice.set rhs i tmp_hi.(i)
      done;
      lhs, Slice.tail rhs
    in
    let mk_leaf elt = Leaf { elt; bbox = P.bbox elt } in
    let rec loop (ps, qs, rs) ((prj, _, _) as prjs) =
      match Slice.length ps with
      | 0 -> failwith "BUG: Kd_tree.create: slice length = 0"
      | 1 -> mk_leaf @@ Slice.get ps 0
      | 2 ->
        let elt = Slice.get ps 1 in
        let elt_bbox = P.bbox elt in
        let lhs = mk_leaf @@ Slice.get ps 0 in
        let subtree_bbox = Bbox.union elt_bbox (tree_bbox lhs) in
        Branch { elt; elt_bbox; subtree_bbox; lhs; rhs = Empty }
      | 3 ->
        let elt = Slice.get ps 1 in
        let elt_bbox = P.bbox elt in
        let lhs = mk_leaf @@ Slice.get ps 0 in
        let rhs = mk_leaf @@ Slice.get ps 2 in
        let subtree_bbox =
          Bbox.union elt_bbox (Bbox.union (tree_bbox lhs) (tree_bbox rhs))
        in
        Branch { elt; elt_bbox; subtree_bbox; lhs; rhs }
      | _len ->
        let ps_l, ps_r = Slice.split_at ps @@ ((Slice.length ps - 1) / 2) in
        let median = Slice.get ps_r 0 in
        let ps_r = Slice.tail ps_r in
        let on_lhs elt = Float.( < ) (prj elt) (prj median) in
        let qs_l, qs_r = split qs ~on_lhs in
        let rs_l, rs_r = split rs ~on_lhs in
        let prjs = rotate prjs in
        let lhs = loop (rotate (ps_l, qs_l, rs_l)) prjs in
        let rhs = loop (rotate (ps_r, qs_r, rs_r)) prjs in
        let elt_bbox = P.bbox median in
        let subtree_bbox =
          Bbox.union elt_bbox (Bbox.union (tree_bbox lhs) (tree_bbox rhs))
        in
        Branch { elt = median; elt_bbox; subtree_bbox; lhs; rhs }
    in
    loop (ps_x, ps_y, ps_z) (P.x, P.y, P.z)
  ;;
end
