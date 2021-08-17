open! Base
module P3 = Path_tracer.P3
module V3 = Path_tracer.V3
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
    | Leaf of P.t
    | Branch of
        { elt : P.t
        ; lhs : t
        ; rhs : t
        }

  let point p = P3.create ~x:(P.x p) ~y:(P.y p) ~z:(P.z p)

  let search t ~center ~radius2 ~f =
    let rec loop t cmps =
      match t with
      | Empty -> ()
      | Leaf p ->
        let v = V3.of_points ~src:(point p) ~tgt:center in
        if Float.O.(V3.quadrance v <= radius2) then f p
      | Branch { elt; lhs; rhs } ->
        let v = V3.of_points ~src:(point elt) ~tgt:center in
        let q = V3.quadrance v in
        let in_range = Float.O.(q <= radius2) in
        let cmp = fst3 cmps in
        let cmps = rotate cmps in
        if in_range
        then (
          f elt;
          loop lhs cmps;
          loop rhs cmps)
        else (
          (* prune branch that is further away *)
          let c = cmp center elt in
          loop (if c < 0 then lhs else rhs) cmps)
    in
    let cmp_x = make_cmp' P3.x P.x in
    let cmp_y = make_cmp' P3.y P.y in
    let cmp_z = make_cmp' P3.z P.z in
    let cmp_xyz = lex cmp_x cmp_y cmp_z in
    let cmp_yzx = lex cmp_y cmp_z cmp_x in
    let cmp_zxy = lex cmp_z cmp_x cmp_y in
    loop t (cmp_xyz, cmp_yzx, cmp_zxy)
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
    let rec loop (ps, qs, rs) ((cmp_p, cmp_q, cmp_r) as cmps) =
      match Slice.length ps with
      | 0 -> failwith "BUG: Kd_tree.create: slice length = 0"
      | 1 -> Leaf (Slice.get ps 0)
      | 2 -> Branch { elt = Slice.get ps 1; lhs = Leaf (Slice.get ps 0); rhs = Empty }
      | 3 ->
        Branch
          { elt = Slice.get ps 1
          ; lhs = Leaf (Slice.get ps 0)
          ; rhs = Leaf (Slice.get ps 2)
          }
      | _ ->
        let median = lower_median ps in
        let ps_l, ps_r = partition ~median ps cmp_p in
        let qs_l, qs_r = partition ~median qs cmp_q in
        let rs_l, rs_r = partition ~median rs cmp_r in
        let cmps = rotate cmps in
        let lhs = loop (rotate (ps_l, qs_l, rs_l)) cmps in
        let rhs = loop (rotate (ps_r, qs_r, rs_r)) cmps in
        Branch { elt = median; lhs; rhs }
    in
    loop (ps_x, ps_y, ps_z) comparators
  ;;
end
