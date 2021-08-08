open Base
include Shape_tree_intf

let num_bins = 32

module Bshape = struct
  type 'a t =
    { shape : 'a
    ; bbox : Bbox.t
    ; centroid : P3.t
    }

  let bbox t = t.bbox
  let cbox t = Bbox.create ~min:t.centroid ~max:t.centroid
  let centroid t = t.centroid
  let shape t = t.shape

  let create elt_bbox shape =
    let bbox = elt_bbox shape in
    let centroid = Bbox.center bbox in
    { shape; bbox; centroid }
  ;;

  let centroid_bbox bshapes = Slice.map_reduce bshapes ~transform:cbox ~combine:Bbox.union
end

module Bin = struct
  type t =
    { count : int
    ; bounds : Bbox.t option
    }

  let create () = { count = 0; bounds = None }
  let bbox t = t.bounds

  let insert t b =
    let b_box = Bshape.bbox b in
    let bounds =
      Some
        (match t.bounds with
        | None -> b_box
        | Some t_box -> Bbox.union t_box b_box)
    in
    { count = t.count + 1; bounds }
  ;;

  let join_bounds t t' = Bbox.union_opt t.bounds t'.bounds

  let join t t' =
    let bounds = join_bounds t t' in
    { count = t.count + t'.count; bounds }
  ;;

  let join_slice = Slice.reduce_exn ~f:join
end

module Proposal = struct
  type 'a t =
    { cost : float
    ; split_index : int
    ; axis : Axis.t
    ; on_lhs : 'a Bshape.t -> bool
    ; lhs_box : Bbox.t
    ; rhs_box : Bbox.t
    }

  let costI = 1.0
  let costT = 0.25
  let leaf_cost n = costI *. Float.of_int n
  let cost t = t.cost
  let on_lhs t = t.on_lhs
  let lhs_box t = t.lhs_box
  let rhs_box t = t.rhs_box
  let axis t = t.axis

  let candidates bins to_bin axis =
    let total_bbox =
      Array.filter_map bins ~f:Bin.bbox |> Array.reduce_exn ~f:Bbox.union
    in
    let total_area = Bbox.surface_area total_bbox in
    let bins = Slice.create bins in
    List.init (num_bins - 1) ~f:(fun p ->
        let lhs, rhs = Slice.split_at bins (p + 1) in
        let lhs = Bin.join_slice lhs in
        let rhs = Bin.join_slice rhs in
        match Bin.bbox lhs, Bin.bbox rhs with
        | None, _ | _, None -> None
        | Some lhs_box, Some rhs_box ->
          let lhs_area = Float.of_int lhs.Bin.count *. Bbox.surface_area lhs_box in
          let rhs_area = Float.of_int rhs.Bin.count *. Bbox.surface_area rhs_box in
          let on_lhs b = to_bin b <= p in
          let open Float.O in
          let cost = costT + ((lhs_area + rhs_area) * costI / total_area) in
          Some { cost; split_index = p; axis; on_lhs; lhs_box; rhs_box })
    |> List.filter_opt
  ;;

  let compare p1 p2 = Float.compare (cost p1) (cost p2)

  let propose_split_one_axis shapes axis cbox =
    let to_axis = P3.axis axis in
    let to_bin =
      let epsilon = 1e-6 in
      let cb_min = to_axis (Bbox.min cbox) in
      let cb_max = to_axis (Bbox.max cbox) in
      let scale = Float.of_int num_bins *. (1.0 -. epsilon) /. (cb_max -. cb_min) in
      fun b -> Float.to_int (scale *. (to_axis (Bshape.centroid b) -. cb_min))
    in
    let bins = Array.init num_bins ~f:(fun (_ : int) -> Bin.create ()) in
    Slice.iter shapes ~f:(fun s ->
        let j = to_bin s in
        let bin = bins.(j) in
        bins.(j) <- Bin.insert bin s);
    candidates bins to_bin axis |> List.min_elt ~compare
  ;;

  let create shapes =
    let cbbox = Bshape.centroid_bbox shapes in
    let candidates =
      List.filter_map
        Axis.[ X; Y; Z ]
        ~f:(fun axis -> propose_split_one_axis shapes axis cbbox)
    in
    List.min_elt candidates ~compare
  ;;
end

module Make (L : Leaf) : S with type elt := L.elt and type elt_hit := L.elt_hit = struct
  type tree =
    | Leaf of
        { bbox : Bbox.t
        ; leaf : L.t
        }
    | Branch of
        { axis : Axis.t
        ; bbox : Bbox.t
        ; lhs : tree
        ; rhs : tree
        }

  type t = { root : tree }

  let rec tree_cata t ~branch ~leaf =
    match t with
    | Leaf { leaf = l; _ } -> leaf l
    | Branch { lhs; rhs; _ } ->
      branch (tree_cata lhs ~branch ~leaf) (tree_cata rhs ~branch ~leaf)
  ;;

  let cata t ~branch ~leaf = tree_cata t.root ~branch ~leaf
  let length = cata ~branch:( + ) ~leaf:L.length
  let depth = cata ~branch:(fun l r -> 1 + max l r) ~leaf:L.depth

  let iter_leaves t ~f =
    let rec loop t =
      match t with
      | Leaf { leaf = l; _ } -> f l
      | Branch { lhs; rhs; _ } ->
        loop lhs;
        loop rhs
    in
    loop t.root
  ;;

  let leaf_length_histogram t =
    let h = Hashtbl.create (module Int) in
    iter_leaves t ~f:(fun l ->
        let len = L.length l in
        Hashtbl.incr h len);
    h
  ;;

  let intersect_tree t ray ~t_min ~t_max =
    let dir = Ray.direction ray in
    let open Float.O in
    let rec loop t ~t_min ~t_max k =
      match t with
      | Leaf { bbox; leaf = l } ->
        if Bbox.is_hit bbox ray ~t_min ~t_max
        then k @@ L.intersect l ray ~t_min ~t_max
        else k @@ None
      | Branch { bbox; axis; lhs; rhs } ->
        let t_min, t_max = Bbox.hit_range bbox ray ~t_min ~t_max in
        if t_min > t_max
        then k @@ None
        else (
          let t1, t2 = if V3.axis axis dir >= 0.0 then lhs, rhs else rhs, lhs in
          loop t1 ~t_min ~t_max (function
              | None -> loop t2 ~t_min ~t_max k
              | Some elt_hit as t1_result ->
                let t_hit = L.elt_hit_t elt_hit in
                loop t2 ~t_min ~t_max:t_hit (function
                    | Some _ as t2_result -> k t2_result
                    | None -> k t1_result)))
    in
    let k0 x = x in
    loop t ~t_min ~t_max k0
  ;;

  let intersect t ray ~t_min ~t_max = intersect_tree t.root ray ~t_min ~t_max

  let make_leaf bbox shapes =
    Leaf { bbox; leaf = L.of_elts (Slice.to_array_map shapes ~f:Bshape.shape) }
  ;;

  let rec create_tree bbox shapes =
    if Slice.length shapes <= L.length_cutoff
    then make_leaf bbox shapes
    else (
      match Proposal.create shapes with
      | None -> make_leaf bbox shapes
      | Some p ->
        let leaf_cost = Proposal.leaf_cost (Slice.length shapes) in
        let open Float.O in
        if Proposal.cost p > leaf_cost
        then make_leaf bbox shapes
        else (
          let l, r = Slice.partition_in_place shapes ~on_lhs:(Proposal.on_lhs p) in
          let lhs = create_tree (Proposal.lhs_box p) l in
          let rhs = create_tree (Proposal.rhs_box p) r in
          let axis = Proposal.axis p in
          Branch { lhs; rhs; bbox; axis }))
  ;;

  let create elts =
    if List.is_empty elts then failwith "Skd_tree.create: given empty list of shapes";
    let elts =
      Sequence.of_list elts
      |> Sequence.map ~f:(Bshape.create L.elt_bbox)
      |> Sequence.to_array
      |> Slice.create
    in
    let bbox = Slice.map_reduce elts ~transform:Bshape.bbox ~combine:Bbox.union in
    let root = create_tree bbox elts in
    { root }
  ;;
end
