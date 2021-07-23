open Base

module Array = struct
  include Array

  let split_at a n = partitioni_tf a ~f:(fun i _ -> i < n)
end

module type Leaf = sig
  type t
  type elt

  val length_cutoff : int
  val of_elts : elt array -> t
  val elt_bbox : elt -> Bbox.t
  val hit : elt -> float -> Ray.t -> Hit.t
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> (float * elt) option
  val length : t -> int
  val depth : t -> int
end

module type S = sig
  type t
  type leaf_elt

  val create : leaf_elt list -> t
  val depth : t -> int
  val length : t -> int

  (* CR dalev: store top-level bbox to compute initial t-bounds *)
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> Hit.t option
end

module Make (L : Leaf) : S with type leaf_elt := L.elt = struct
  module Bshape = struct
    type t = {shape: L.elt; bbox: Bbox.t; centroid: P3.t}

    let bbox t = t.bbox
    let cbox t = Bbox.create ~min:t.centroid ~max:t.centroid
    let centroid t = t.centroid
    let shape t = t.shape

    let create shape =
      let bbox = L.elt_bbox shape in
      let centroid = Bbox.center bbox in
      {shape; bbox; centroid}

    let centroid_bbox bshapes =
      let init = cbox (Slice.get bshapes 0) in
      Slice.fold bshapes ~init ~f:(fun acc b -> Bbox.union acc (cbox b))
  end

  type t =
    | Leaf of L.t
    | Branch of {to_axis: V3.t -> float; lhs_clip: float; rhs_clip: float; lhs: t; rhs: t}

  let rec tree_cata t ~branch ~leaf =
    match t with
    | Leaf l -> leaf l
    | Branch {lhs; rhs; _} ->
        branch (tree_cata lhs ~branch ~leaf) (tree_cata rhs ~branch ~leaf)

  let length = tree_cata ~branch:( + ) ~leaf:L.length
  let depth = tree_cata ~branch:(fun l r -> 1 + max l r) ~leaf:L.depth

  let intersect t ray ~t_min:t_enter ~t_max:t_leave =
    let d_inv = Ray.direction_inv ray in
    let o = P3.to_v3 (Ray.origin ray) in
    let open Float.O in
    let solve_ray_plane to_axis p = (p - to_axis o) * to_axis d_inv in
    let rec loop t found t_found t_enter t_leave k =
      if t_found < t_enter then
        k t_found found
      else
        match t with
        | Leaf l -> (
          match L.intersect l ray ~t_min:t_enter ~t_max:t_found with
          | Some (t_hit, elt_hit) -> k t_hit (Some elt_hit)
          | None -> k t_found found )
        | Branch {to_axis; lhs_clip; rhs_clip; lhs; rhs} ->
            let t_lhs = solve_ray_plane to_axis lhs_clip in
            let t_rhs = solve_ray_plane to_axis rhs_clip in
            let t_clip1, subtree1, t_clip2, subtree2 =
              if to_axis d_inv >= 0.0 then
                (t_lhs, lhs, t_rhs, rhs)
              else
                (t_rhs, rhs, t_lhs, lhs) in
            let k =
              if t_clip2 <= t_leave then
                let t_enter = Float.max t_enter t_clip2 in
                fun t_found found -> loop subtree2 found t_found t_enter t_leave k
              else
                k in
            if t_enter <= t_clip1 then
              let t_leave = Float.min t_leave t_clip1 in
              loop subtree1 found t_found t_enter t_leave k
            else
              k t_found found in
    let k0 t_hit found =
      match found with None -> None | Some item -> Some (L.hit item t_hit ray) in
    loop t None t_leave t_enter t_leave k0

  (* == Tree builder using Binned SAH == *)

  module Bin = struct
    type t = {count: int; bounds: Bbox.t option}

    let create () = {count= 0; bounds= None}
    let bbox t = t.bounds

    let scaled_area t =
      Option.map t.bounds ~f:(fun bbox -> Float.of_int t.count *. Bbox.surface_area bbox)

    let insert t b =
      let b_box = Bshape.bbox b in
      let bounds =
        Some (match t.bounds with None -> b_box | Some t_box -> Bbox.union t_box b_box)
      in
      {count= t.count + 1; bounds}

    let join_bounds t t' = Bbox.union_opt t.bounds t'.bounds

    let join t t' =
      let bounds = join_bounds t t' in
      {count= t.count + t'.count; bounds}
  end

  let join_bins = Array.reduce_exn ~f:Bin.join
  let num_bins = 32
  let costI = 1.0
  let costT = 0.25
  let leaf_cost n = costI *. Float.of_int n

  module Proposal = struct
    type t = {cost: float; split_index: int; axis: Axis.t; on_lhs: Bshape.t -> bool}

    let cost t = t.cost
    let on_lhs t = t.on_lhs
    let axis t = t.axis

    let candidates bins to_bin axis =
      let total_bbox =
        Array.filter_map bins ~f:Bin.bbox |> Array.reduce_exn ~f:Bbox.union in
      let total_area = Bbox.surface_area total_bbox in
      List.init (num_bins - 1) ~f:(fun p ->
          let lhs, rhs = Array.split_at bins (p + 1) in
          let f = Fn.compose Bin.scaled_area join_bins in
          match (f lhs, f rhs) with
          | None, _ | _, None -> None
          | Some lhs_area, Some rhs_area ->
              let on_lhs b = to_bin b <= p in
              let open Float.O in
              let cost = costT + ((lhs_area + rhs_area) * costI / total_area) in
              Some {cost; split_index= p; axis; on_lhs} )
      |> List.filter_opt

    let compare p1 p2 = Float.compare (cost p1) (cost p2)
  end

  let propose_split_one_axis shapes axis cbox =
    let to_axis = P3.axis axis in
    let to_bin =
      let epsilon = 1e-6 in
      let cb_min = to_axis (Bbox.min cbox) in
      let cb_max = to_axis (Bbox.max cbox) in
      let scale = Float.of_int num_bins *. (1.0 -. epsilon) /. (cb_max -. cb_min) in
      fun b -> Float.to_int (scale *. (to_axis (Bshape.centroid b) -. cb_min)) in
    let bins = Array.init num_bins ~f:(fun (_ : int) -> Bin.create ()) in
    Slice.iter shapes ~f:(fun s ->
        let j = to_bin s in
        let bin = bins.(j) in
        bins.(j) <- Bin.insert bin s ) ;
    Proposal.candidates bins to_bin axis |> List.min_elt ~compare:Proposal.compare

  let propose_split shapes =
    let cbbox = Bshape.centroid_bbox shapes in
    let candidates =
      List.filter_map
        Axis.[X; Y; Z]
        ~f:(fun axis -> propose_split_one_axis shapes axis cbbox) in
    List.min_elt candidates ~compare:Proposal.compare

  let make_leaf shapes = Leaf (L.of_elts (Slice.to_array_map shapes ~f:Bshape.shape))

  let rec create' shapes =
    if Slice.length shapes <= L.length_cutoff then
      make_leaf shapes
    else
      match propose_split shapes with
      | None -> make_leaf shapes
      | Some p ->
          let leaf_cost = leaf_cost (Slice.length shapes) in
          let open Float.O in
          if Proposal.cost p <= leaf_cost then
            let l, r = Slice.partition_in_place shapes ~on_lhs:(Proposal.on_lhs p) in
            let axis = Proposal.axis p in
            let to_axis = P3.axis axis in
            let lhs_clip =
              Slice.fold l ~init:Float.neg_infinity ~f:(fun acc b ->
                  Float.max acc (to_axis (Bbox.max (Bshape.bbox b))) ) in
            let rhs_clip =
              Slice.fold r ~init:Float.infinity ~f:(fun acc b ->
                  Float.min acc (to_axis (Bbox.min (Bshape.bbox b))) ) in
            Branch
              {lhs= create' l; rhs= create' r; lhs_clip; rhs_clip; to_axis= V3.axis axis}
          else
            make_leaf shapes

  let create elts =
    if List.is_empty elts then failwith "Skd_tree.create: given empty list of shapes" ;
    Sequence.of_list elts |> Sequence.map ~f:Bshape.create |> Sequence.to_array
    |> Slice.create |> create'
end
