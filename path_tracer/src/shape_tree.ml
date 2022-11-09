open Base
include Shape_tree_intf

module Bshape = struct
  type 'a t =
    { shape : 'a
    ; bbox : Bbox.t
    ; centroid : P3.t
    }

  let bbox t = t.bbox
  let centroid t = t.centroid
  let shape t = t.shape

  let create elt_bbox shape =
    let bbox = elt_bbox shape in
    let centroid = Bbox.center bbox in
    { shape; bbox; centroid }
  ;;

  let centroid_bbox bshapes =
    let transform t = Bbox.create ~min:t.centroid ~max:t.centroid in
    Slice.map_reduce bshapes ~transform ~combine:Bbox.union
  ;;
end

module Bin = struct
  type t =
    { mutable count : int
    ; mutable bounds : Bbox.t option
    ; mutable bbox_l : Bbox.t option
    ; mutable bbox_r : Bbox.t option
    }

  let create () = { count = 0; bounds = None; bbox_l = None; bbox_r = None }
  let bbox t = t.bounds
  let bbox_l t = t.bbox_l
  let bbox_r t = t.bbox_r
  let count t = t.count

  let insert t b =
    let b_box = Bshape.bbox b in
    let bounds =
      Some
        (match t.bounds with
         | None -> b_box
         | Some t_box -> Bbox.union t_box b_box)
    in
    t.bounds <- bounds;
    t.count <- t.count + 1
  ;;

  let populate_bbox_r bins =
    let last_bin = Array.last bins in
    last_bin.bbox_r <- bbox last_bin;
    for j = Array.length bins - 2 downto 0 do
      let bin_j = bins.(j) in
      bin_j.bbox_r <- Bbox.union_opt (bbox bin_j) (bbox_r bins.(j + 1))
    done
  ;;

  let populate_bbox_l bins =
    let first_bin = bins.(0) in
    first_bin.bbox_l <- bbox first_bin;
    for j = 1 to Array.length bins - 1 do
      let bin_j = bins.(j) in
      bin_j.bbox_l <- Bbox.union_opt (bbox bin_j) (bbox_l bins.(j - 1))
    done
  ;;
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
      match Bin.bbox_l (Array.last bins) with
      | None -> failwith "BUG: candidates: bbox_l is not populated"
      | Some b -> b
    in
    let total_area = Bbox.surface_area total_bbox in
    let total_count = Array.sum (module Int) bins ~f:Bin.count in
    let rec loop p candidates ~n_left =
      if p >= Array.length bins - 1
      then candidates
      else (
        let lhs = bins.(p) in
        let rhs = bins.(p + 1) in
        let lhs_count = n_left + Bin.count lhs in
        let rhs_count = total_count - lhs_count in
        let n_left = lhs_count in
        match Bin.bbox_l lhs, Bin.bbox_r rhs with
        | None, _ | _, None -> loop (p + 1) candidates ~n_left
        | Some lhs_box, Some rhs_box ->
          let lhs_area = Float.of_int lhs_count *. Bbox.surface_area lhs_box in
          let rhs_area = Float.of_int rhs_count *. Bbox.surface_area rhs_box in
          let on_lhs b = to_bin b <= p in
          let cost = Float.O.(costT + ((lhs_area + rhs_area) * costI / total_area)) in
          let c = { cost; split_index = p; axis; on_lhs; lhs_box; rhs_box } in
          loop (p + 1) (c :: candidates) ~n_left)
    in
    loop 0 [] ~n_left:0
  ;;

  let compare p1 p2 = Float.compare (cost p1) (cost p2)

  let propose_split_one_axis ~num_bins shapes axis cbbox =
    let to_axis = P3.axis axis in
    let open Float.O in
    let epsilon = 1e-6 in
    let cb_min = to_axis (Bbox.min cbbox) in
    let cb_max = to_axis (Bbox.max cbbox) in
    let scale = Float.of_int num_bins * (1.0 - epsilon) / (cb_max - cb_min) in
    if not (Float.is_finite scale)
    then None
    else (
      let to_bin b = Float.to_int (scale * (to_axis (Bshape.centroid b) - cb_min)) in
      let bins = Array.init num_bins ~f:(fun (_ : int) -> Bin.create ()) in
      Slice.iter shapes ~f:(fun s -> Bin.insert bins.(to_bin s) s);
      Bin.populate_bbox_r bins;
      Bin.populate_bbox_l bins;
      candidates bins to_bin axis |> List.min_elt ~compare)
  ;;

  let create ~num_bins shapes =
    let cbbox = Bshape.centroid_bbox shapes in
    List.min_elt ~compare
    @@ List.filter_map Axis.all ~f:(fun axis ->
         propose_split_one_axis ~num_bins shapes axis cbbox)
  ;;
end

module Make (L : Leaf) = struct
  type leaf = L.t

  module Tree = struct
    type t' =
      | Leaf of leaf
      | Branch of
          { axis : bool * bool * bool -> bool
          ; lhs : t
          ; rhs : t
          }

    and t = Bbox.t * t'

    let bbox : t -> Bbox.t = fst

    let rec cata t ~branch ~leaf =
      match snd t with
      | Leaf l -> leaf l
      | Branch { lhs; rhs; _ } -> branch (cata lhs ~branch ~leaf) (cata rhs ~branch ~leaf)
    ;;

    let iter_leaves t ~f = cata t ~branch:(fun () () -> ()) ~leaf:f

    let make_leaf bbox shapes =
      bbox, Leaf (L.of_elts (Slice.to_array_map shapes ~f:Bshape.shape))
    ;;

    let create ~num_bins bbox shapes =
      let rec loop bbox shapes =
        match Proposal.create ~num_bins shapes with
        | None -> make_leaf bbox shapes
        | Some p ->
          let leaf_cost = Proposal.leaf_cost (Slice.length shapes) in
          if (Float.O.(Proposal.cost p >= leaf_cost)
             && Slice.length shapes <= L.length_cutoff)
             || Slice.length shapes <= 4
          then make_leaf bbox shapes
          else (
            let l, r = Slice.partition_in_place shapes ~on_lhs:(Proposal.on_lhs p) in
            let axis = Proposal.axis p in
            let lhs_box, rhs_box = Proposal.lhs_box p, Proposal.rhs_box p in
            let lhs = loop lhs_box l in
            let rhs = loop rhs_box r in
            bbox, Branch { lhs; rhs; axis = Axis.tuple_selector axis })
      in
      loop bbox shapes
    ;;

    let intersect t ray ~t_min ~t_max =
      let dir = Ray.direction ray in
      let dirs = Float.O.(V3.x dir >= 0.0, V3.y dir >= 0.0, V3.z dir >= 0.0) in
      let rec loop t ~t_min ~t_max =
        let open Float.O in
        let t_min, t_max = Bbox.hit_range (bbox t) ray ~t_min ~t_max in
        if t_min > t_max
        then None
        else begin
          match snd t with
          | Leaf l -> L.intersect l ray ~t_min ~t_max
          | Branch { axis; lhs; rhs } ->
            let t1, t2 = if axis dirs then lhs, rhs else rhs, lhs in
            (match loop t1 ~t_min ~t_max with
             | None -> loop t2 ~t_min ~t_max
             | Some elt_hit as t1_result ->
               let t_hit = L.elt_hit_t elt_hit in
               (match loop t2 ~t_min ~t_max:t_hit with
                | Some _ as t2_result -> t2_result
                | None -> t1_result))
        end
      in
      loop t ~t_min ~t_max
    ;;

    let fold_neighbors t point ~init ~f =
      let rec loop t acc k =
        let bbox = bbox t in
        match snd t with
        | Leaf leaf -> k @@ if Bbox.mem bbox point then f acc leaf else acc
        | Branch { lhs; rhs; axis = _ } ->
          if Bbox.mem bbox point then loop lhs acc (fun acc -> loop rhs acc k) else k acc
      in
      loop t init Fn.id
    ;;
  end

  type t = { root : Tree.t }

  let bbox t = Tree.bbox t.root
  let cata t = Tree.cata t.root
  let length = cata ~branch:( + ) ~leaf:L.length
  let depth = cata ~branch:(fun l r -> 1 + max l r) ~leaf:L.depth

  let leaf_length_histogram t =
    let h = Hashtbl.create (module Int) in
    Tree.iter_leaves t.root ~f:(fun l ->
      let len = L.length l in
      Hashtbl.incr h len);
    h
  ;;

  let intersect t ray ~t_min ~t_max = Tree.intersect t.root ray ~t_min ~t_max
  let fold_neighbors t point ~init ~f = Tree.fold_neighbors t.root point ~init ~f

  let create ?(num_bins = 32) elts =
    assert (num_bins >= 4);
    if List.is_empty elts
    then failwith "Shape_tree.create: expected non-empty list of shapes";
    let elts = Array.of_list_map elts ~f:(Bshape.create L.elt_bbox) |> Slice.create in
    let bbox =
      let slice_bbox = Slice.map_reduce ~transform:Bshape.bbox ~combine:Bbox.union in
      slice_bbox elts
    in
    let root = Tree.create ~num_bins bbox elts in
    { root }
  ;;
end

module Array_leaf (Elt : sig
  type t
  type hit

  val hit_t : hit -> float
  val length_cutoff : int
  val bbox : t -> Bbox.t
  val length : t -> int
  val depth : t -> int
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> hit option
end) : Leaf with type t = Elt.t array and type elt = Elt.t and type elt_hit = Elt.hit =
struct
  type elt = Elt.t
  type t = Elt.t array
  type elt_hit = Elt.hit

  let elt_hit_t = Elt.hit_t
  let length_cutoff = Elt.length_cutoff

  let of_elts es =
    assert (not (Array.is_empty es));
    es
  ;;

  let elt_bbox = Elt.bbox
  let length t = Array.sum (module Int) t ~f:Elt.length

  let depth t =
    match Array.max_elt (Array.map t ~f:Elt.depth) ~compare:Int.compare with
    | Some d -> 1 + d
    | None -> failwith "BUG: array leaf is empty"
  ;;

  let intersect t ray ~t_min ~t_max =
    let t_max = ref t_max in
    let item = ref None in
    for i = 0 to Array.length t - 1 do
      let s = t.(i) in
      match Elt.intersect s ray ~t_min ~t_max:!t_max with
      | None -> ()
      | Some hit as some_hit ->
        item := some_hit;
        t_max := Elt.hit_t hit
    done;
    !item
  ;;
end
