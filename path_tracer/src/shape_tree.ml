open Base
include Shape_tree_intf
module Task = Domainslib.Task

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

  let propose_split_one_axis shapes axis cbbox =
    let num_bins = 32 in
    let to_axis = P3.axis axis in
    let to_bin =
      let epsilon = 1e-6 in
      let cb_min = to_axis (Bbox.min cbbox) in
      let cb_max = to_axis (Bbox.max cbbox) in
      let scale = Float.of_int num_bins *. (1.0 -. epsilon) /. (cb_max -. cb_min) in
      fun b -> Float.to_int (scale *. (to_axis (Bshape.centroid b) -. cb_min))
    in
    let bins = Array.init num_bins ~f:(fun (_ : int) -> Bin.create ()) in
    Slice.iter shapes ~f:(fun s -> Bin.insert bins.(to_bin s) s);
    Bin.populate_bbox_r bins;
    Bin.populate_bbox_l bins;
    candidates bins to_bin axis |> List.min_elt ~compare
  ;;

  let create shapes =
    let cbbox = Bshape.centroid_bbox shapes in
    let candidates =
      List.filter_map Axis.all ~f:(fun axis -> propose_split_one_axis shapes axis cbbox)
    in
    List.min_elt candidates ~compare
  ;;
end

module Make (L : Leaf) : S with type elt := L.elt and type elt_hit := L.elt_hit = struct
  module Tree = struct
    type t =
      | Leaf of
          { bbox : Bbox.t
          ; leaf : L.t
          }
      | Branch of
          { axis : V3.t -> float
          ; bbox : Bbox.t
          ; lhs : t
          ; rhs : t
          }

    let rec cata t ~branch ~leaf =
      match t with
      | Leaf { leaf = l; _ } -> leaf l
      | Branch { lhs; rhs; _ } -> branch (cata lhs ~branch ~leaf) (cata rhs ~branch ~leaf)
    ;;

    let iter_leaves t ~f = cata t ~branch:(fun () () -> ()) ~leaf:f

    let make_leaf bbox shapes =
      Leaf { bbox; leaf = L.of_elts (Slice.to_array_map shapes ~f:Bshape.shape) }
    ;;

    let create pool bbox shapes =
      let rec loop bbox shapes ~depth =
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
              let axis = V3.axis @@ Proposal.axis p in
              let rhs_box = Proposal.rhs_box p in
              let spawn_rhs = Int.O.(Slice.length r >= 5000 && depth < 8) in
              let depth = Int.O.(depth + 1) in
              let do_rhs =
                let thunk () = loop rhs_box r ~depth in
                if not spawn_rhs
                then thunk
                else (
                  let t = Task.async pool thunk in
                  fun () -> Task.await pool t)
              in
              let lhs = loop (Proposal.lhs_box p) l ~depth in
              let rhs = do_rhs () in
              Branch { lhs; rhs; bbox; axis }))
      in
      loop bbox shapes ~depth:0
    ;;

    let intersect t ray ~t_min ~t_max =
      let dir = Ray.direction ray in
      let rec loop t ~t_min ~t_max =
        let open Float.O in
        match t with
        | Leaf { bbox; leaf = l } ->
          let t_min, t_max = Bbox.hit_range bbox ray ~t_min ~t_max in
          if t_min <= t_max then L.intersect l ray ~t_min ~t_max else None
        | Branch { bbox; axis; lhs; rhs } ->
          let t_min, t_max = Bbox.hit_range bbox ray ~t_min ~t_max in
          if t_min > t_max
          then None
          else (
            let t1, t2 = if axis dir >= 0.0 then lhs, rhs else rhs, lhs in
            match loop t1 ~t_min ~t_max with
            | None -> loop t2 ~t_min ~t_max
            | Some elt_hit as t1_result ->
              let t_hit = L.elt_hit_t elt_hit in
              (match loop t2 ~t_min ~t_max:t_hit with
              | Some _ as t2_result -> t2_result
              | None -> t1_result))
      in
      loop t ~t_min ~t_max
    ;;
  end

  type t = { root : Tree.t }

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

  let chunks slice =
    let len = Slice.length slice / 8 in
    let rec loop slice chunks =
      if Slice.length slice <= len
      then slice :: chunks
      else (
        let pre, suf = Slice.split_at slice len in
        loop suf (pre :: chunks))
    in
    loop slice []
  ;;

  let create elts =
    if List.is_empty elts
    then failwith "Shape_tree.create: expected non-empty list of shapes";
    let elts =
      Sequence.of_list elts
      |> Sequence.map ~f:(Bshape.create L.elt_bbox)
      |> Sequence.to_array
      |> Slice.create
    in
    let pool = Task.setup_pool ~num_additional_domains:7 in
    let bbox =
      let tasks =
        List.map (chunks elts) ~f:(fun s ->
            Task.async pool (fun () ->
                Slice.map_reduce s ~transform:Bshape.bbox ~combine:Bbox.union))
      in
      List.map tasks ~f:(Task.await pool) |> List.reduce_exn ~f:Bbox.union
    in
    let root = Tree.create pool bbox elts in
    Task.teardown_pool pool;
    { root }
  ;;
end
