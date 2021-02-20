open Base

type t = Leaf of Shape.t | Branch of Bbox.t * t * t

module Bshape = struct
  type t = {shape: Shape.t; bbox: Bbox.t; centroid: P3.t}

  let bbox t = t.bbox
  let cbox t = Bbox.create ~min:t.centroid ~max:t.centroid
  let centroid t = t.centroid
  let leaf t = Leaf t.shape

  let create shape =
    let bbox = Shape.bbox shape in
    let centroid = Bbox.center bbox in
    {shape; bbox; centroid}
end

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

  let join_bounds t t' =
    match (t.bounds, t'.bounds) with
    | None, other | other, None -> other
    | Some b, Some b' -> Some (Bbox.union b b')

  let join t t' =
    let bounds = join_bounds t t' in
    {count= t.count + t'.count; bounds}
end

module Array = struct
  include Array

  let split_at a n = partitioni_tf a ~f:(fun i _ -> i < n)
end

let num_bins = 32
let costI = 1.0
let costT = 0.25
let leaf_cost n = costI *. Float.of_int n

let centroid_bbox bshapes =
  let init = Bshape.cbox (Slice.get bshapes 0) in
  Slice.fold bshapes ~init ~f:(fun acc b -> Bbox.union acc (Bshape.cbox b))

let create shapes =
  let bins = Array.init num_bins ~f:(fun (_ : int) -> Bin.create ()) in
  let clear_bins () =
    Array.iteri bins ~f:(fun i (_ : Bin.t) -> bins.(i) <- Bin.create ()) in
  let join_bins = Array.reduce_exn ~f:Bin.join in
  let rec loop shapes =
    match Slice.length shapes with
    | 1 -> Bshape.leaf (Slice.get shapes 0)
    | 2 ->
        let s = Slice.get shapes 0 in
        let s' = Slice.get shapes 1 in
        let box = Bbox.union (Bshape.bbox s) (Bshape.bbox s') in
        Branch (box, Bshape.leaf s, Bshape.leaf s')
    | _ -> (
        clear_bins () ;
        let cbox = centroid_bbox shapes in
        let longest_axis = Bbox.longest_axis cbox in
        let to_axis v = P3.axis v longest_axis in
        let to_bin =
          let epsilon = 1e-6 in
          let cb_min = to_axis (Bbox.min cbox) in
          let cb_max = to_axis (Bbox.max cbox) in
          let scale = Float.of_int num_bins *. (1.0 -. epsilon) /. (cb_max -. cb_min) in
          fun b -> Float.to_int (scale *. (to_axis (Bshape.centroid b) -. cb_min)) in
        Slice.iter shapes ~f:(fun s ->
            let j = to_bin s in
            let bin = bins.(j) in
            bins.(j) <- Bin.insert bin s ) ;
        let total_bbox =
          Array.filter_map bins ~f:Bin.bbox |> Array.reduce_exn ~f:Bbox.union in
        let best_partition =
          let total_area = Bbox.surface_area total_bbox in
          List.init (num_bins - 1) ~f:(fun p ->
              let lhs, rhs = Array.split_at bins (p + 1) in
              let f = Fn.compose Bin.scaled_area join_bins in
              match (f lhs, f rhs) with
              | None, _ | _, None -> None
              | Some lhs_area, Some rhs_area ->
                  let open Float.O in
                  let cost = costT + ((lhs_area + rhs_area) * costI / total_area) in
                  Some (p, cost) )
          |> List.filter_opt
          |> List.min_elt ~compare:(fun (_, cost) (_, cost') -> Float.compare cost cost')
        in
        match best_partition with
        | None ->
            Printf.failwithf "to do: Bvh build: all shapes assigned to same bin: len = %d"
              (Slice.length shapes) ()
        | Some (p, split_cost) ->
            let leaf_cost = leaf_cost (Slice.length shapes) in
            if Float.( < ) leaf_cost split_cost then failwith "to do: Bvh multi-leaf"
            else
              let lhs, rhs = Slice.partition_in_place shapes p to_bin in
              Branch (total_bbox, loop lhs, loop rhs) ) in
  match shapes with
  | [] -> failwith "BUG: Bvh.create got empty list of shapes"
  | hd :: _ ->
      let b = Bshape.create hd in
      let a = Array.create ~len:(List.length shapes) b in
      List.iteri shapes ~f:(fun i s -> a.(i) <- Bshape.create s) ;
      loop (Slice.create a)

let intersect t ray ~t_min ~t_max =
  let rec loop t t_max =
    match t with
    | Leaf s -> (
      match Shape.intersect s ray ~t_min ~t_max with
      | None -> None
      | Some t_hit -> Some (t_hit, s) )
    | Branch (bbox, lhs, rhs) ->
        if Bbox.is_hit bbox ray ~t_min ~t_max then
          match loop lhs t_max with
          | None -> loop rhs t_max
          | Some (t_hit, _) as some_lhs -> (
            match loop rhs t_hit with None -> some_lhs | Some _ as some_rhs -> some_rhs )
        else None in
  match loop t t_max with
  | None -> None
  | Some (t_hit, shape) -> Some (Hit.create ~t_hit shape ray)
