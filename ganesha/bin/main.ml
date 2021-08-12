open Base
open Stdio
open Path_tracer
open Ply_format
module Bigstring = Base_bigstring
module FArray = Caml.Float.Array
module Common_args = Render_command.Args

module Args = struct
  type t =
    { common : Common_args.t
    ; ganesha_ply : string
    ; stop_after_bvh : bool
    }

  let parse () =
    let stop_after_bvh = ref false in
    let ganesha_ply = ref "ganesha.ply" in
    let specs =
      Caml.Arg.
        [ "-ganesha-ply", Set_string ganesha_ply, "<file> path to ganesha.ply"
        ; "-stop-after-bvh", Set stop_after_bvh, "stop after BVH build"
        ]
    in
    let common = Common_args.parse ~specs () in
    { common; ganesha_ply = !ganesha_ply; stop_after_bvh = !stop_after_bvh }
  ;;
end

let camera aspect =
  let eye = P3.create ~x:328.0 ~y:40.282 ~z:245.0 in
  let target = P3.create ~x:328.0 ~y:10.0 ~z:0.0 in
  let up = V3.create ~x:(-0.00212272) ~y:0.998201 ~z:(-0.0599264) in
  Camera.create ~eye ~target ~up ~aspect ~vertical_fov_deg:30.0
;;

let background =
  let escape_color = Color.create ~r:0.5 ~g:0.7 ~b:1.0 in
  fun ray ->
    let d = V3.normalize (Ray.direction ray) in
    let t = 0.5 *. (V3.dot d V3.unit_y +. 1.0) in
    Color.lerp t Color.white escape_color
;;

module Mesh = struct
  type t =
    { xs : floatarray
    ; ys : floatarray
    ; zs : floatarray
    ; faces : int array array
    }

  let floats_exn : Ply.Data.Column.t -> floatarray = function
    | Floats fs -> fs
    | Ints _ | Rows _ -> failwith "floats_exn: expected Floats"
  ;;

  let create ply camera =
    let d = Ply.data ply in
    let v = Map.find_exn d "vertex" in
    let vi = Map.find_exn d "vertex_indices" in
    let faces =
      match Map.find vi "rows" with
      | None -> failwith "BUG: vertex_indices has no rows property"
      | Some (Floats _) -> failwith "got Floats, expected Rows"
      | Some (Ints _) -> failwith "got Ints, expected Rows"
      | Some (Rows faces) -> faces
    in
    let f = Fn.compose floats_exn (Map.find_exn v) in
    let fst3 (a, _, _) = a in
    let map3 (a, b, c) ~f = f a, f b, f c in
    let flds = "x", "y", "z" in
    let xyzs' = map3 flds ~f in
    let lengths = map3 xyzs' ~f:FArray.length in
    (let lx, ly, lz = lengths in
     assert (lx = ly);
     assert (ly = lz));
    let xs, ys, zs = map3 lengths ~f:FArray.create in
    let p3 (x, y, z) = P3.create ~x ~y ~z in
    for i = 0 to fst3 lengths - 1 do
      let xyz = p3 @@ map3 xyzs' ~f:(Fn.flip FArray.get i) in
      let { P3.x; y; z } = Camera.transform camera xyz in
      FArray.set xs i x;
      FArray.set ys i y;
      FArray.set zs i z
    done;
    { xs; ys; zs; faces }
  ;;
end

module Tri_hit = struct
  type t =
    { t_hit : float
    ; u : float
    ; v : float
    ; pt : P3.t
    ; g_normal : V3.t
    }

  let t_hit t = t.t_hit
end

module Make_triangle (M : sig
  val mesh : Mesh.t
end) =
struct
  type t = int (* index into faces *)

  let xs = M.mesh.Mesh.xs
  let ys = M.mesh.Mesh.ys
  let zs = M.mesh.Mesh.zs
  let faces = M.mesh.Mesh.faces

  let vertex t j =
    let i = faces.(t).(j) in
    let x = FArray.get xs i in
    let y = FArray.get ys i in
    let z = FArray.get zs i in
    P3.create ~x ~y ~z
  ;;

  let bbox t =
    let a = vertex t 0 in
    let b = vertex t 1 in
    let c = vertex t 2 in
    let lo = P3.map2 ~f:Float.min in
    let hi = P3.map2 ~f:Float.max in
    Bbox.create ~min:(lo (lo a b) c) ~max:(hi (hi a b) c)
  ;;

  let intersect t r ~t_min ~t_max =
    let epsilon = 1e-6 in
    let a = vertex t 0
    and b = vertex t 1
    and c = vertex t 2 in
    let e1 = V3.of_points ~tgt:b ~src:a in
    let e2 = V3.of_points ~tgt:c ~src:a in
    let dir = Ray.direction r in
    let pvec = V3.cross dir e2 in
    let det = V3.dot e1 pvec in
    let open Float.O in
    if Float.abs det < epsilon
    then None
    else (
      let det_inv = 1.0 / det in
      let tvec = V3.of_points ~tgt:(Ray.origin r) ~src:a in
      let u = det_inv * V3.dot tvec pvec in
      let qvec = V3.cross tvec e1 in
      let v = det_inv * V3.dot dir qvec in
      if 0.0 <= u && u <= 1.0 && 0.0 <= v && u + v <= 1.0
      then (
        let t_hit = det_inv * V3.dot e2 qvec in
        if t_min <= t_hit && t_hit <= t_max
        then (
          let w = 1.0 - u - v in
          let pt = P3.Infix.(P3.scale a w + P3.scale b u + P3.scale c v) in
          let g_normal = V3.normalize @@ V3.cross e1 e2 in
          Some { Tri_hit.t_hit; u; v; pt; g_normal })
        else None)
      else None)
  ;;
end

module Make_array_leaf (Elt : sig
  type t

  val bbox : t -> Bbox.t
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> Tri_hit.t option
end) : Shape_tree.Leaf with type elt = Elt.t and type elt_hit = Tri_hit.t = struct
  type t = Elt.t array
  type elt = Elt.t
  type elt_hit = Tri_hit.t

  let elt_hit_t = Tri_hit.t_hit
  let length_cutoff = 8
  let of_elts = Fn.id
  let elt_bbox = Elt.bbox
  let depth _ = 0
  let length = Array.length

  let intersect t ray ~t_min ~t_max =
    let t_max = ref t_max in
    let item = ref None in
    for i = 0 to Array.length t - 1 do
      let s = t.(i) in
      match Elt.intersect s ray ~t_min ~t_max:!t_max with
      | None -> ()
      | Some tri_hit as some_tri_hit ->
        item := some_tri_hit;
        t_max := Tri_hit.t_hit tri_hit
    done;
    !item
  ;;
end

let load_ply_exn path =
  let shared = false in
  let fd = Unix.openfile path [ O_RDONLY ] 0o600 in
  let (bs : Bigstring.t) =
    Bigarray.array1_of_genarray
    @@ Unix.map_file fd Bigarray.char Bigarray.c_layout shared [| -1 |]
  in
  Exn.protect
    ~f:(fun () -> Or_error.ok_exn @@ Ply.of_bigstring bs)
    ~finally:(fun () -> Unix.close fd)
;;

let main { Args.common; ganesha_ply; stop_after_bvh } =
  let { Common_args.width; height; _ } = common in
  let camera = camera (width // height) in
  let ganesha_ply = load_ply_exn ganesha_ply in
  let mesh = Mesh.create ganesha_ply camera in
  let module Triangle =
    Make_triangle (struct
      let mesh = mesh
    end)
  in
  let module Leaf = Make_array_leaf (Triangle) in
  let module Triangles = Shape_tree.Make (Leaf) in
  let module Leaf_lengths = struct
    type s =
      { size : int
      ; count : int
      }
    [@@deriving sexp_of]

    type t = s list [@@deriving sexp_of]

    let create tree =
      Triangles.leaf_length_histogram tree
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (size, count) -> { size; count })
      |> List.sort ~compare:(fun a b -> Int.compare a.size b.size)
    ;;
  end
  in
  let triangles : Triangle.t list = List.init (Array.length mesh.Mesh.faces) ~f:Fn.id in
  printf "dim = %d x %d;\n" width height;
  printf "#triangles = %d\n%!" (List.length triangles);
  let elapsed, tree =
    Render_command.with_elapsed_time (fun () -> Triangles.create triangles)
  in
  printf
    "tree depth = %d\nbuild time = %.3f ms\nreachable words = %d\n%!"
    (Triangles.depth tree)
    (Float.of_int63 elapsed *. 1e-6)
    (Caml.Obj.reachable_words @@ Caml.Obj.repr tree);
  printf
    "leaf lengths =\n%s\n%!"
    (Sexp.to_string_hum @@ [%sexp_of: Leaf_lengths.t] (Leaf_lengths.create tree));
  if stop_after_bvh
  then (
    printf "Stop after bvh build\n";
    Caml.exit 0);
  let ganesha_material =
    (* we don't support texture mapping yet, so just hard-coding this to green *)
    Material.lambertian (Texture.solid (Color.create ~r:0.0 ~g:0.7 ~b:0.1))
  in
  let module Render_cmd =
    Render_command.Make (struct
      let background = background
      let camera = camera

      let intersect r =
        match Triangles.intersect tree r ~t_min:0.0 ~t_max:Float.max_finite_value with
        | None -> None
        | Some tri_hit ->
          let open Float.O in
          let { Tri_hit.g_normal; pt; u; v; t_hit = _ } = tri_hit in
          let hit_front = V3.dot (Ray.direction r) g_normal < 0.0 in
          let normal = if hit_front then g_normal else V3.Infix.( ~- ) g_normal in
          let ss = Shader_space.create normal pt in
          let tex_coord = Texture.Coord.create u v in
          let wi = Shader_space.omega_i ss r in
          let do_scatter =
            Material.scatter ganesha_material ss tex_coord ~omega_i:wi ~hit_front
          in
          Some { Hit.shader_space = ss; emit = Color.black; do_scatter }
      ;;
    end)
  in
  Render_cmd.run common
;;

let () = main (Args.parse ())
