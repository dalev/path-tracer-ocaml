open Base
open Stdio
open Path_tracer
open Ply_format
module Bigstring = Base_bigstring
module FArray = Caml.Float.Array
module Common_args = Progressive_photon_map.Args

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
  let eye = P3.create ~x:328.0 ~y:70.282 ~z:345.0 in
  let target = P3.create ~x:328.0 ~y:10.0 ~z:0.0 in
  let up = V3.create ~x:(-0.00212272) ~y:0.998201 ~z:(-0.0599264) in
  Camera.create ~eye ~target ~up ~aspect ~vertical_fov_deg:30.0
;;

let t00 = Texture.Coord.create 0.0 0.0
let t01 = Texture.Coord.create 0.0 1.0
let t10 = Texture.Coord.create 1.0 0.0
let t11 = Texture.Coord.create 1.0 1.0

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
    let num_vertices = fst3 lengths in
    let in_bounds a = a >= 0 && a < num_vertices in
    assert (Array.for_all faces ~f:(fun face -> Array.for_all face ~f:in_bounds));
    { xs; ys; zs; faces }
  ;;
end

module Make_triangle (M : sig
  val mesh : Mesh.t
end) =
struct
  module Face = struct
    type t =
      { a : int
      ; b : int
      ; c : int
      }

    let xs = M.mesh.Mesh.xs
    let ys = M.mesh.Mesh.ys
    let zs = M.mesh.Mesh.zs

    let point i =
      let x = FArray.unsafe_get xs i in
      let y = FArray.unsafe_get ys i in
      let z = FArray.unsafe_get zs i in
      P3.create ~x ~y ~z
    ;;

    let[@inline] vertices t = point t.a, point t.b, point t.c
    let tex_coords _ = t00, t01, t11

    let material _ =
      (* we don't support texture mapping yet, so just hard-coding this to green *)
      Material.lambertian (Texture.solid (Color.create ~r:0.0 ~g:0.7 ~b:0.1))
    ;;
  end

  include Triangle.Make (Face)
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

let with_elapsed_time f =
  let start = Time_now.nanoseconds_since_unix_epoch () in
  let x = f () in
  let stop = Time_now.nanoseconds_since_unix_epoch () in
  let elapsed = Int63.(stop - start) in
  elapsed, x
;;

let main { Args.common; ganesha_ply; stop_after_bvh } =
  let { Common_args.width; height; output; _ } = common in
  let camera = camera (width // height) in
  let ganesha_ply = load_ply_exn ganesha_ply in
  let mesh = Mesh.create ganesha_ply camera in
  let module Mesh_triangle =
    Make_triangle [@inlined] (struct
      let mesh = mesh
    end)
  in
  let module Leaf =
    Shape_tree.Array_leaf (struct
      include Mesh_triangle

      type hit = Hit.t

      let hit_t = Hit.t_hit
      let length_cutoff = 8
      let depth _ = 0
      let length _ = 1
    end)
  in
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
  let triangles : Mesh_triangle.t list =
    let f = function
      | [| a; b; c |] -> { Mesh_triangle.Face.a; b; c }
      | _ -> failwith "expected triangular face"
    in
    Array.to_list @@ Array.map ~f mesh.Mesh.faces
  in
  printf "dim = %d x %d;\n" width height;
  printf "#triangles = %d\n%!" (List.length triangles);
  let pool = Domainslib.Task.setup_pool ~num_additional_domains:7 in
  let elapsed, tree = with_elapsed_time (fun () -> Triangles.create ~pool triangles) in
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
  let ganesha_bbox = Triangles.bbox tree in
  printf "ganesha bbox = %s\n" @@ Sexp.to_string_mach ([%sexp_of: Bbox.t] ganesha_bbox);
  let module Floor = struct
    (* this is already in camera-space *)
    let center =
      let { P3.x; z; y = _ } = Bbox.center ganesha_bbox in
      let y = (Bbox.min ganesha_bbox).P3.y in
      P3.create ~x ~y ~z
    ;;

    let x', z' =
      let s = 500. in
      V3.(scale unit_x s), V3.(scale unit_z s)
    ;;

    let checker =
      let solid_tex r g b = Texture.solid (Color.create ~r ~g ~b) in
      let a = solid_tex 0.2 0.3 0.1 in
      let b = solid_tex 0.9 0.9 0.9 in
      Material.lambertian @@ Texture.checker ~width:100 ~height:100 a b
    ;;

    let a = t00, P3.translate center V3.Infix.(~-(x' + z'))
    let b = t01, P3.translate (snd a) (V3.scale x' 2.0)
    let c = t11, P3.translate (snd b) (V3.scale z' 2.0)
    let d = t10, P3.translate (snd a) (V3.scale z' 2.0)

    module Face = struct
      type t =
        { vertices : P3.t * P3.t * P3.t
        ; texs : Texture.Coord.t * Texture.Coord.t * Texture.Coord.t
        }

      let vertices t = t.vertices
      let tex_coords t = t.texs
      let material _ = checker
      let create a b c = { vertices = snd a, snd b, snd c; texs = fst a, fst b, fst c }
    end

    module Tri = Triangle.Make (Face)

    let f1 = Face.create a b c
    let f2 = Face.create a c d

    let intersect r =
      let t_min = 0.
      and t_max = Float.max_finite_value in
      match Tri.intersect f1 r ~t_min ~t_max with
      | Some _ as h -> h
      | None ->
        (match Tri.intersect f2 r ~t_min ~t_max with
        | Some _ as h -> h
        | None -> None)
    ;;

    let to_hit t ray = Tri.Hit.to_hit t ray
    let t_hit t = Tri.Hit.t_hit t
  end
  in
  let module Ppm =
    Progressive_photon_map.Make (struct
      let camera = camera
      let bbox = ganesha_bbox

      let lights =
        [ Progressive_photon_map.Light.create_spot
            ~position:(P3.create ~x:0.0 ~y:0.0 ~z:1.0)
            ~direction:V3.Infix.(~-V3.unit_z)
            ~color:Color.white
            ~power:1000.0
        ]
      ;;

      let args = common

      let intersect r =
        let t_min = 0.0 in
        match Floor.intersect r with
        | Some h ->
          let t_max = Floor.t_hit h in
          (match Triangles.intersect tree r ~t_min ~t_max with
          | None -> Some (Floor.to_hit h r)
          | Some tri_hit -> Some (Mesh_triangle.Hit.to_hit tri_hit r))
        | None ->
          (match Triangles.intersect tree r ~t_min:0.0 ~t_max:Float.max_finite_value with
          | None -> None
          | Some tri_hit -> Some (Mesh_triangle.Hit.to_hit tri_hit r))
      ;;
    end)
  in
  let output =
    match Bimage_io.Output.create output with
    | Ok t -> t
    | Error _ -> failwith "cannot create output"
  in
  let elapsed_ns, () = with_elapsed_time (fun () -> Ppm.go pool output) in
  printf "elapsed ms: %.3f\n" @@ (Float.of_int63 elapsed_ns *. 1e-6);
  Domainslib.Task.teardown_pool pool
;;

let () = main (Args.parse ())
