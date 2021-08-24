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
  let eye = P3.create ~x:328.0 ~y:40.282 ~z:245.0 in
  let target = P3.create ~x:328.0 ~y:10.0 ~z:0.0 in
  let up = V3.create ~x:(-0.00212272) ~y:0.998201 ~z:(-0.0599264) in
  Camera.create ~eye ~target ~up ~aspect ~vertical_fov_deg:30.0
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
  let { Common_args.width; height; _ } = common in
  let camera = camera (width // height) in
  let ganesha_ply = load_ply_exn ganesha_ply in
  let mesh = Mesh.create ganesha_ply camera in
  let module Triangle =
    Make_triangle [@inlined] (struct
      let mesh = mesh
    end)
  in
  let module Leaf =
    Shape_tree.Array_leaf (struct
      include Triangle

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
  let triangles : Triangle.t list =
    let f = function
      | [| a; b; c |] -> { Triangle.Face.a; b; c }
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
  let ganesha_material =
    (* we don't support texture mapping yet, so just hard-coding this to green *)
    Material.lambertian (Texture.solid (Color.create ~r:0.0 ~g:0.7 ~b:0.1))
  in
  let module Ppm =
    Progressive_photon_map.Make (struct
      let camera = camera

      let bbox =
        let b = Triangles.bbox tree in
        printf "ganesha bbox = %s\n" @@ Sexp.to_string_mach ([%sexp_of: Bbox.t] b);
        b
      ;;

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
        match Triangles.intersect tree r ~t_min:0.0 ~t_max:Float.max_finite_value with
        | None -> None
        | Some tri_hit ->
          let open Float.O in
          let module H = Triangle.Hit in
          let g_normal = H.g_normal tri_hit in
          let pt = H.point tri_hit in
          let tex_coord = H.tex_coord tri_hit in
          let hit_front = V3.dot (Ray.direction r) g_normal < 0.0 in
          let normal = if hit_front then g_normal else V3.Infix.( ~- ) g_normal in
          let ss = Shader_space.create normal pt in
          let wi = Shader_space.omega_i ss r in
          let do_scatter =
            Material.scatter ganesha_material ss tex_coord ~omega_i:wi ~hit_front
          in
          Some { Hit.shader_space = ss; emit = Color.black; do_scatter }
      ;;
    end)
  in
  let elapsed_ns, () = with_elapsed_time (fun () -> Ppm.go pool) in
  printf "elapsed ms: %.3f\n" @@ (Float.of_int63 elapsed_ns *. 1e-6);
  Domainslib.Task.teardown_pool pool
;;

let () = main (Args.parse ())
