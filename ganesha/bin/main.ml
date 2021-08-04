open! Base
open Stdio
open Path_tracer
open Ply_format
module Bigstring = Base_bigstring
module FArray = Caml.Float.Array
module Image = Bimage.Image

module Args = struct
  type t =
    { width : int
    ; height : int
    ; spp : int
    ; output : string
    ; no_progress : bool
    ; no_simd : bool
    ; max_bounces : int
    ; ganesha_ply : string
    }

  let parse () =
    let width = ref 600 in
    let height = ref !width in
    let spp = ref 1 in
    let file = ref "ganesha.png" in
    let no_progress = ref false in
    let no_simd = ref false in
    let max_bounces = ref 4 in
    let ganesha_ply = ref "ganesha.ply" in
    let usage_msg =
      Printf.sprintf "Defaults: width = %d, height = %d, output = %s" !width !height !file
    in
    Caml.Arg.parse
      [ "-width", Set_int width, "<integer> image width"
      ; "-height", Set_int height, "<integer> image height"
      ; "-samples-per-pixel", Set_int spp, "<integer> samples-per-pixel"
      ; "-o", Set_string file, "<file> output file"
      ; "-no-simd", Set no_simd, "do not use SIMD accelerated intersection"
      ; "-no-progress", Set no_progress, "suppress progress monitor"
      ; "-max-bounces", Set_int max_bounces, "<integer> max ray bounces"
      ; "-ganesha-ply", Set_string ganesha_ply, "<file> path to ganesha.ply"
      ]
      (fun (_ : string) -> failwith "No anonymous arguments expected")
      usage_msg;
    { width = !width
    ; height = !height
    ; spp = !spp
    ; output = !file
    ; no_progress = !no_progress
    ; no_simd = !no_simd
    ; max_bounces = !max_bounces
    ; ganesha_ply = !ganesha_ply
    }
  ;;
end

let color_space = Bimage.rgb
let mkImage width height = Image.v Bimage.f64 color_space width height

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
    let xs' = f "x" in
    let ys' = f "y" in
    let zs' = f "z" in
    let len = FArray.length xs' in
    assert (len = FArray.length ys');
    assert (len = FArray.length zs');
    let xs = FArray.create len in
    let ys = FArray.create len in
    let zs = FArray.create len in
    for i = 0 to len - 1 do
      let x = FArray.get xs i in
      let y = FArray.get ys i in
      let z = FArray.get zs i in
      let p = Camera.transform camera @@ P3.create ~x ~y ~z in
      FArray.set xs i (P3.x p);
      FArray.set ys i (P3.y p);
      FArray.set zs i (P3.z p)
    done;
    { xs; ys; zs; faces }
  ;;
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

  let vertex i =
    let x = FArray.get xs i in
    let y = FArray.get ys i in
    let z = FArray.get zs i in
    P3.create ~x ~y ~z
  ;;

  let bbox t =
    let face = faces.(t) in
    let a = vertex face.(0) in
    let b = vertex face.(1) in
    let c = vertex face.(2) in
    let lo = P3.map2 ~f:Float.min in
    let hi = P3.map2 ~f:Float.max in
    Bbox.create ~min:(lo (lo a b) c) ~max:(hi (hi a b) c)
  ;;

  let hit (_ : t) (_t_hit : float) (_ray : Ray.t) = failwith "hit"
  let transform (_ : t) ~f:_ = failwith "transform"
  let intersect (_ : t) _ ~t_min:_ ~t_max:_ = failwith "intersect"
end

module Make_array_leaf (Elt : sig
  type t

  val bbox : t -> Bbox.t
  val hit : t -> float -> Ray.t -> Hit.t
  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> float option
end) : Skd_tree.Leaf with type elt = Elt.t = struct
  type t = Elt.t array
  type elt = Elt.t

  let length_cutoff = 8
  let of_elts = Fn.id
  let elt_bbox = Elt.bbox
  let hit = Elt.hit
  let depth _ = 0
  let length = Array.length

  let intersect t ray ~t_min ~t_max =
    let t_max = ref t_max in
    let item = ref None in
    for i = 0 to Array.length t - 1 do
      let s = t.(i) in
      match Elt.intersect s ray ~t_min ~t_max:!t_max with
      | None -> ()
      | Some t_hit ->
        item := Some s;
        t_max := t_hit
    done;
    match !item with
    | None -> None
    | Some s -> Some (!t_max, s)
  ;;
end

let load_ply_exn path =
  let shared = false in
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0o600 in
  let (bs : Bigstring.t) =
    Bigarray.array1_of_genarray
    @@ Unix.map_file fd Bigarray.char Bigarray.c_layout shared [| -1 |]
  in
  let ply = Ply.of_bigstring bs in
  Unix.close fd;
  Or_error.ok_exn ply
;;

let main args =
  let { Args.width
      ; height
      ; spp
      ; output
      ; no_progress
      ; max_bounces
      ; no_simd = _
      ; ganesha_ply
      }
    =
    args
  in
  let camera = camera (width // height) in
  let ganesha_ply = load_ply_exn ganesha_ply in
  let mesh = Mesh.create ganesha_ply camera in
  let module Triangle =
    Make_triangle (struct
      let mesh = mesh
    end)
  in
  let module Leaf = Make_array_leaf (Triangle) in
  let module Triangles = Skd_tree.Make (Leaf) in
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
  let img = mkImage width height in
  let write_pixel ~x ~y color =
    let r, g, b = Color.to_rgb color in
    Image.set img x y 0 r;
    Image.set img x y 1 g;
    Image.set img x y 2 b
  in
  let triangles : Triangle.t list = List.init (Array.length mesh.Mesh.faces) ~f:Fn.id in
  printf "dim = %d x %d;\n" width height;
  printf "#triangles = %d\n" (List.length triangles);
  let tree = Triangles.create triangles in
  printf "tree depth = %d\n" (Triangles.depth tree);
  printf
    "leaf lengths =\n%s\n%!"
    (Sexp.to_string_hum @@ [%sexp_of: Leaf_lengths.t] (Leaf_lengths.create tree));
  let i =
    let intersect r =
      Triangles.intersect tree r ~t_min:0.0 ~t_max:Float.max_finite_value
    in
    Integrator.create
      ~width
      ~height
      ~write_pixel
      ~max_bounces
      ~samples_per_pixel:spp
      ~intersect
      ~background
      ~camera
      ~diffuse_plus_light:Pdf.diffuse
  in
  let () =
    if no_progress
    then Integrator.render i ~update_progress:ignore
    else (
      let p =
        let open Progress.Line in
        let total = Integrator.count_tiles i in
        list [ spinner (); elapsed (); bar ~style:`ASCII total; count_to total; spacer 4 ]
      in
      Progress.with_reporter p (fun report ->
          let update_progress () = report 1 in
          Integrator.render i ~update_progress))
  in
  printf "\n";
  let () =
    match Bimage_io.write output img with
    | Ok () -> ()
    | Error (`File_not_found f) -> printf "File not found: %s" f
    | Error (#Bimage.Error.t as other) -> Bimage.Error.unwrap (Error other)
  in
  printf "Done\n"
;;

let () = main (Args.parse ())
