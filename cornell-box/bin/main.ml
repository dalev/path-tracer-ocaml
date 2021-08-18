open! Base
open Path_tracer

module Face = struct
  type t =
    { material : Material.t
    ; vertices : P3.t * P3.t * P3.t
    }

  let vertices t = t.vertices
end

module Triangle = struct
  include Triangle.Make (Face)

  let create ~material a b c = { Face.material; vertices = a, b, c }
  let material t = t.Face.material

  let transform t ~f =
    let a, b, c = Face.vertices t in
    { t with vertices = f a, f b, f c }
  ;;
end

let triangle_fan ~material pts =
  assert (List.length pts >= 3);
  let o = List.hd_exn pts in
  let tri = Triangle.create ~material in
  let rec loop pts tris =
    match pts with
    | [] -> failwith "BUG: triangle_fan"
    | [ _ ] -> tris
    | a :: (b :: _ as tl) -> loop tl (tri o a b :: tris)
  in
  loop (List.tl_exn pts) []
;;

let quad ~material a u v =
  let b = P3.translate a v in
  let c = P3.translate b u in
  let d = P3.translate a u in
  triangle_fan ~material [ a; b; c; d ]
;;

let solid_tex r g b = Texture.solid (Color.create ~r ~g ~b)

let empty_box () =
  let solid r g b = Material.lambertian (solid_tex r g b) in
  let red = solid 0.7 0.0 0.0 in
  let blue = solid 0.0 0.0 0.7 in
  let grey = solid 0.7 0.7 0.7 in
  let checker =
    let a = solid_tex 0.2 0.3 0.1 in
    let b = solid_tex 0.9 0.9 0.9 in
    Material.lambertian @@ Texture.checker ~width:10 ~height:10 a b
  in
  let right_wall = quad ~material:red P3.origin V3.unit_z V3.unit_y in
  let left_wall = quad ~material:blue (P3.of_v3 V3.unit_x) V3.unit_z V3.unit_y in
  let floor = quad ~material:checker P3.origin V3.unit_x V3.unit_z in
  let ceiling = quad ~material:grey (P3.of_v3 V3.unit_y) V3.unit_x V3.unit_z in
  let rear_wall = quad ~material:grey (P3.of_v3 V3.unit_z) V3.unit_x V3.unit_y in
  List.concat_no_order [ right_wall; left_wall; floor; ceiling; rear_wall ]
;;

let spheres () =
  let open Float.O in
  let radius = 0.20 in
  let metal =
    let material = Material.metal (solid_tex 1.0 1.0 1.0) in
    let center = P3.create ~x:(1.0 - 0.1 - radius) ~y:radius ~z:(1.0 - 0.2 - radius) in
    Sphere.create ~material ~center ~radius
  in
  let glass =
    let material = Material.glass in
    let center = P3.create ~x:(0.1 + radius) ~y:(0.1 + radius) ~z:(0.2 + radius) in
    Sphere.create ~material ~center ~radius
  in
  [ metal; glass ]
;;

module Shape : sig
  type t

  val of_triangle : Triangle.t -> t
  val of_sphere : Sphere.t -> t
  val bbox : t -> Bbox.t
  val transform : t -> f:(P3.t -> P3.t) -> t

  module Hit : sig
    type t

    val t_hit : t -> float
    val to_scatter : t -> Ray.t -> Hit.t
  end

  val intersect : t -> Ray.t -> t_min:float -> t_max:float -> Hit.t option
end = struct
  type t =
    | S of Sphere.t
    | T of Triangle.t

  let of_triangle t = T t
  let of_sphere s = S s

  module Hit = struct
    type t =
      | S of
          { t_hit : float
          ; sphere : Sphere.t
          }
      | T of Triangle.Hit.t

    let t_hit = function
      | S { t_hit; _ } -> t_hit
      | T t -> Triangle.Hit.t_hit t
    ;;

    let to_scatter t ray =
      match t with
      | S { t_hit; sphere } -> Sphere.hit sphere t_hit ray
      | T tri_hit ->
        let open Float.O in
        let module H = Triangle.Hit in
        let g_normal = H.g_normal tri_hit in
        let pt = H.point tri_hit in
        let tex_coord = H.tex_coord tri_hit in
        let hit_front = V3.dot (Ray.direction ray) g_normal < 0.0 in
        let normal = if hit_front then g_normal else V3.Infix.( ~- ) g_normal in
        let ss = Shader_space.create normal pt in
        let wi = Shader_space.omega_i ss ray in
        let material = Triangle.material @@ H.face tri_hit in
        let do_scatter = Material.scatter material ss tex_coord ~omega_i:wi ~hit_front in
        { Hit.shader_space = ss; emit = Color.black; do_scatter }
    ;;
  end

  let bbox = function
    | S s -> Sphere.bbox s
    | T t -> Triangle.bbox t
  ;;

  let intersect t ray ~t_min ~t_max =
    match t with
    | S s ->
      Option.map (Sphere.intersect s ray ~t_min ~t_max) ~f:(fun t_hit ->
          Hit.S { t_hit; sphere = s })
    | T t -> Option.map (Triangle.intersect t ray ~t_min ~t_max) ~f:(fun h -> Hit.T h)
  ;;

  let transform t ~f =
    match t with
    | S s -> S (Sphere.transform s ~f)
    | T t -> T (Triangle.transform t ~f)
  ;;
end

module Args = struct
  type t =
    { width : int
    ; height : int
    ; iterations : int
    ; max_bounces : int
    ; photon_count : int
    ; alpha : float
    ; output : string
    }

  let parse ?(specs = []) () =
    let width = ref 600 in
    let height = ref !width in
    let iterations = ref 1 in
    let photon_count = ref 10_000 in
    let file = ref "output.png" in
    let alpha = ref (2 // 3) in
    let no_progress = ref false in
    let max_bounces = ref 4 in
    let usage_msg =
      Printf.sprintf "Defaults: width = %d, height = %d, output = %s" !width !height !file
    in
    let specs =
      specs
      @ Caml.Arg.
          [ "-width", Set_int width, "<integer> image width"
          ; "-height", Set_int height, "<integer> image height"
          ; "-iterations", Set_int iterations, "<integer> # photon-map iterations"
          ; "-photon-count", Set_int photon_count, "<integer> #photons per iteration"
          ; "-alpha", Set_float alpha, "<float-in-(0,1)> photon-map alpha"
          ; "-o", Set_string file, "<file> output file"
          ; "-no-progress", Set no_progress, "suppress progress monitor"
          ; "-max-bounces", Set_int max_bounces, "<integer> max ray bounces"
          ]
    in
    Caml.Arg.parse
      specs
      (fun (_ : string) -> failwith "No anonymous arguments expected")
      usage_msg;
    { width = !width
    ; height = !height
    ; iterations = !iterations
    ; max_bounces = !max_bounces
    ; photon_count = !photon_count
    ; alpha = !alpha
    ; output = !file
    }
  ;;
end

module Shape_tree = Shape_tree.Make (Shape_tree.Array_leaf (struct
  include Shape

  type hit = Hit.t

  let hit_t = Hit.t_hit
  let length_cutoff = 2
  let depth _ = 0
  let length _ = 1
end))

let main argv =
  let { Args.width; height; _ } = argv in
  let camera =
    let aspect = width // height in
    let eye = P3.create ~x:0.5 ~y:0.5 ~z:(-1.0) in
    let target = P3.create ~x:0.5 ~y:0.5 ~z:0.0 in
    let up = V3.unit_y in
    Camera.create ~eye ~target ~up ~aspect ~vertical_fov_deg:45.0
  in
  let tree =
    let f = Camera.transform camera in
    let shapes =
      List.map (empty_box ()) ~f:Shape.of_triangle
      @ List.map (spheres ()) ~f:Shape.of_sphere
    in
    Shape_tree.create @@ List.map shapes ~f:(fun t -> Shape.transform t ~f)
  in
  let module Scene = struct
    let width = width
    let height = height
    let camera = camera
    let num_iterations = argv.Args.iterations
    let max_bounces = argv.Args.max_bounces
    let photon_count = argv.Args.photon_count
    let alpha = argv.Args.alpha
    let bbox = Shape_tree.bbox tree

    let point_lights =
      let position = Camera.transform camera (P3.create ~x:0.5 ~y:0.95 ~z:0.5) in
      [ Ppm.Point_light.create ~position ]
    ;;

    let intersect ray =
      match Shape_tree.intersect tree ray ~t_min:0.0 ~t_max:Float.max_finite_value with
      | None -> None
      | Some h -> Some (Shape.Hit.to_scatter h ray)
    ;;
  end
  in
  let module Ppm = Ppm.Make (Scene) in
  let num_additional_domains = 7 in
  let pool = Domainslib.Task.setup_pool ~num_additional_domains in
  let img = Ppm.go pool in
  Domainslib.Task.teardown_pool pool;
  match Bimage_io.write "output.png" img with
  | Ok () -> ()
  | Error (`File_not_found s) -> failwith @@ "file not found: " ^ s
  | Error (#Bimage.Error.t as e) -> Bimage.Error.exc e
;;

let () = main @@ Args.parse ()
