open! Base
open Path_tracer
module Ppm = Progressive_photon_map

module Face = struct
  type t =
    { material : Material.t
    ; vertices : P3.t * P3.t * P3.t
    ; texs : Texture.Coord.t * Texture.Coord.t * Texture.Coord.t
    }

  let vertices t = t.vertices
  let material t = t.material
  let tex_coords t = t.texs
end

module Triangle = struct
  include Triangle.Make (Face)

  let create ~material (a, ta) (b, tb) (c, tc) =
    { Face.material; vertices = a, b, c; texs = ta, tb, tc }
  ;;

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
  triangle_fan ~material Texture.Coord.[ a, t00; b, t10; c, t11; d, t01 ]
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
  let behind_camera =
    (* stick this here to prevent photons from escaping into the great beyond *)
    let material = Material.lambertian (solid_tex 0.75 0.75 0.75) in
    let radius = 10.0 in
    let center = P3.create ~x:0.5 ~y:0.5 ~z:(-2.0 -. radius) in
    Sphere.create ~material ~center ~radius
  in
  [ metal; glass; behind_camera ]
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
      | T tri_hit -> Triangle.Hit.to_hit tri_hit ray
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

module Args = Progressive_photon_map.Args

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
    let vertical_fov_deg =
      let theta = 2.0 *. Float.atan 0.5 in
      theta *. 180.0 /. Float.pi
    in
    Camera.create ~eye ~target ~up ~aspect ~vertical_fov_deg
  in
  let light_center = P3.create ~x:0.5 ~y:0.82 ~z:0.5 in
  let _light_enclosure =
    let radius = 0.05 in
    (* slightly off center from the light to create a shadow along the walls *)
    let center = P3.create ~x:0.5 ~y:0.80 ~z:0.5 in
    [ Shape.of_sphere @@ Sphere.create ~material:Material.glass ~center ~radius ]
  in
  let light_enclosure' =
    (* a cube with the top and bottom open *)
    let quad p u v =
      let u = V3.scale u 2.0
      and v = V3.scale v 2.0 in
      quad ~material:(Material.metal (solid_tex 0.30 0.999 0.30)) p u v
    in
    let r = 0.05 in
    let rx = V3.(scale unit_x r) in
    let ry = V3.(scale unit_y r) in
    let rz = V3.(scale unit_z r) in
    let open V3.Infix in
    let lc = P3.to_v3 light_center in
    let a = P3.of_v3 @@ (lc - rx - ry - rz) in
    let b = P3.of_v3 @@ (lc + rx - ry + rz) in
    let r = quad a rz ry in
    let f = quad a ry rx in
    let l = quad b ~-rz ry in
    let b = quad b rx ry in
    List.map ~f:Shape.of_triangle @@ List.concat_no_order [ r; f; l; b ]
  in
  let tree =
    let f = Camera.transform camera in
    let shapes =
      light_enclosure'
      @ List.map (empty_box ()) ~f:Shape.of_triangle
      @ List.map (spheres ()) ~f:Shape.of_sphere
    in
    Shape_tree.create @@ List.map shapes ~f:(fun t -> Shape.transform t ~f)
  in
  let module Scene = struct
    let args = argv
    let camera = camera
    let bbox = Shape_tree.bbox tree

    let lights =
      let position = Camera.transform camera light_center in
      [ Ppm.Light.create_point ~position ~power:2.0 ~color:Color.white ]
    ;;

    let intersect ray =
      match Shape_tree.intersect tree ray ~t_min:0.0 ~t_max:Float.max_finite_value with
      | None -> None
      | Some h -> Some (Shape.Hit.to_scatter h ray)
    ;;
  end
  in
  let module Ppm = Ppm.Make (Scene) in
  let start = Time_now.nanoseconds_since_unix_epoch () in
  let output = argv.Args.output in
  let () = Ppm.go ~output in
  let elapsed_ns = Int63.O.(Time_now.nanoseconds_since_unix_epoch () - start) in
  Stdio.printf "render time = %.3f ms\n" (1e-6 *. Float.of_int63 elapsed_ns)
;;

let () = main @@ Args.parse ()
