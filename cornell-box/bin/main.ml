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
    let center = P3.create ~x:(0.1 + radius) ~y:radius ~z:(0.2 + radius) in
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

module Shape_tree :
  Shape_tree.S with type elt := Shape.t and type elt_hit := Shape.Hit.t =
Shape_tree.Make (Shape_tree.Array_leaf (struct
  include Shape

  type hit = Hit.t

  let hit_t = Hit.t_hit
  let length_cutoff = 2
  let depth _ = 0
  let length _ = 1
end))

let main argv =
  let { Render_command.Args.width; height; _ } = argv in
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
  let module Render_cmd =
    Render_command.Make (struct
      let camera = camera

      let background =
        let escape_color = Color.create ~r:0.5 ~g:0.7 ~b:1.0 in
        fun ray ->
          let d = V3.normalize (Ray.direction ray) in
          let t = 0.5 *. (V3.dot d V3.unit_y +. 1.0) in
          Color.lerp t Color.white escape_color
      ;;

      let intersect ray =
        match Shape_tree.intersect tree ray ~t_min:0.0 ~t_max:Float.max_finite_value with
        | None -> None
        | Some h -> Some (Shape.Hit.to_scatter h ray)
      ;;
    end)
  in
  Render_cmd.run argv
;;

let () = main @@ Render_command.Args.parse ()
