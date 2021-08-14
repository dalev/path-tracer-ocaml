open! Base
open Stdio
open Path_tracer
module Common_args = Render_command.Args

module Args = struct
  type t =
    { common : Common_args.t
    ; no_simd : bool
    }

  let parse () =
    let no_simd = ref false in
    let common =
      Common_args.parse
        ~specs:[ "-no-simd", Set no_simd, "do not use SIMD accelerated intersection" ]
        ()
    in
    { common; no_simd = !no_simd }
  ;;
end

let camera aspect =
  let eye = P3.create ~x:13.0 ~y:2.0 ~z:4.5 in
  let target = P3.origin in
  let up = V3.unit_y in
  Camera.create ~eye ~target ~up ~aspect ~vertical_fov_deg:20.0
;;

module Shirley_spheres = struct
  let p3 x y z = P3.create ~x ~y ~z
  let solid_tex r g b = Texture.solid (Color.create ~r ~g ~b)
  let solid_lambertian r g b = Material.lambertian @@ solid_tex r g b

  let ground =
    let a = solid_tex 0.2 0.3 0.1 in
    let b = solid_tex 0.9 0.9 0.9 in
    let checks = Material.lambertian @@ Texture.checker ~width:1000 ~height:2000 a b in
    Sphere.create ~material:checks ~center:(p3 0.0 (-1000.0) 0.0) ~radius:1000.0
  ;;

  let big_spheres =
    let glass = Material.glass in
    let metal = Material.metal (solid_tex 0.7 0.6 0.5) in
    let blue = solid_lambertian 0.0 0.1 0.4 in
    let radius = 1.0 in
    [ Sphere.create ~material:glass ~center:(p3 (-4.0) 1.0 0.0) ~radius
    ; Sphere.create ~material:metal ~center:(p3 0.0 1.0 0.0) ~radius
    ; Sphere.create ~material:blue ~center:(p3 4.0 1.0 0.0) ~radius
    ]
  ;;

  let randomf () = Random.float 1.0

  let random_v3 () =
    let x = randomf () in
    let y = randomf () in
    let z = randomf () in
    V3.create ~x ~y ~z
  ;;

  let random_lambertian () =
    let c = Color.of_v3 V3.Infix.(random_v3 () * random_v3 ()) in
    Material.lambertian (Texture.solid c)
  ;;

  let random_material () =
    let open Float.O in
    let roll = randomf () in
    if roll < 0.8
    then random_lambertian ()
    else if roll < 0.95
    then (
      let z = (0.5 * randomf ()) + 0.5 in
      Material.metal (solid_tex z z z))
    else Material.glass
  ;;

  let perturb x = Float.of_int x +. (0.9 *. randomf ())

  let small_sphere a b =
    let x = perturb a in
    let z = perturb b in
    let radius = 0.2 in
    let center = p3 x radius z in
    let p = p3 4.0 radius 0.0 in
    if Float.( > ) (V3.quadrance (V3.of_points ~src:center ~tgt:p)) 0.81
    then (
      let material = random_material () in
      Some (Sphere.create ~material ~center ~radius))
    else None
  ;;

  let spheres () =
    let rng = List.range (-11) 11 ~start:`inclusive ~stop:`inclusive in
    (ground :: big_spheres)
    @ List.concat_map rng ~f:(fun a -> List.filter_map rng ~f:(fun b -> small_sphere a b))
  ;;
end

let background =
  let escape_color = Color.create ~r:0.5 ~g:0.7 ~b:1.0 in
  fun ray ->
    let d = V3.normalize (Ray.direction ray) in
    let t = 0.5 *. (V3.dot d V3.unit_y +. 1.0) in
    Color.lerp t Color.white escape_color
;;

module type Leaf_S =
  Shape_tree.Leaf with type elt = Sphere.t and type elt_hit = float * Sphere.t

module Array_leaf : Leaf_S = Shape_tree.Array_leaf (struct
  include Sphere

  type hit = float * Sphere.t

  let hit_t = fst
  let length_cutoff = 8
  let depth _ = 0
  let length _ = 1

  let intersect t ray ~t_min ~t_max =
    match Sphere.intersect t ray ~t_min ~t_max with
    | None -> None
    | Some hit_t -> Some (hit_t, t)
  ;;
end)

module Simd_leaf : Leaf_S = struct
  module FArray = Caml.Float.Array

  type f64array = FArray.t

  type coords =
    { xs : f64array
    ; ys : f64array
    ; zs : f64array
    ; rs : f64array
    }

  type t =
    { coords : coords
    ; ms : Material.t array
    ; length : int
    }

  type elt = Sphere.t
  type elt_hit = float * elt

  let elt_hit_t = fst

  (* [float_ref]'s [float_contents] is unboxed (whereas the contents of [float ref] is boxed) *)
  type float_ref = { mutable float_contents : float }

  (* To achieve [@@noalloc], we make a couple of compromises:
     - the float_ref is an "output parameter": on return, contains the t_hit of the nearest sphere intersection.
     - return [int] rather than [int option]: -1 indicates no intersection was found.
       otherwise it's an index into the [coords] arrays indicating the closest intersection. *)
  external spheres_intersect_native
    :  coords
    -> (float[@unboxed])
    -> (float[@unboxed])
    -> Ray.t
    -> float_ref
    -> int
    = "spheres_intersect_bytecode" "spheres_intersect_native"
    [@@noalloc]

  external leaf_size : unit -> int = "leaf_size"

  let elt_bbox = Sphere.bbox
  let length_cutoff = leaf_size ()

  let of_elts elts =
    let elts_len = Array.length elts in
    let pad_len =
      let remainder = elts_len % 4 in
      if remainder = 0 then 0 else 4 - remainder
    in
    let len = elts_len + pad_len in
    let farray in_bounds_f =
      FArray.init len (fun i -> if i < elts_len then in_bounds_f elts.(i) else Float.nan)
    in
    let xs = farray (Fn.compose P3.x Sphere.center) in
    let ys = farray (Fn.compose P3.y Sphere.center) in
    let zs = farray (Fn.compose P3.z Sphere.center) in
    let rs = farray Sphere.radius in
    let ms = Array.map elts ~f:Sphere.material in
    { coords = { xs; ys; zs; rs }; ms; length = len }
  ;;

  let depth _ = 0
  let length t = t.length

  let center t idx =
    let c = t.coords in
    let x = FArray.get c.xs idx
    and y = FArray.get c.ys idx
    and z = FArray.get c.zs idx in
    P3.create ~x ~y ~z
  ;;

  let intersect t ray ~t_min ~t_max =
    let t_hit_ref = { float_contents = Float.nan } in
    let idx = spheres_intersect_native t.coords t_min t_max ray t_hit_ref in
    if idx < 0
    then None
    else (
      let t_hit = t_hit_ref.float_contents in
      let center = center t idx in
      let radius = FArray.get t.coords.rs idx in
      let sph = Sphere.create ~material:t.ms.(idx) ~center ~radius in
      Some (t_hit, sph))
  ;;
end

module type Spheres_S =
  Shape_tree.S with type elt := Sphere.t and type elt_hit := float * Sphere.t

let main { Args.common; no_simd } =
  let { Common_args.width; height; _ } = common in
  let leaf =
    if no_simd then (module Array_leaf : Leaf_S) else (module Simd_leaf : Leaf_S)
  in
  let module Leaf = (val leaf : Leaf_S) in
  let module Spheres : Spheres_S = Shape_tree.Make (Leaf) in
  let module Leaf_lengths = struct
    type s =
      { size : int
      ; count : int
      }
    [@@deriving sexp_of]

    type t = s list [@@deriving sexp_of]

    let create tree =
      Spheres.leaf_length_histogram tree
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (size, count) -> { size; count })
      |> List.sort ~compare:(fun a b -> Int.compare a.size b.size)
    ;;
  end
  in
  let spheres =
    Random.init 42;
    Shirley_spheres.spheres ()
  in
  printf "dim = %d x %d;\n" width height;
  printf "#spheres = %d\n" (List.length spheres);
  let camera = camera (width // height) in
  let elapsed, tree =
    let spheres =
      List.map spheres ~f:(fun s -> Sphere.transform s ~f:(Camera.transform camera))
    in
    Render_command.with_elapsed_time (fun () -> Spheres.create spheres)
  in
  printf "tree depth = %d\n" (Spheres.depth tree);
  printf "build time = %.3f ms\n" (Float.of_int63 elapsed *. 1e-6);
  printf
    "leaf lengths =\n%s\n%!"
    (Sexp.to_string_hum @@ [%sexp_of: Leaf_lengths.t] (Leaf_lengths.create tree));
  let module Render_cmd =
    Render_command.Make (struct
      let camera = camera
      let background = background

      let intersect r =
        match Spheres.intersect tree r ~t_min:0.0 ~t_max:Float.max_finite_value with
        | None -> None
        | Some (t_hit, sphere) -> Some (Sphere.hit sphere t_hit r)
      ;;
    end)
  in
  Render_cmd.run common
;;

let () = main (Args.parse ())
