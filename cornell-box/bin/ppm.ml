open Base
open Path_tracer
open Stdio
module L = Low_discrepancy_sequence
module Task = Domainslib.Task

let take_2d s =
  let open L.Sample in
  let samples_index = ref 0 in
  fun () ->
    let j = !samples_index in
    let u = s.%{j}
    and v = s.%{j + 1} in
    samples_index := j + 2;
    u, v
;;

module Point_light = struct
  type t = { position : P3.t }

  let create ~position = { position }

  let color (_ : t) =
    let p = 250.0 (* watts *) in
    Color.(scale white p)
  ;;

  let random_direction u v =
    let open Float.O in
    let theta = 2.0 * Float.pi * u in
    let phi = Float.acos (1.0 - (2.0 * v)) in
    let sin_phi = Float.sin phi in
    V3.create
      ~x:(sin_phi * Float.cos theta)
      ~y:(sin_phi * Float.sin theta)
      ~z:(Float.cos phi)
  ;;

  let random_ray t u v =
    let dir = random_direction u v in
    Ray.create t.position dir
  ;;
end

module Photon = struct
  type t =
    { omega_i : V3.t (* points towards origin of this photon in SS *)
    ; shader_space : Shader_space.t (* local geom of diffuse interaction *)
    ; flux : Color.t
    ; radius : float
    }

  let create shader_space ray flux ~radius =
    let omega_i = Shader_space.omega_i shader_space ray in
    { omega_i; shader_space; flux; radius }
  ;;

  let center t = Shader_space.world_origin t.shader_space
end

module Photon_map : sig
  type t

  (* call [f] on each photon whose radius includes the given point *)
  val iter_neighbors : t -> P3.t -> f:(Photon.t -> unit) -> unit
  val length : t -> int

  val create
    :  Task.pool
    -> Low_discrepancy_sequence.t
    -> scene_intersect:(Ray.t -> Hit.t option)
    -> radius:float
    -> photon_count:int
    -> max_bounces:int
    -> point_lights:Point_light.t list
    -> t
end = struct
  module Tree = Shape_tree.Make (Shape_tree.Array_leaf (struct
    include Photon

    let length_cutoff = 8

    let bbox t =
      let center = Shader_space.world_origin t.shader_space in
      let radius = P3.create ~x:t.radius ~y:t.radius ~z:t.radius in
      let min = P3.Infix.(center - radius)
      and max = P3.Infix.(center + radius) in
      Bbox.create ~min ~max
    ;;

    type hit = unit

    let hit_t () = -1.0
    let depth _ = 0
    let length _ = 1

    let intersect (_ : t) (_ : Ray.t) ~t_min:_ ~t_max:_ =
      failwith "Photon_map.Tree.intersect: not used"
    ;;
  end))

  type t =
    { tree : Tree.t
    ; length : int
    }

  let length t = t.length

  let iter_neighbors t point ~f =
    let open Float.O in
    Tree.iter_neighbors t.tree point ~f:(fun a ->
        Array.iter a ~f:(fun p ->
            let v = V3.of_points ~src:(Photon.center p) ~tgt:point in
            if V3.quadrance v < Float.square p.radius then f p))
  ;;

  let trace_photon light sample max_bounces intersect ~radius =
    let open Float.O in
    let take_2d = take_2d sample in
    let photons = ref [] in
    let rec loop ray flux max_bounces =
      if Int.O.(max_bounces > 0)
      then (
        let max_bounces = Int.O.(max_bounces - 1) in
        match intersect ray with
        | None -> ()
        | Some h ->
          let u, v = take_2d () in
          (match Hit.scatter h u with
          | Absorb -> ()
          | Specular (ray, color) -> loop ray Color.Infix.(flux * color) max_bounces
          | Diffuse color ->
            let ss = Hit.shader_space h in
            let flux = Color.Infix.(flux * color) in
            photons := Photon.create ss ray flux ~radius :: !photons;
            let color_max = Color.max_coord color in
            if u < color_max
            then (
              let flux = Color.scale flux (1.0 / color_max) in
              let u = u / color_max in
              let dir = Shader_space.unit_square_to_hemisphere u v in
              let ray = Shader_space.world_ray ss dir in
              loop ray flux max_bounces)))
    in
    let u, v = take_2d () in
    let ray = Point_light.random_ray light u v in
    loop ray (Point_light.color light) max_bounces;
    !photons
  ;;

  let create
      pool
      sampler
      ~scene_intersect
      ~radius
      ~photon_count
      ~max_bounces
      ~point_lights
    =
    let module Chan = Domainslib.Chan in
    let c = Chan.make_unbounded () in
    let collector =
      Caml.Domain.spawn (fun () ->
          let rec loop photons =
            match Chan.recv c with
            | Some ps -> loop @@ List.rev_append ps photons
            | None -> photons
          in
          loop [])
    in
    (* CR dalev: reuses sampler for different lights, fix me *)
    List.iter point_lights ~f:(fun light ->
        Task.parallel_for pool ~start:0 ~finish:(photon_count - 1) ~body:(fun i ->
            let _, suffix = L.split_at sampler i in
            let s = snd @@ L.step suffix in
            Chan.send c @@ Some (trace_photon light s max_bounces scene_intersect ~radius)));
    Chan.send c None;
    let photons = Caml.Domain.join collector in
    let length = List.length photons in
    { tree = Tree.create ~pool photons; length }
  ;;
end

module Make (Scene : sig
  val bbox : Bbox.t
  val camera : Camera.t
  val point_lights : Point_light.t list
  val intersect : Ray.t -> Hit.t option
  val width : int
  val height : int
  val max_bounces : int
  val num_iterations : int
  val photon_count : int
  val alpha : float
end) =
struct
  open Scene

  let max_area = 32 * 32

  let init_radius2 =
    let { P3.x; y; z } = P3.Infix.( - ) (Bbox.max bbox) (Bbox.min bbox) in
    let a = (x +. y +. z) /. 3.0 in
    a /. Float.of_int (width + height)
  ;;

  let create_blank_image () = Bimage.Image.v Bimage.Type.f64 Bimage.Color.rgb width height

  let render_image pool sampler pmap =
    let module Image = Bimage.Image in
    let img = create_blank_image () in
    let write_pixel ~x ~y color =
      let y = height - 1 - y in
      let r, g, b = Color.to_rgb color in
      Image.set img x y 0 r;
      Image.set img x y 1 g;
      Image.set img x y 2 b
    in
    let inv_widthf = 1 // width in
    let inv_heightf = 1 // height in
    let estimate_color ~x ~y sample_vec =
      let take_2d = take_2d sample_vec in
      let rec loop ray beta max_bounces =
        if max_bounces <= 0
        then Color.black
        else (
          match intersect ray with
          | None -> Color.black
          | Some h ->
            let max_bounces = max_bounces - 1 in
            let u, _v = take_2d () in
            (match Hit.scatter h u with
            | Absorb -> Color.black
            | Specular (ray, color) ->
              let beta = Color.Infix.(color * beta) in
              loop ray beta max_bounces
            | Diffuse color ->
              let beta = Color.Infix.(color * beta) in
              let shader_space = Hit.shader_space h in
              let hit_point = Shader_space.world_origin shader_space in
              let hit_normal = Shader_space.world_normal shader_space in
              let open Float.O in
              let l = ref Color.black in
              let m = ref 0 in
              Photon_map.iter_neighbors pmap hit_point ~f:(fun p ->
                  let p_ss = p.Photon.shader_space in
                  let p_normal = Shader_space.world_normal p_ss in
                  if V3.dot p_normal hit_normal > 1e-3
                  then (
                    let area = Float.pi * Float.square p.radius in
                    let scalar = 1.0 / area in
                    Int.incr m;
                    l := Color.Infix.(!l + Color.scale p.flux scalar)));
              let open Color.Infix in
              if Int.( = ) !m 0
              then Color.black
              else (
                let m = Float.of_int !m in
                Color.scale (beta * !l) (1.0 / m))))
      in
      let dx, dy = take_2d () in
      let cx = inv_widthf *. (dx +. Float.of_int x)
      and cy = inv_heightf *. (dy +. Float.of_int y) in
      loop (Camera.ray camera cx cy) Color.white max_bounces
    in
    let pmap_length = Photon_map.length pmap in
    Task.parallel_for
      pool
      ~start:0
      ~finish:((width * height) - 1)
      ~body:(fun pixel ->
        let x = pixel % width
        and y = pixel / width in
        let _prefix, sampler = L.split_at sampler pixel in
        let sample_vec = snd @@ L.step sampler in
        let color' = estimate_color ~x ~y sample_vec in
        let color = Color.scale color' (1.0 /. Float.of_int pmap_length) in
        write_pixel ~x ~y color);
    img
  ;;

  let radius2 i =
    assert (i >= 1);
    let product = ref 1.0 in
    for k = 1 to i - 1 do
      let k = Float.of_int k in
      product := !product *. (k +. alpha) /. k
    done;
    !product *. init_radius2 /. Float.of_int i
  ;;

  let radius i = Float.sqrt @@ radius2 i

  let go pool =
    printf "#max-bounces = %d\n" max_bounces;
    printf "#photons/iter = %d\n" photon_count;
    printf "#iterations = %d\n" num_iterations;
    printf "-----\n%!";
    let sampler = L.create ~dimension:(2 + (2 * (max_bounces + 1))) in
    let img_sum = create_blank_image () in
    for i = 0 to num_iterations - 1 do
      let radius = radius (i + 1) in
      printf "#iteration = %d, radius = %.3f\n%!" i radius;
      let _prefix, p_sampler =
        L.split_at sampler (i * (photon_count + (width * height)))
      in
      let pmap =
        Photon_map.create
          pool
          p_sampler
          ~scene_intersect:Scene.intersect
          ~photon_count
          ~max_bounces
          ~radius
          ~point_lights
      in
      printf "  photon map length = %d\n%!" (Photon_map.length pmap);
      let _prefix, eye_sampler = L.split_at p_sampler photon_count in
      let img = render_image pool eye_sampler pmap in
      ignore
        (Bimage.Image.map2_inplace ( +. ) img_sum img
          : (float, Bigarray.float64_elt, [ `Rgb ]) Bimage.Image.t)
    done;
    let n = Float.of_int num_iterations in
    Bimage.Image.map_inplace (fun x -> x /. n) img_sum
  ;;
end
