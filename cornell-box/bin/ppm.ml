open Base
open Path_tracer
open Stdio
module L = Low_discrepancy_sequence

module Point_light = struct
  type t = { position : P3.t }

  let create ~position = { position }
  let color (_ : t) = Color.(scale white (2500.0 *. 4.0 *. Float.pi))

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

module Hit_point = struct
  type t =
    { pixel : int * int
    ; beta : Color.t
    ; shader_space : Shader_space.t
    }

  let point t = Shader_space.world_origin t.shader_space
  let create ~shader_space ~pixel ~beta = { shader_space; pixel; beta }
end

module Pixel_stat = struct
  type t =
    { mutable radius2 : float
    ; mutable tau : Color.t
    ; mutable n_over_alpha : int
    }

  let create ~radius2 = { radius2; tau = Color.black; n_over_alpha = 0 }
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
end) =
struct
  open Scene
  module Task = Domainslib.Task

  let num_photons = 10_000
  let alpha = 2 // 3
  let inv_widthf = 1 // width
  let inv_heightf = 1 // height
  let max_area = 32 * 32
  let num_additional_domains = 7
  let tiles = Tile.split ~max_area @@ Tile.create ~width ~height

  let init_radius2 =
    let { P3.x; y; z } = P3.Infix.( - ) (Bbox.max bbox) (Bbox.min bbox) in
    let a = (x +. y +. z) /. 3.0 in
    a /. Float.of_int (width + height)
  ;;

  let collect_hit_points tile sampler =
    let pts = ref [] in
    let record p = pts := p :: !pts in
    let sampler = ref sampler in
    Tile.iter tile ~f:(fun ~x ~y ->
        let sampler', s = L.step !sampler in
        sampler := sampler';
        let take_2d =
          let open L.Sample in
          let samples_index = ref 2 in
          fun () ->
            let j = !samples_index in
            let u = s.%{j}
            and v = s.%{j + 1} in
            samples_index := j + 2;
            u, v
        in
        let rec loop ray beta max_bounces =
          if max_bounces > 0
          then
            Option.iter (intersect ray) ~f:(fun h ->
                let max_bounces = max_bounces - 1 in
                let u, _v = take_2d () in
                match Hit.scatter h u with
                | Absorb -> ()
                | Specular (ray, color) ->
                  let beta = Color.Infix.(color * beta) in
                  loop ray beta max_bounces
                | Diffuse color ->
                  let shader_space = Hit.shader_space h in
                  let beta = Color.Infix.(color * beta) in
                  let pixel = x, y in
                  record (Hit_point.create ~pixel ~shader_space ~beta))
        in
        let open L.Sample in
        let dx = s.%{0}
        and dy = s.%{1} in
        let xf = inv_widthf *. (dx +. Float.of_int x)
        and yf = inv_heightf *. (dy +. Float.of_int y) in
        loop (Camera.ray camera xf yf) Color.white max_bounces);
    !pts
  ;;

  let pixel_stats =
    Array.init (width * height) ~f:(fun _ -> Pixel_stat.create ~radius2:init_radius2)
  ;;

  let trace_photon (_hit_points : Hit_point.t list) light ray sample =
    let sample_idx = ref 2 in
    let u () =
      let open L.Sample in
      let x = sample.%{!sample_idx} in
      Int.incr sample_idx;
      x
    in
    let rec loop ray flux =
      match intersect ray with
      | None -> ()
      | Some h ->
        (match Hit.scatter h (u ()) with
        | Absorb -> ()
        | Specular (ray, color) -> loop ray Color.Infix.(flux * color)
        | Diffuse color ->
          let _color = Color.Infix.(flux * color) in
          ())
    in
    loop ray (Point_light.color light)
  ;;

  let go () =
    ignore num_iterations;
    printf "#tiles = %d\n" (List.length tiles);
    let pool = Task.setup_pool ~num_additional_domains in
    let tasks =
      let s = ref (L.create ~dimension:(2 + (2 * (max_bounces + 1)))) in
      List.map tiles ~f:(fun tile ->
          let n = Tile.area tile in
          let sampler, suffix = L.split_at !s n in
          s := suffix;
          Task.async pool (fun () -> collect_hit_points tile sampler))
    in
    let hit_pts = List.concat_map tasks ~f:(Task.await pool) in
    let sampler = ref @@ L.create ~dimension:(2 + max_bounces) in
    let open L.Sample in
    List.iter point_lights ~f:(fun light ->
        let sampler', s = L.step !sampler in
        sampler := sampler';
        let ray = Point_light.random_ray light s.%{0} s.%{1} in
        trace_photon hit_pts light ray s);
    Task.teardown_pool pool;
    printf "Found %d hit points\n" (List.length hit_pts)
  ;;
end
