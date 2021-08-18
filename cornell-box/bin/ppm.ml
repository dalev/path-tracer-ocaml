open Base
open Path_tracer
open Stdio
module L = Low_discrepancy_sequence

module Point_light = struct
  type t = { position : P3.t }

  let create ~position = { position }
  let color (_ : t) = Color.(scale white 1.0)

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

  let pixel t = t.pixel
  let shader_space t = t.shader_space
  let beta t = t.beta
  let point t = Shader_space.world_origin t.shader_space
  let create ~shader_space ~pixel ~beta = { shader_space; pixel; beta }
end

module Pixel_stat = struct
  type t =
    { mutable radius2 : float
    ; mutable tau : Color.t
    ; mutable n : float
    ; mutex : Caml.Mutex.t
    }

  let create ~radius2 =
    { radius2; tau = Color.black; n = 0.0; mutex = Caml.Mutex.create () }
  ;;

  let critical_section t ~f =
    Caml.Mutex.lock t.mutex;
    Exn.protect ~f ~finally:(fun () -> Caml.Mutex.unlock t.mutex)
  ;;

  let radius2 t = t.radius2
  let tau t = t.tau
  let n t = t.n
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

  let collect_hit_points tile sampler =
    let pts = ref [] in
    let record p = pts := p :: !pts in
    let sampler = ref sampler in
    Tile.iter tile ~f:(fun ~x ~y ->
        let sampler', s = L.step !sampler in
        sampler := sampler';
        let take_2d = take_2d s in
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
        let dx, dy = take_2d () in
        let xf = inv_widthf *. (dx +. Float.of_int x)
        and yf = inv_heightf *. (dy +. Float.of_int y) in
        loop (Camera.ray camera xf yf) Color.white max_bounces);
    !pts
  ;;

  let create_pixel_stats ~radius2 =
    Array.init (width * height) ~f:(fun _ -> Pixel_stat.create ~radius2)
  ;;

  let shared_pixel_stats = create_pixel_stats ~radius2:init_radius2
  let shared_pixel_stat x y = shared_pixel_stats.((y * width) + x)

  module Hit_tree = Shape_tree.Make (Shape_tree.Array_leaf (struct
    type t = Hit_point.t
    type hit = unit

    let hit_t () = -1.0
    let depth _ = 0
    let length _ = 1
    let length_cutoff = 4
    let intersect (_ : t) (_ : Ray.t) ~t_min:_ ~t_max:_ = None

    let bbox t =
      let x, y = Hit_point.pixel t in
      let radius2 = Pixel_stat.radius2 (shared_pixel_stat x y) in
      let radius = Float.sqrt radius2 in
      let center = Hit_point.point t in
      let offset = V3.create ~x:radius ~y:radius ~z:radius in
      let min = P3.translate center V3.Infix.(~-offset)
      and max = P3.translate center offset in
      Bbox.create ~min ~max
    ;;
  end))

  let visit_neighbor hp photon_ss flux pixel_stats =
    let hp_ss = Hit_point.shader_space hp in
    let quadrance =
      V3.quadrance
      @@ V3.of_points
           ~tgt:(Shader_space.world_origin hp_ss)
           ~src:(Shader_space.world_origin photon_ss)
    in
    let pixel_stat =
      let x, y = Hit_point.pixel hp in
      pixel_stats.((y * width) + x)
    in
    let radius2 = Pixel_stat.radius2 pixel_stat in
    let dp =
      V3.dot (Shader_space.world_normal hp_ss) (Shader_space.world_normal photon_ss)
    in
    let open Float.O in
    if dp >= 1e-3 && quadrance <= radius2
    then
      Pixel_stat.critical_section pixel_stat ~f:(fun () ->
          pixel_stat.Pixel_stat.tau
            <- Color.Infix.(pixel_stat.Pixel_stat.tau + (flux * hp.Hit_point.beta));
          pixel_stat.Pixel_stat.n <- pixel_stat.Pixel_stat.n + 1.0)
  ;;

  let trace_photon hit_points light sample max_bounces pixel_stats =
    let take_2d = take_2d sample in
    let rec loop ray flux max_bounces =
      let max_bounces = max_bounces - 1 in
      if max_bounces >= 0
      then (
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
            Hit_tree.iter_neighbors
              hit_points
              (Shader_space.world_origin ss)
              ~f:(fun hps ->
                Array.iter hps ~f:(fun hp -> visit_neighbor hp ss flux pixel_stats));
            let dir = Shader_space.unit_square_to_hemisphere u v in
            let ray = Shader_space.world_ray ss dir in
            loop ray flux max_bounces))
    in
    let u, v = take_2d () in
    let ray = Point_light.random_ray light u v in
    loop ray (Point_light.color light) max_bounces
  ;;

  let gather_hit_points pool sampler tiles =
    let sampler = ref sampler in
    let tasks =
      List.map tiles ~f:(fun tile ->
          let prefix, suffix = L.split_at !sampler (Tile.area tile) in
          sampler := suffix;
          Task.async pool (fun () -> collect_hit_points tile prefix))
    in
    let pts = List.concat_map tasks ~f:(Task.await pool) in
    Hit_tree.create ~pool pts
  ;;

  let trace_photons pool sampler hit_points =
    let local_pixel_stats =
      Array.map shared_pixel_stats ~f:(fun sh ->
          let radius2 = sh.Pixel_stat.radius2 in
          Pixel_stat.create ~radius2)
    in
    List.iter point_lights ~f:(fun light ->
        Task.parallel_for pool ~start:1 ~finish:num_photons ~body:(fun i ->
            let pre, _ = L.split_at sampler i in
            let s = snd @@ L.step pre in
            trace_photon hit_points light s max_bounces local_pixel_stats));
    Task.parallel_for
      pool
      ~start:0
      ~finish:(Array.length shared_pixel_stats - 1)
      ~body:(fun i ->
        let local = local_pixel_stats.(i)
        and shared = shared_pixel_stats.(i) in
        let m = local.Pixel_stat.n in
        if Float.O.(m > 0.0)
        then (
          let n = shared.Pixel_stat.n in
          let r2 = shared.Pixel_stat.radius2 in
          let n' = n +. (alpha *. m) in
          let r2' = r2 *. n' /. (n +. m) in
          let tau' =
            Color.scale
              Color.Infix.(shared.Pixel_stat.tau + local.Pixel_stat.tau)
              (r2' /. r2)
          in
          shared.Pixel_stat.n <- n';
          shared.Pixel_stat.radius2 <- r2';
          shared.Pixel_stat.tau <- tau'))
  ;;

  let create_image pool =
    let img_data = Array.create ~len:(3 * Array.length shared_pixel_stats) 0.0 in
    Task.parallel_for
      pool
      ~start:0
      ~finish:(Array.length shared_pixel_stats - 1)
      ~body:(fun pix ->
        let ps = shared_pixel_stats.(pix) in
        let r, g, b =
          Color.to_rgb
          @@ Color.scale
               (Pixel_stat.tau ps)
               (1.0
               /. (Float.pi
                  *. Float.of_int (num_iterations * num_photons)
                  *. Pixel_stat.radius2 ps))
        in
        let base = 3 * pix in
        Array.set img_data (base + 0) r;
        Array.set img_data (base + 1) g;
        Array.set img_data (base + 2) b);
    Bimage.Image.of_data
      Bimage.Color.rgb
      width
      height
      (Bimage.Data.of_array Bimage.Type.f64 img_data)
  ;;

  let go () =
    printf "#tiles = %d\n" (List.length tiles);
    printf "#max-bounces = %d\n" max_bounces;
    printf "#photons = %d\n" num_photons;
    printf "#iterations = %d\n" num_iterations;
    printf "-----\n%!";
    let pool = Task.setup_pool ~num_additional_domains in
    let h_sampler = L.create ~dimension:(2 + (2 * (max_bounces + 1))) in
    let p_sampler = L.create ~dimension:(2 + (2 * (max_bounces + 1))) in
    for i = 0 to num_iterations - 1 do
      printf "#iteration = %d\n%!" i;
      let _prefix, h_sampler = L.split_at h_sampler (i * width * height) in
      let _prefix, p_sampler = L.split_at p_sampler (i * num_photons) in
      let hit_points = gather_hit_points pool h_sampler tiles in
      trace_photons pool p_sampler hit_points
    done;
    let img = create_image pool in
    Task.teardown_pool pool;
    let n_sum = Array.sum (module Float) shared_pixel_stats ~f:Pixel_stat.n in
    printf "sum(n/alpha) = %.3f\n" n_sum;
    img
  ;;
end
