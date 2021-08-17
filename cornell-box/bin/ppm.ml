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
    ; mutable n_over_alpha : int
    }

  let create ~radius2 = { radius2; tau = Color.black; n_over_alpha = 0 }
  let radius2 t = t.radius2
  let tau t = t.tau
  let n_over_alpha t = t.n_over_alpha
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

  let pixel_stats =
    Array.init (width * height) ~f:(fun _ -> Pixel_stat.create ~radius2:init_radius2)
  ;;

  let pixel_stat x y = pixel_stats.((y * width) + x)

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
      let radius2 = Pixel_stat.radius2 (pixel_stat x y) in
      let radius = Float.sqrt radius2 in
      let center = Hit_point.point t in
      let offset = V3.create ~x:radius ~y:radius ~z:radius in
      let min = P3.translate center V3.Infix.(~-offset)
      and max = P3.translate center offset in
      Bbox.create ~min ~max
    ;;
  end))

  let trace_photon hit_points light ray sample =
    let sample_idx = ref 1 in
    let u () =
      Int.incr sample_idx;
      L.Sample.get sample !sample_idx
    in
    let rec loop ray flux fuel =
      if fuel > 0
      then (
        let fuel = fuel - 1 in
        match intersect ray with
        | None -> ()
        | Some h ->
          let u = u ()
          and v = u () in
          (match Hit.scatter h u with
          | Absorb -> ()
          | Specular (ray, color) -> loop ray Color.Infix.(flux * color) fuel
          | Diffuse color ->
            let ss = Hit.shader_space h in
            let flux = Color.Infix.(flux * color) in
            Hit_tree.iter_neighbors
              hit_points
              (Shader_space.world_origin ss)
              ~f:(fun hps ->
                Array.iter hps ~f:(fun hp ->
                    let hp_ss = Hit_point.shader_space hp in
                    let w =
                      V3.of_points
                        ~tgt:(Shader_space.world_origin hp_ss)
                        ~src:(Shader_space.world_origin ss)
                    in
                    let pixel_stat =
                      let x, y = Hit_point.pixel hp in
                      pixel_stat x y
                    in
                    let radius2 = Pixel_stat.radius2 pixel_stat in
                    let dp =
                      V3.dot
                        (Shader_space.world_normal hp_ss)
                        (Shader_space.world_normal ss)
                    in
                    let open Float.O in
                    if dp >= 1e-5 && V3.quadrance w <= radius2
                    then (
                      let n = Float.of_int (Pixel_stat.n_over_alpha pixel_stat) * alpha in
                      let g = (n + alpha) / (n + 1.0) in
                      pixel_stat.Pixel_stat.tau
                        <- Color.scale
                             Color.Infix.(
                               pixel_stat.Pixel_stat.tau
                               + Color.scale flux (1.0 / Float.pi))
                             g;
                      pixel_stat.Pixel_stat.radius2 <- g * radius2;
                      pixel_stat.Pixel_stat.n_over_alpha
                        <- Int.O.(pixel_stat.Pixel_stat.n_over_alpha + 1)));
                let dir = Shader_space.unit_square_to_hemisphere u v in
                let ray = Shader_space.world_ray ss dir in
                loop ray flux fuel)))
    in
    loop ray (Point_light.color light) max_bounces
  ;;

  let gather_hit_points pool sampler tiles =
    let cumulative_area = ref 0 in
    let tasks =
      List.map tiles ~f:(fun tile ->
          cumulative_area := !cumulative_area + Tile.area tile;
          let prefix, _suffix = L.split_at sampler !cumulative_area in
          Task.async pool (fun () -> collect_hit_points tile prefix))
    in
    let pts = List.concat_map tasks ~f:(Task.await pool) in
    Hit_tree.create pts
  ;;

  let trace_photons pool sampler hit_points =
    let open L.Sample in
    List.iter point_lights ~f:(fun light ->
        Task.parallel_for pool ~start:1 ~finish:num_photons ~body:(fun i ->
            let pre, _ = L.split_at sampler i in
            let s = snd @@ L.step pre in
            let ray = Point_light.random_ray light s.%{0} s.%{1} in
            trace_photon hit_points light ray s))
  ;;

  let create_image pool =
    let img_data = Array.create ~len:(3 * Array.length pixel_stats) 0.0 in
    Task.parallel_for
      pool
      ~start:0
      ~finish:(Array.length pixel_stats - 1)
      ~body:(fun pix ->
        let ps = pixel_stats.(pix) in
        let r, g, b =
          Color.to_rgb
          @@ Color.scale
               (Pixel_stat.tau ps)
               (1.0 /. (Float.of_int num_photons *. Pixel_stat.radius2 ps))
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
    printf "#tiles = %d\n%!" (List.length tiles);
    let pool = Task.setup_pool ~num_additional_domains in
    let sampler = ref @@ L.create ~dimension:(2 + (2 * (max_bounces + 1))) in
    for i = 1 to num_iterations do
      printf "#iteration = %d\n%!" i;
      let h_sampler, suffix = L.split_at !sampler (width * height) in
      let p_sampler, suffix = L.split_at suffix num_photons in
      sampler := suffix;
      let hit_points = gather_hit_points pool h_sampler tiles in
      trace_photons pool p_sampler hit_points
    done;
    let img = create_image pool in
    Task.teardown_pool pool;
    let n_sum = Array.sum (module Int) pixel_stats ~f:Pixel_stat.n_over_alpha in
    printf "sum(n/alpha) = %d\n" n_sum;
    img
  ;;
end
