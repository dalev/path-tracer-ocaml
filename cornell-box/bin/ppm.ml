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

  module Kd_tree = Kd_tree.Make (struct
    include Hit_point

    let x = Fn.compose P3.x point
    let y = Fn.compose P3.y point
    let z = Fn.compose P3.z point

    let bbox t =
      let x, y = t.pixel in
      let radius2 = Pixel_stat.radius2 pixel_stats.((y * width) + x) in
      let radius = Float.sqrt radius2 in
      let center = point t in
      let offset = V3.create ~x:radius ~y:radius ~z:radius in
      let max = P3.translate center offset
      and min = P3.translate center V3.Infix.(~-offset) in
      Bbox.create ~min ~max
    ;;
  end)

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
            Kd_tree.search hit_points ~center:(Shader_space.world_origin ss) ~f:(fun hp ->
                let hp_ss = Hit_point.shader_space hp in
                let w =
                  V3.of_points
                    ~tgt:(Shader_space.world_origin hp_ss)
                    ~src:(Shader_space.world_origin ss)
                in
                let pixel_stat =
                  let x, y = Hit_point.pixel hp in
                  let i = (y * width) + x in
                  if i >= Array.length pixel_stats || i < 0
                  then
                    raise_s
                      [%message
                        "pixel_stats index out of bounds"
                          ~idx:(i : int)
                          ~length:(Array.length pixel_stats : int)]
                  else pixel_stats.(i)
                in
                let radius2 = Pixel_stat.radius2 pixel_stat in
                let dp =
                  V3.dot (Shader_space.world_normal hp_ss) (Shader_space.world_normal ss)
                in
                let open Float.O in
                if dp >= 1e-3 && V3.quadrance w <= radius2
                then (
                  let n = Float.of_int (Pixel_stat.n_over_alpha pixel_stat) * alpha in
                  let g = (n + alpha) / (n + 1.0) in
                  pixel_stat.Pixel_stat.tau
                    <- Color.scale
                         Color.Infix.(
                           pixel_stat.Pixel_stat.tau + Color.scale flux (1.0 / Float.pi))
                         g;
                  pixel_stat.Pixel_stat.radius2 <- g * radius2;
                  pixel_stat.Pixel_stat.n_over_alpha
                    <- Int.O.(pixel_stat.Pixel_stat.n_over_alpha + 1)));
            let dir = Shader_space.unit_square_to_hemisphere u v in
            let ray = Shader_space.world_ray ss dir in
            loop ray flux fuel))
    in
    loop ray (Point_light.color light) max_bounces
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
    let hit_pts =
      let pts = List.map tasks ~f:(Task.await pool) in
      let len = List.sum (module Int) pts ~f:List.length in
      let a = Array.create ~len (List.hd_exn (List.hd_exn pts)) in
      let i = ref 0 in
      List.iter pts ~f:(fun pts ->
          List.iter pts ~f:(fun p ->
              a.(!i) <- p;
              Int.incr i));
      Kd_tree.create a
    in
    let sampler = ref @@ L.create ~dimension:(2 + (2 * (max_bounces + 1))) in
    let open L.Sample in
    List.iter point_lights ~f:(fun light ->
        Task.parallel_for pool ~start:1 ~finish:num_photons ~body:(fun _ ->
            let sampler', s = L.step !sampler in
            sampler := sampler';
            let ray = Point_light.random_ray light s.%{0} s.%{1} in
            trace_photon hit_pts light ray s));
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
    Task.teardown_pool pool;
    Bimage.Image.of_data
      Bimage.Color.rgb
      width
      height
      (Bimage.Data.of_array Bimage.Type.f64 img_data)
  ;;
end
