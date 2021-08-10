open! Base
module L = Low_discrepancy_sequence

module Tile = struct
  type t =
    { row : int
    ; col : int
    ; width : int
    ; height : int
    }

  let area t = t.width * t.height

  let split_width t =
    let half_w = t.width / 2 in
    let lhs = { t with width = half_w } in
    let rhs = { t with col = t.col + half_w; width = t.width - half_w } in
    lhs, rhs
  ;;

  let split_height t =
    let half_h = t.height / 2 in
    let lhs = { t with height = half_h } in
    let rhs = { t with row = t.row + half_h; height = t.height - half_h } in
    lhs, rhs
  ;;

  let split_once t = if t.width > t.height then split_width t else split_height t

  let split t ~max_area =
    let rec loop t =
      if area t <= max_area
      then [ t ]
      else (
        let lhs, rhs = split_once t in
        loop lhs @ loop rhs)
    in
    loop t
  ;;

  let create ~width ~height = { row = 0; col = 0; width; height }
end

type t =
  { width : int
  ; height : int
  ; write_pixel : x:int -> y:int -> Color.t -> unit
  ; samples_per_pixel : int
  ; max_bounces : int
  ; trace_path :
      cx:float -> cy:float -> int -> Low_discrepancy_sequence.Sample.t -> Color.t
  ; tiles : Tile.t list
  }

let count_tiles t = List.length t.tiles

let path_tracer ~intersect ~background ~diffuse_plus_light ~camera =
  Staged.stage
  @@ fun ~cx ~cy max_bounces samples ->
  let ray = Camera.ray camera cx cy in
  let take_2d =
    let open L.Sample in
    let samples_index = ref 2 in
    fun () ->
      let j = !samples_index in
      let u = samples.%{j}
      and v = samples.%{j + 1} in
      samples_index := j + 2;
      u, v
  in
  let add_mul a b c = Color.Infix.(a + (b * c)) in
  let rec loop ray max_bounces emit0 attn0 =
    if max_bounces <= 0
    then add_mul emit0 attn0 Color.black
    else (
      let max_bounces = max_bounces - 1 in
      match intersect ray with
      | None -> add_mul emit0 attn0 (background ray)
      | Some h ->
        let emit = Hit.emit h in
        let u, v = take_2d () in
        (match (Hit.scatter h u : Scatter.t) with
        | Absorb -> add_mul emit0 attn0 emit
        | Specular (scattered_ray, attenuation) ->
          loop
            scattered_ray
            max_bounces
            (add_mul emit attenuation emit0)
            Color.Infix.(attenuation * attn0)
        | Diffuse attenuation ->
          let ss = Hit.shader_space h in
          let dir = Pdf.sample diffuse_plus_light ss u v in
          let diffuse_pd = Pdf.eval Pdf.diffuse dir ss in
          if Float.( = ) diffuse_pd 0.0
          then add_mul emit0 attn0 emit
          else (
            let divisor = Pdf.eval diffuse_plus_light dir ss in
            let pd = diffuse_pd /. divisor in
            if not (Float.is_finite pd)
            then add_mul emit0 attn0 emit
            else (
              let scattered_ray = Shader_space.world_ray ss dir in
              let attenuation = Color.scale attenuation pd in
              loop
                scattered_ray
                max_bounces
                (add_mul emit attenuation emit0)
                Color.Infix.(attenuation * attn0)))))
  in
  loop ray max_bounces Color.black Color.white
;;

let create
    ~width
    ~height
    ~write_pixel
    ~samples_per_pixel
    ~max_bounces
    ~camera
    ~intersect
    ~background
    ~diffuse_plus_light
  =
  let trace_path =
    Staged.unstage @@ path_tracer ~intersect ~background ~diffuse_plus_light ~camera
  in
  let max_area = 32 * 32 in
  let tiles = Tile.split ~max_area (Tile.create ~width ~height) in
  { width; height; write_pixel; samples_per_pixel; max_bounces; trace_path; tiles }
;;

let gamma = Color.map ~f:Float.sqrt

let render_tile t tile tile_sampler =
  let x0 = tile.Tile.col in
  let y0 = tile.Tile.row in
  let widthf = 1 // t.width in
  let heightf = 1 // t.height in
  let spp_invf = 1 // t.samples_per_pixel in
  let sampler = ref tile_sampler in
  for y = y0 to y0 + tile.Tile.height - 1 do
    let yf = Float.of_int (t.height - 1 - y) in
    for x = x0 to x0 + tile.Tile.width - 1 do
      let xf = Float.of_int x in
      let color = ref Color.black in
      for _ = 1 to t.samples_per_pixel do
        let sampler', s = L.step !sampler in
        sampler := sampler';
        let open L.Sample in
        let dx = s.%{0}
        and dy = s.%{1} in
        let cx = (xf +. dx) *. widthf in
        let cy = (yf +. dy) *. heightf in
        color := Color.Infix.( + ) !color @@ t.trace_path ~cx ~cy t.max_bounces s
      done;
      t.write_pixel ~x ~y @@ gamma (Color.scale !color spp_invf)
    done
  done
;;

let create_tile_samplers t tiles =
  let s = ref (L.create ~dimension:(2 + (2 * (t.max_bounces + 1)))) in
  List.map tiles ~f:(fun tile ->
      let n = t.samples_per_pixel * Tile.area tile in
      let tile_sampler, suffix = L.split_at !s n in
      s := suffix;
      tile, tile_sampler)
;;

let render ~update_progress t =
  let module Task = Domainslib.Task in
  let pool = Task.setup_pool ~num_additional_domains:7 in
  let tasks =
    List.map (create_tile_samplers t t.tiles) ~f:(fun (tile, sampler) ->
        Task.async pool (fun () ->
            render_tile t tile sampler;
            update_progress ()))
  in
  List.iter tasks ~f:(fun t -> Task.await pool t);
  Task.teardown_pool pool
;;
