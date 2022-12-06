open! Base
module L = Low_discrepancy_sequence
module Image = Bimage.Image

type image = (float, Bigarray.float64_elt, [ `Rgb ]) Image.t

type t =
  { width : int
  ; height : int
  ; image : image
  ; samples_per_pixel : int
  ; max_bounces : int
  ; trace_path : cx:float -> cy:float -> sample:(dimension:int -> float) -> Color.t
  }

let path_tracer ~intersect ~background ~diffuse_plus_light ~camera ~max_bounces =
  Staged.stage
  @@ fun ~cx ~cy ~sample ->
  let ray = Camera.ray camera cx cy in
  let take_2d =
    let samples_index = ref 2 in
    fun () ->
      let j = !samples_index in
      let u = sample ~dimension:j
      and v = sample ~dimension:(j + 1) in
      samples_index := j + 2;
      u, v
  in
  let add_mul a b c = Color.fma b c a in
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
  ~image
  ~samples_per_pixel
  ~max_bounces
  ~camera
  ~intersect
  ~background
  ~diffuse_plus_light
  =
  let trace_path =
    Staged.unstage
    @@ path_tracer ~intersect ~background ~diffuse_plus_light ~camera ~max_bounces
  in
  { width; height; image; samples_per_pixel; max_bounces; trace_path }
;;

let create_sampler t = L.create ~dimension:(2 + (2 * t.max_bounces))

let render_tile t tile filter_kernel =
  let widthf = 1 // t.width in
  let heightf = 1 // t.height in
  let ft = Film_tile.create tile ~filter_kernel in
  let lds = create_sampler t in
  for pass = 0 to t.samples_per_pixel - 1 do
    Tile.iter tile ~f:(fun ~local_x ~local_y ~global_x ~global_y ->
      let offset = (global_y * t.width) + global_x + (pass * t.samples_per_pixel) in
      let sample ~dimension = L.get lds ~offset ~dimension in
      let xf = Float.of_int global_x
      and yf = Float.of_int global_y
      and dx = sample ~dimension:0
      and dy = sample ~dimension:1 in
      let cx = (xf +. dx) *. widthf
      and cy = 1.0 -. ((yf +. dy) *. heightf) in
      let color = t.trace_path ~cx ~cy ~sample in
      Film_tile.write_pixel ft ~x:local_x ~y:local_y color)
  done;
  ft
;;

let stitch_tile img film_tile =
  let in_bounds x y = 0 <= x && x < img.Image.width && 0 <= y && y < img.Image.height in
  Film_tile.iter film_tile ~f:(fun ~global_x ~global_y color ->
    if in_bounds global_x global_y
    then begin
      let[@inline] incr ch v =
        let a = Image.get img global_x global_y ch in
        Image.set img global_x global_y ch (v +. a)
      in
      let r, g, b = Color.to_rgb color in
      incr 0 r;
      incr 1 g;
      incr 2 b
    end)
;;

let render ~update_progress t =
  let open Domainslib in
  let max_area = Int.pow 32 2 in
  let tiles = Tile.create ~width:t.width ~height:t.height |> Tile.split ~max_area in
  let pixel_radius = 1 in
  let filter_kernel = Filter_kernel.Binomial.create ~order:5 ~pixel_radius in
  let c = Chan.make_unbounded () in
  let pool =
    let num_domains = Caml.Domain.recommended_domain_count () - 1 in
    Task.setup_pool ~num_domains ()
  in
  Task.run pool (fun () ->
    List.iter tiles ~f:(fun tile ->
      ignore
      @@ Task.async pool (fun () ->
           let ft = render_tile t tile filter_kernel in
           Chan.send c ft));
    for _i = 0 to List.length tiles - 1 do
      let ft = Chan.recv c in
      stitch_tile t.image ft;
      update_progress @@ Tile.area (Film_tile.tile ft)
    done);
  Task.teardown_pool pool
;;
