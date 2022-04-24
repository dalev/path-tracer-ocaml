open! Base
module L = Low_discrepancy_sequence

type t =
  { width : int
  ; height : int
  ; write_pixel : x:int -> y:int -> Color.t -> unit
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
    ~write_pixel
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
  { width; height; write_pixel; samples_per_pixel; max_bounces; trace_path }
;;

let create_sampler t = L.create ~dimension:(2 + (2 * t.max_bounces))

let render ~update_progress t =
  let widthf = 1 // t.width in
  let heightf = 1 // t.height in
  let lds = create_sampler t in
  for pass = 0 to t.samples_per_pixel - 1 do
    let offset' = ref pass in
    for y = 0 to t.height - 1 do
      let yf = Float.of_int (t.height - 1 - y) in
      for x = 0 to t.width - 1 do
        let xf = Float.of_int x in
        let offset = !offset' in
        offset' := offset + t.samples_per_pixel;
        let sample ~dimension = L.get lds ~offset ~dimension in
        let dx = sample ~dimension:0
        and dy = sample ~dimension:1 in
        let cx = (xf +. dx) *. widthf in
        let cy = (yf +. dy) *. heightf in
        let color = t.trace_path ~cx ~cy ~sample in
        t.write_pixel ~x ~y color
      done;
      update_progress t.width
    done
  done
;;
